(* A functional client interface to ldap

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

module Make (M : Ldap_types.Monad) = struct

let (>>=) = M.bind

module Lber = Lber.Make(M)
module Ldap_protocol = Ldap_protocol.Make(M)

type msgid = Int32.t

type ld_socket = Ssl of M.Ssl.socket
                 | Plain of M.Unix.file_descr

type conn = {
  mutable rb: Lber.readbyte;
  mutable socket: ld_socket; (* communications channel to the ldap server *)
  mutable current_msgid: Int32.t; (* the largest message id allocated so far *)
  pending_messages: (int32, Ldap_types.ldap_message Queue.t) Hashtbl.t;
  protocol_version: int;
}

type attr = { attr_name: string; attr_values: string list }
type modattr = Ldap_types.modify_optype * string * string list
type result = Ldap_types.search_result_entry list
type entry = Ldap_types.search_result_entry
type authmethod = [ `SIMPLE | `SASL ]
type search_result = [ `Entry of entry
                     | `Referral of (string list)
                     | `Success of (Ldap_types.ldap_controls option) ]
type page_control =
  [ `Noctrl
  | `Initctrl of int
  | `Subctrl of (int * string) ]

let ext_res = {Ldap_types.ext_matched_dn="";
               ext_referral=None}

let _ = Ssl.init ()

(* limits us to Int32.max_int active async operations
   at any one time *)
let find_free_msgid con =
  let msgid = con.current_msgid in
    (if msgid = Int32.max_int then
       con.current_msgid <- 0l
     else
       con.current_msgid <- Int32.succ con.current_msgid);
    msgid

(* allocate a message id from the free message id pool *)
let allocate_messageid con =
  let msgid = find_free_msgid con in
    Hashtbl.replace con.pending_messages msgid (Queue.create ());
    msgid

let free_messageid con msgid =
  try Hashtbl.remove con.pending_messages msgid
  with Not_found ->
    raise (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "free_messageid: invalid msgid", ext_res))

(* send an ldapmessage *)
let send_message con msg =
  let write ld_socket buf off len =
    match ld_socket with
      | Ssl s ->
          M.catch
            (fun () -> M.Ssl.write s buf off len)
            (function
              | Ssl.Write_error _ ->
                  M.fail (Unix.Unix_error (Unix.EPIPE, "Ssl.write", ""))
              | exn -> M.fail exn
            )
      | Plain s -> M.Unix.write s buf off len
  in
  let e_msg = Ldap_protocol.encode_ldapmessage msg in
  let len = String.length e_msg in
  let rec result written =
    M.catch
      (fun () ->
        if written < len then (
          write con.socket e_msg written (len - written)
          >>= fun old ->
          result (old + written)
        )
        else
          M.return ()
      )
    (function
      | Unix.Unix_error (Unix.EBADF, _, _)
      | Unix.Unix_error (Unix.EPIPE, _, _)
      | Unix.Unix_error (Unix.ECONNRESET, _, _)
      | Unix.Unix_error (Unix.ECONNABORTED, _, _)
      | _ ->
          M.fail
            (Ldap_types.LDAP_Failure
               (`SERVER_DOWN,
                "the connection object is invalid, data cannot be written",
                ext_res
               )
            )
    )
  in
  result 0

(* recieve an ldapmessage for a particular message id (messages for
   all other ids will be read and queued. They can be retreived later) *)
let receive_message con msgid =
  let q_for_msgid con msgid =
    try Hashtbl.find con.pending_messages msgid
    with Not_found -> raise (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "invalid message id", ext_res))
  in
  let rec read_message con msgid =
    Ldap_protocol.decode_ldapmessage con.rb >>= fun msg ->
      if msg.Ldap_types.messageID = msgid
      then M.return msg
      else
        (let q = q_for_msgid con msg.Ldap_types.messageID in
           Queue.add msg q;
           read_message con msgid)
  in
  let q = q_for_msgid con msgid in
    try
      if Queue.is_empty q then
        read_message con msgid
      else M.return (Queue.take q)
    with
        Lber.Readbyte_error Lber.Transport_error ->
          raise (Ldap_types.LDAP_Failure (`SERVER_DOWN, "read error", ext_res))
      | Lber.Readbyte_error Lber.End_of_stream ->
          raise (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "bug in ldap decoder detected", ext_res))

let init ?(connect_timeout = 1) ?(version = 3) hosts =
  if ((version < 2) || (version > 3)) then
    raise (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "invalid protocol version", ext_res))
  else
    let fd =
      let addrs =
        (List.flatten
           (List.map
              (fun (mech, host, port) ->
                 try
                   (List.rev_map
                      (fun addr -> (mech, addr, port))
                      (Array.to_list ((Unix.gethostbyname host).Unix.h_addr_list)))
                 with Not_found -> [])
              (List.map
                 (fun host ->
                    (match Ldap_url.of_string host with
                         {Ldap_types.url_mech=mech;url_host=(Some host);url_port=(Some port)} ->
                           (mech, host, int_of_string port)
                       | {Ldap_types.url_mech=mech;url_host=(Some host);url_port=None} ->
                           (mech, host, 389)
                       | _ -> raise
                           (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "invalid ldap url", ext_res))))
                 hosts)))
      in
      let rec open_con addrs =
        let previous_signal = ref Sys.Signal_default in
          match addrs with
              (mech, addr, port) :: tl ->
                (try
                   if mech = `PLAIN then (
                     let s = M.Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
                     M.catch
                       (fun () ->
                         previous_signal :=
                           Sys.signal Sys.sigalrm
                             (Sys.Signal_handle (fun _ -> failwith "timeout"));
                         ignore (Unix.alarm connect_timeout);
                         M.Unix.connect s (Unix.ADDR_INET (addr, port))
                         >>= fun () ->
                         ignore (Unix.alarm 0);
                         Sys.set_signal Sys.sigalrm !previous_signal;
                         M.return (Plain s)
                       )
                       (fun exn -> M.Unix.close s >>= fun () -> M.fail exn)
                   )
                   else
                     (previous_signal :=
                        Sys.signal Sys.sigalrm
                          (Sys.Signal_handle (fun _ -> failwith "timeout"));
                      ignore (Unix.alarm connect_timeout);
                      let ssl =
                        M.Ssl.open_connection
                        Ssl.SSLv23
                        (Unix.ADDR_INET (addr, port))
                        >>= (fun x -> M.return (Ssl x))
                      in
                      ignore (Unix.alarm 0);
                      Sys.set_signal Sys.sigalrm !previous_signal;
                      ssl)
                 with
                     Unix.Unix_error (Unix.ECONNREFUSED, _, _)
                   | Unix.Unix_error (Unix.EHOSTDOWN, _, _)
                   | Unix.Unix_error (Unix.EHOSTUNREACH, _, _)
                   | Unix.Unix_error (Unix.ECONNRESET, _, _)
                   | Unix.Unix_error (Unix.ECONNABORTED, _, _)
                   | Ssl.Connection_error _
                   | Failure "timeout" ->
                       ignore (Unix.alarm 0);
                       Sys.set_signal Sys.sigalrm !previous_signal;
                       open_con tl)
            | [] -> raise (Ldap_types.LDAP_Failure (`SERVER_DOWN, "", ext_res))
      in
        open_con addrs
    in
    fd >>= fun fd ->
    let rb = match fd with
      | Ssl s -> Lber.readbyte_of_ssl s
      | Plain s -> Lber.readbyte_of_fd s
    in
    M.return
      {rb;
       socket=fd;
       current_msgid=1l;
       pending_messages=(Hashtbl.create 3);
       protocol_version=version;
      }

(* sync auth_method types between the two files *)
let bind_s ?(who = "") ?(cred = "") ?(auth_method = `SIMPLE) con =
  let msgid = allocate_messageid con in
  (try (
    send_message con
      {Ldap_types.messageID=msgid;
       Ldap_types.protocolOp=Ldap_types.Bind_request
          {Ldap_types.bind_version=con.protocol_version;
           bind_name=who;
           bind_authentication=(Ldap_types.Simple cred)};
       controls=None}
    >>= fun () ->
    receive_message con msgid >>= function
      | {Ldap_types.protocolOp =
          Ldap_types.Bind_response
            {Ldap_types.bind_result = {Ldap_types.result_code = `SUCCESS}}
        } -> M.return ()
      | {Ldap_types.protocolOp =
          Ldap_types.Bind_response {Ldap_types.bind_result = res}
        } ->
          raise (Ldap_types.LDAP_Failure
                   (res.Ldap_types.result_code, res.Ldap_types.error_message,
                     {Ldap_types.ext_matched_dn=res.Ldap_types.matched_dn;
                      ext_referral=res.Ldap_types.ldap_referral}))
      | _ -> raise (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "invalid server response", ext_res))
   )
   with exn -> free_messageid con msgid; raise exn
  )
  >>= fun () ->
  M.return (free_messageid con msgid)

let search ?(base = "") ?(scope = `SUBTREE) ?(aliasderef=`NEVERDEREFALIASES)
  ?(sizelimit=0l) ?(timelimit=0l) ?(attrs = []) ?(attrsonly = false)
  ?(page_control = `Noctrl) con filter =
  let msgid = allocate_messageid con in
  let build_res_ctrl size cookie =
    {Ldap_types.criticality = false;
    Ldap_types.control_details=(`Paged_results_control {Ldap_types.size; Ldap_types.cookie})}
  in
  let controls = match (page_control) with
    | `Noctrl -> None
    | `Initctrl size | `Subctrl (size,_) when size < 1 ->
      raise (Ldap_types.LDAP_Failure(`LOCAL_ERROR, "invalid page size", ext_res))
    | `Initctrl size -> Some [(build_res_ctrl size "")]
    | `Subctrl (size,cookie) -> Some [(build_res_ctrl size cookie)]
   in
  try begin
    let e_filter = (try Ldap_filter.of_string filter
      with _ ->
        (raise
           (Ldap_types.LDAP_Failure
              (`LOCAL_ERROR, "bad search filter", ext_res))))
    in
    send_message con
      {Ldap_types.messageID=msgid;
       Ldap_types.protocolOp=Ldap_types.Search_request
          {Ldap_types.baseObject=base;
           scope=scope;
           derefAliases=aliasderef;
           sizeLimit=sizelimit;
           timeLimit=timelimit;
           typesOnly=attrsonly;
           filter=e_filter;
           s_attributes=attrs};
       controls}
    >>= fun () ->
    M.return msgid
  end
  with exn -> free_messageid con msgid;raise exn

let get_search_entry con msgid =
  try
    receive_message con msgid >>= function
      | {Ldap_types.protocolOp=Ldap_types.Search_result_entry e} -> M.return (`Entry e)
      | {Ldap_types.protocolOp=Ldap_types.Search_result_reference r} -> M.return (`Referral r)
      | {Ldap_types.protocolOp=Ldap_types.Search_result_done {Ldap_types.result_code=`SUCCESS}} ->
          raise (Ldap_types.LDAP_Failure (`SUCCESS, "success", ext_res))
      | {Ldap_types.protocolOp=Ldap_types.Search_result_done res} ->
        raise (Ldap_types.LDAP_Failure (res.Ldap_types.result_code, res.Ldap_types.error_message,
                             {Ldap_types.ext_matched_dn=res.Ldap_types.matched_dn;
                              ext_referral=res.Ldap_types.ldap_referral}))
      | _ -> raise (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "unexpected search response", ext_res))
  with exn -> free_messageid con msgid;raise exn

let get_search_entry_with_controls con msgid =
  try
    receive_message con msgid >>= function
      | {Ldap_types.protocolOp=Ldap_types.Search_result_entry e} -> M.return (`Entry e)
      | {Ldap_types.protocolOp=Ldap_types.Search_result_reference r} -> M.return (`Referral r)
      | {Ldap_types.protocolOp=Ldap_types.Search_result_done {Ldap_types.result_code=`SUCCESS};Ldap_types.controls=cntrls} ->
        M.return (`Success cntrls)
      | {Ldap_types.protocolOp=Ldap_types.Search_result_done res} ->
        raise (Ldap_types.LDAP_Failure (res.Ldap_types.result_code, res.Ldap_types.error_message,
                             {Ldap_types.ext_matched_dn=res.Ldap_types.matched_dn;
                              ext_referral=res.Ldap_types.ldap_referral}))
      | _ -> raise (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "unexpected search response", ext_res))
  with exn -> free_messageid con msgid;raise exn

let abandon con msgid =
  let my_msgid = allocate_messageid con in
    try
      free_messageid con msgid;
      send_message con
        {Ldap_types.messageID=my_msgid;
         Ldap_types.protocolOp=(Ldap_types.Abandon_request msgid);
         controls=None}
    with exn -> free_messageid con my_msgid;raise exn

let search_s ?(base = "") ?(scope = `SUBTREE) ?(aliasderef=`NEVERDEREFALIASES)
  ?(sizelimit=0l) ?(timelimit=0l) ?(attrs = []) ?(attrsonly = false) con filter =
  let msgid = search ~base:base ~scope:scope ~aliasderef:aliasderef ~sizelimit:sizelimit
                ~timelimit:timelimit ~attrs:attrs ~attrsonly:attrsonly con filter
  in
  msgid >>= fun msgid ->
  let rec f result =
    M.catch
      (fun () ->
        get_search_entry con msgid >>= fun x ->
        f (x :: result)
      )
      (function
        | Ldap_types.LDAP_Failure (`SUCCESS, _, _) ->
            M.return result
        | Ldap_types.LDAP_Failure (code, msg, ext) ->
            M.fail (Ldap_types.LDAP_Failure (code, msg, ext))
        | exn ->
            M.catch
              (fun () -> abandon con msgid >>= fun _ -> M.return ())
              (fun _ -> M.return ())
            >>= fun () ->
            M.fail exn
      )
  in
  f [] >>= fun result ->
  free_messageid con msgid;
  M.return result

let add_s con (entry: entry) =
  let msgid = allocate_messageid con in
  M.catch
    (fun () ->
      send_message con
        {Ldap_types.messageID=msgid;
         Ldap_types.protocolOp=Ldap_types.Add_request entry;
         controls=None}
      >>= fun () ->
      receive_message con msgid >>= function
        | {Ldap_types.protocolOp=Ldap_types.Add_response {Ldap_types.result_code=`SUCCESS}} ->
            M.return ()
        | {Ldap_types.protocolOp=Ldap_types.Add_response res} ->
            M.fail (Ldap_types.LDAP_Failure (res.Ldap_types.result_code, res.Ldap_types.error_message,
                                            {Ldap_types.ext_matched_dn=res.Ldap_types.matched_dn;
                                             ext_referral=res.Ldap_types.ldap_referral}))
        | _ -> M.fail (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "invalid add response", ext_res))
    )
    (fun exn -> free_messageid con msgid;raise exn)
  >>= fun () ->
  free_messageid con msgid;
  M.return ()

let delete_s con ~dn =
  let msgid = allocate_messageid con in
  M.catch
    (fun () ->
      send_message con
        {Ldap_types.messageID=msgid;
         Ldap_types.protocolOp=Ldap_types.Delete_request dn;
         controls=None}
      >>= fun () ->
      receive_message con msgid >>= function
        | {Ldap_types.protocolOp=Ldap_types.Delete_response {Ldap_types.result_code=`SUCCESS}} ->
            M.return ()
        | {Ldap_types.protocolOp=Ldap_types.Delete_response res} ->
            M.fail (Ldap_types.LDAP_Failure (res.Ldap_types.result_code, res.Ldap_types.error_message,
                                  {Ldap_types.ext_matched_dn=res.Ldap_types.matched_dn;
                                   ext_referral=res.Ldap_types.ldap_referral}))
        | _ -> M.fail (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "invalid delete response", ext_res))
    )
    (fun exn -> free_messageid con msgid;raise exn)
  >>= fun () ->
  free_messageid con msgid;
  M.return ()

let unbind con =
  M.catch
    (fun () ->
      match con.socket with
        | Ssl s -> M.Ssl.shutdown s
        | Plain s -> M.Unix.close s
    )
    (fun _ -> M.return ())

let modify_s con ~dn ~mods =
  let rec convertmods ?(converted=[]) mods =
    match mods with
        (op, attr, values) :: tl ->
          (convertmods
             ~converted:({Ldap_types.mod_op=op;
                          mod_value={Ldap_types.attr_type=attr;
                                     attr_vals=values}} :: converted)
             tl)
      | [] -> converted
  in
  let msgid = allocate_messageid con in
  M.catch
    (fun () ->
      send_message con
        {Ldap_types.messageID=msgid;
         Ldap_types.protocolOp=Ldap_types.Modify_request
            {Ldap_types.mod_dn=dn;
             modification=convertmods mods};
         controls=None}
      >>= fun () ->
      receive_message con msgid >>= function
        | {Ldap_types.protocolOp=Ldap_types.Modify_response {Ldap_types.result_code=`SUCCESS}} ->
            M.return ()
        | {Ldap_types.protocolOp=Ldap_types.Modify_response res} ->
            M.fail (Ldap_types.LDAP_Failure (res.Ldap_types.result_code, res.Ldap_types.error_message,
                                  {Ldap_types.ext_matched_dn=res.Ldap_types.matched_dn;
                                   ext_referral=res.Ldap_types.ldap_referral}))
         | _ -> M.fail (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "invalid modify response", ext_res))
    )
    (fun exn -> free_messageid con msgid;raise exn)
  >>= fun () ->
  free_messageid con msgid;
  M.return ()

let modrdn_s ?(deleteoldrdn=true) ?(newsup=None) con ~dn ~newdn =
  let msgid = allocate_messageid con in
  M.catch
    (fun () ->
      send_message con
        {Ldap_types.messageID=msgid;
         Ldap_types.protocolOp=Ldap_types.Modify_dn_request
            {Ldap_types.modn_dn=dn;
             modn_newrdn=newdn;
             modn_deleteoldrdn=deleteoldrdn;
             modn_newSuperior=None};
         controls=None}
      >>= fun () ->
      receive_message con msgid >>= function
        | {Ldap_types.protocolOp=Ldap_types.Modify_dn_response {Ldap_types.result_code=`SUCCESS}} ->
            M.return ()
        | {Ldap_types.protocolOp=Ldap_types.Modify_dn_response res} ->
            M.fail (Ldap_types.LDAP_Failure (res.Ldap_types.result_code, res.Ldap_types.error_message,
                                  {Ldap_types.ext_matched_dn=res.Ldap_types.matched_dn;
                                   ext_referral=res.Ldap_types.ldap_referral}))
         | _ -> M.fail (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "invalid modify dn response", ext_res))
    )
    (fun exn -> free_messageid con msgid;raise exn)
  >>= fun () ->
  free_messageid con msgid;
  M.return ()

let create_grouping_s groupingType value = ()
let end_grouping_s cookie value = ()

end
