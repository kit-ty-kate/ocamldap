(*pp camlp4o pa_macro.cmo *)

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

open Ldap_types
open Ldap_protocol
open Lber
open Unix
open Sys

type msgqueue = {
  msg_queue: ldap_message Queue.t;
  mutable msg_inuse: bool;
}

type msgid = int

IFDEF SSL THEN
type ld_socket = Ssl of Ssl.socket
		 | Plain of file_descr
ELSE
type ld_socket = Plain of file_descr
END;;

type conn = {
  mutable rb: readbyte;
  mutable socket: ld_socket; (* communications channel to the ldap server *)
  mutable current_msgid: int; (* the largest message id allocated so far *)
  pending_messages: (int, msgqueue) Hashtbl.t;
  protocol_version: msgid;
}

type attr = { attr_name: string; attr_values: string list }
type modattr = modify_optype * string * string list
type result = search_result_entry list
type entry = search_result_entry
type authmethod = [ `SIMPLE | `SASL ]
type search_result = [ `Entry of entry
		     | `Referral of (string list) ]

let ext_res = {ext_matched_dn="";
	       ext_referral=None}

IFDEF SSL THEN
let _ = Ssl.init ()
END;;

exception Free of int
let find_free_msgid con = 
  try 
    Hashtbl.iter 
      (fun k v -> if not v.msg_inuse then raise (Free k))
      con.pending_messages;
    con.current_msgid
  with Free k -> k

(* test async operations, make sure we can't screw
   something up with an async op *)

(* allocate a message id from the free message id pool *)
let allocate_messageid con =
  let msgid = find_free_msgid con in
    (if msgid = con.current_msgid then
       con.current_msgid <- con.current_msgid + 1);
    Hashtbl.replace con.pending_messages msgid 
      {msg_queue=(Queue.create ());
       msg_inuse=true};
    msgid

let free_messageid con msgid =
  let msgid_s = Hashtbl.find con.pending_messages msgid in
    msgid_s.msg_inuse <- false;
    Queue.clear msgid_s.msg_queue

(* send an ldapmessage *)
let send_message con msg =
  let write ld_socket buf off len = 
    IFDEF SSL THEN
      match ld_socket with
	  Ssl s -> 
	    try Ssl.write s buf off len
	    with Write_error _ -> raise (Unix_error (EPIPE, "Ssl.write", ""))
	| Plain s -> Unix.write s buf off len
    ELSE
      match ld_socket with
	  Plain s -> Unix.write s buf off len
    END
  in
  let e_msg = encode_ldapmessage msg in
  let len = String.length e_msg in
  let written = ref 0 in
    try
      while !written < len
      do
	written := ((write con.socket e_msg 
		       !written (len - !written)) + 
		    !written)
      done
    with 
	Unix_error (EBADF, _, _)
      | Unix_error (EPIPE, _, _)
      | Unix_error (ECONNRESET, _, _)
      | Unix_error (ECONNABORTED, _, _) ->
	  (raise 
	     (LDAP_Failure 
		(`SERVER_DOWN, 
		 "the connection object is invalid, data cannot be written",
		 ext_res)))
      
(* recieve an ldapmessage for a particular message id (messages for
   all other ids will be read and queued. They can be retreived later) *)
let receive_message con msgid =
  let q_for_msgid con msgid =
    try (Hashtbl.find con.pending_messages msgid).msg_queue
    with Not_found -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid message id", ext_res))
  in
  let rec read_message con msgid =
    let msg = decode_ldapmessage con.rb in
      if msg.messageID = msgid then msg
      else
	let q = q_for_msgid con msg.messageID in
	  Queue.add msg q;
	  read_message con msgid
  in
  let q = q_for_msgid con msgid in
    try
      if Queue.is_empty q then
	read_message con msgid
      else Queue.take q
    with Sys_error e -> raise (LDAP_Failure (`SERVER_DOWN, e, ext_res))

(* test all functionality, especially dns awareness 
   - implement connect timeouts. 
   - do we really want to fail
   if any host after the first is unknown? 
   - allow the use of ldapurls in the hosts list*)
let init ?(connect_timeout = 1) ?(version = 3) hosts =
  if ((version < 2) || (version > 3)) then
    raise (LDAP_Failure (`LOCAL_ERROR, "invalid protocol version", ext_res))
  else
    let fd =       
      let addrs =	
	(List.flatten
	   (List.map
	      (fun (mech, host, port) ->
		 try 
		   (List.rev_map
		      (fun addr -> (mech, addr, port))
		      (Array.to_list ((gethostbyname host).h_addr_list)))
		 with Not_found -> [])
	      (List.map
		 (fun host -> 
		    (match Ldap_url.of_string host with
			 {url_mech=mech;url_host=(Some host);url_port=(Some port)} -> 
			   (mech, host, int_of_string port)
		       | {url_mech=mech;url_host=(Some host);url_port=None} -> 
			   (mech, host, 389)
		       | _ -> raise 
			   (LDAP_Failure (`LOCAL_ERROR, "invalid ldap url", ext_res))))
		 hosts)))
      in
      let rec open_con addrs =
	let previous_signal = ref Signal_default in
	  match addrs with
	      (mech, addr, port) :: tl -> 
		(try
		   if mech = `PLAIN then
		     let s = socket PF_INET SOCK_STREAM 0 in
		       try
			 previous_signal :=
			   signal sigalrm 
			     (Signal_handle (fun _ -> failwith "timeout"));
			 ignore (alarm connect_timeout);
			 connect s (ADDR_INET (addr, port));
			 ignore (alarm 0);
			 set_signal sigalrm !previous_signal;
			 Plain s
		       with exn -> close s;raise exn
		   else
	      	     IFDEF SSL THEN 
		       previous_signal := 
		         signal sigalrm 
			   (Signal_handle (fun _ -> failwith "timeout"));
		       ignore (alarm connect_timeout);
		       let ssl = Ssl (Ssl.open_connection (ADDR_INET (addr, port))) in
			 ignore (alarm 0);
			 set_signal sigalrm !previous_signal;
			 ssl
		     ELSE 
		       raise (LDAP_Failure 
				(`LOCAL_ERROR, 
				 "ssl support is not enabled", 
				 ext_res)) 
		     END
		 with
		     Unix_error (ECONNREFUSED, _, _) 
		   | Unix_error (EHOSTDOWN, _, _)
		   | Unix_error (EHOSTUNREACH, _, _) 
		   | Unix_error (ECONNRESET, _, _)
		   | Unix_error (ECONNABORTED, _, _)
		   | IFDEF SSL THEN Ssl.Connection_error _ ELSE Failure "" END
		   | Failure "timeout" ->
		       ignore (alarm 0);
		       set_signal sigalrm !previous_signal;
		       open_con tl)
	    | [] -> raise (LDAP_Failure (`SERVER_DOWN, "", ext_res))
      in
	open_con addrs
    in
    let plain_rb_of_fd fd =
      let buf = String.create 1 in
      let in_ch = in_channel_of_descr fd in
      let peek_buf = String.create 50 in
      let peek_buf_pos = ref 0 in
      let peek_buf_len = ref 0 in
      let rb ?(peek=false) () = 
	if !peek_buf_len = 0 || peek then
	  let result = input in_ch buf 0 1 in
	    if result = 1 then
	      if peek then
		(peek_buf.[!peek_buf_len] <- buf.[0];
		 peek_buf_len := !peek_buf_len + 1;	       
		 buf.[0])
	      else buf.[0]
	    else (close fd;raise (LDAP_Failure (`SERVER_DOWN, "", ext_res)))
	else if !peek_buf_pos = !peek_buf_len - 1 then (* last char in peek buf *)
	  let b = peek_buf.[!peek_buf_pos] in
	    peek_buf_pos := 0;
	    peek_buf_len := 0;
	    b
	else (* reading char in peek buf *)
	  let b = peek_buf.[!peek_buf_pos] in
	    peek_buf_pos := !peek_buf_pos + 1;
	    b
      in
	rb
    in
    let ssl_rb_of_fd fd = 
      IFDEF SSL THEN
        let buf = String.create 16384 (* the size of an ssl record *)
	and pos = ref 0
	and len = ref 0
	and peek_pos = ref 0 in
	let rec rb ?(peek=false) () = 
	  if !pos = !len || (peek && !peek_pos = !len) then
	    let result = 
	      try Ssl.read fd buf 0 16384 
	      with Read_error _ -> raise (Sys_error "")
	    in
	      if result >= 1 then
		(len := result;
		 (if peek then peek_pos := 1 else pos := 1);	    
		 buf.[0])
	      else (Ssl.shutdown fd;raise (LDAP_Failure (`SERVER_DOWN, "", ext_res)))
	  else
	    if peek then
	      let c = buf.[!peek_pos] in
		peek_pos := !peek_pos + 1;
		c
	    else
	      let c = buf.[!pos] in
		pos := !pos + 1;
		peek_pos := !pos;
		c	    
	in
	  rb
      ELSE
        raise (LDAP_Failure (`LOCAL_ERROR, "ssl support is not enabled", ext_res))
      END
    in
      {rb=(IFDEF SSL THEN
	     match fd with
		 Ssl s -> ssl_rb_of_fd s
	       | Plain s -> plain_rb_of_fd s
           ELSE
	     match fd with
		 Plain s -> plain_rb_of_fd s 
	   END);
       socket=fd;
       current_msgid=1;
       pending_messages=(Hashtbl.create 3);
       protocol_version=version}

(* sync auth_method types between the two files *)
let bind_s ?(who = "") ?(cred = "") ?(auth_method = `SIMPLE) con =
  let msgid = allocate_messageid con in
    (try
       send_message con
	 {messageID=msgid;
	  protocolOp=Bind_request
		       {bind_version=con.protocol_version;
			bind_name=who;
			bind_authentication=(Simple cred)};
	  controls=None};
       match receive_message con msgid with
	   {protocolOp=Bind_response {bind_result={result_code=`SUCCESS}}} -> ()
	 | {protocolOp=Bind_response {bind_result=res}} -> 
	     raise (LDAP_Failure 
		      (res.result_code, res.error_message, 
		       {ext_matched_dn=res.matched_dn;
			ext_referral=res.ldap_referral}))
	 | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid server response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid

let search ?(base = "") ?(scope = `SUBTREE) ?(aliasderef=`NEVERDEREFALIASES) 
  ?(sizelimit=0l) ?(timelimit=0l) ?(attrs = []) ?(attrsonly = false) con filter =
  let msgid = allocate_messageid con in
    try
      let e_filter = (try Ldap_filter.of_string filter 
		      with _ -> 
			(raise 
			   (LDAP_Failure 
			      (`LOCAL_ERROR, "bad search filter", ext_res))))
      in
	send_message con
	  {messageID=msgid;
	   protocolOp=Search_request
			{baseObject=base;
			 scope=scope;
			 derefAliases=aliasderef;
			 sizeLimit=sizelimit;
			 timeLimit=timelimit;
			 typesOnly=attrsonly;
			 filter=e_filter;
			 s_attributes=attrs};
	   controls=None};
	msgid
    with exn -> free_messageid con msgid;raise exn  

let get_search_entry con msgid =
  try
    match receive_message con msgid with
	{protocolOp=Search_result_entry e} -> `Entry e
      | {protocolOp=Search_result_reference r} -> `Referral r
      | {protocolOp=Search_result_done {result_code=`SUCCESS}} ->
	  raise (LDAP_Failure (`SUCCESS, "success", ext_res))
      | {protocolOp=Search_result_done res} ->
	raise (LDAP_Failure (res.result_code, res.error_message, 
			     {ext_matched_dn=res.matched_dn;
			      ext_referral=res.ldap_referral}))
      | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "unexpected search response", ext_res))
  with exn -> free_messageid con msgid;raise exn      

let abandon con msgid =
  let my_msgid = allocate_messageid con in
    try
      (try free_messageid con msgid with _ -> ());
      send_message con
	{messageID=my_msgid;
	 protocolOp=(Abandon_request msgid);
	 controls=None}
    with exn -> free_messageid con my_msgid;raise exn      

let search_s ?(base = "") ?(scope = `SUBTREE) ?(aliasderef=`NEVERDEREFALIASES) 
  ?(sizelimit=0l) ?(timelimit=0l) ?(attrs = []) ?(attrsonly = false) con filter =
  let msgid = search ~base:base ~scope:scope ~aliasderef:aliasderef ~sizelimit:sizelimit 
		~timelimit:timelimit ~attrs:attrs ~attrsonly:attrsonly con filter
  in
  let result = ref [] in
    (try 
       while true
       do
	 result := (get_search_entry con msgid) :: !result
       done
     with 
	 LDAP_Failure (`SUCCESS, _, _) -> ()
       | exn -> (try abandon con msgid with _ -> ());raise exn);
    free_messageid con msgid;
    !result

let add_s con (entry: entry) =
  let msgid = allocate_messageid con in
    (try
       send_message con
	 {messageID=msgid;
	  protocolOp=Add_request entry;
	  controls=None};
       match receive_message con msgid with
	   {protocolOp=Add_response {result_code=`SUCCESS}} -> ()
	 | {protocolOp=Add_response res} -> 
	     raise (LDAP_Failure (res.result_code, res.error_message, 
				  {ext_matched_dn=res.matched_dn;
				   ext_referral=res.ldap_referral}))
	 | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid add response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid      

let delete_s con ~dn =
  let msgid = allocate_messageid con in
    (try
       send_message con
	 {messageID=msgid;
	  protocolOp=Delete_request dn;
	  controls=None};
       match receive_message con msgid with
	   {protocolOp=Delete_response {result_code=`SUCCESS}} -> ()
	 | {protocolOp=Delete_response res} -> 
	     raise (LDAP_Failure (res.result_code, res.error_message, 
				  {ext_matched_dn=res.matched_dn;
				   ext_referral=res.ldap_referral}))
	 | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid delete response", ext_res))	     
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid

let unbind con = 
  try 
    (IFDEF SSL THEN
       match con.socket with
	   Ssl s -> Ssl.shutdown s
	 | Plain s -> close s
     ELSE
       match con.socket with
	   Plain s -> close s
     END)
  with _ -> ()

let modify_s con ~dn ~mods =
  let rec convertmods ?(converted=[]) mods =
    match mods with
	(op, attr, values) :: tl ->
	  (convertmods 
	     ~converted:({mod_op=op;
			  mod_value={attr_type=attr;
				     attr_vals=values}} :: converted)
	     tl)
      | [] -> converted
  in
  let msgid = allocate_messageid con in
    (try
       send_message con
	 {messageID=msgid;
	  protocolOp=Modify_request
		       {mod_dn=dn;
			modification=convertmods mods};
	  controls=None};
       match receive_message con msgid with
	   {protocolOp=Modify_response {result_code=`SUCCESS}} -> ()
	 | {protocolOp=Modify_response res} ->
	     raise (LDAP_Failure (res.result_code, res.error_message, 
				  {ext_matched_dn=res.matched_dn;
				   ext_referral=res.ldap_referral}))
	 | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid modify response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid

let modrdn_s ?(deleteoldrdn=true) ?(newsup=None) con ~dn ~newdn =
  let msgid = allocate_messageid con in
    (try
       send_message con
	 {messageID=msgid;
	  protocolOp=Modify_dn_request 
		       {modn_dn=dn;
			modn_newrdn=newdn;
			modn_deleteoldrdn=deleteoldrdn;
			modn_newSuperior=None};
	  controls=None};
       match receive_message con msgid with
	   {protocolOp=Modify_dn_response {result_code=`SUCCESS}} -> ()
	 | {protocolOp=Modify_dn_response res} -> 
	     raise (LDAP_Failure (res.result_code, res.error_message, 
				  {ext_matched_dn=res.matched_dn;
				   ext_referral=res.ldap_referral}))
	 | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid modify dn response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid
