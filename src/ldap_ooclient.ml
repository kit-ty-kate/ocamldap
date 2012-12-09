(* An object oriented interface to ldap

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)

module Make (M : Ldap_types.Monad) = struct

open Ldap_types
module Ldap_funclient = Ldap_funclient.Make(M)
open Ldap_funclient
open Ldap_schema
open String

let (>>=) = M.bind

(* types used throughout the library *)
(* add types *)
type op = string * string list
type op_lst = op list
type referral_policy = [ `RETURN
                       | `FOLLOW ]

(* change type for ldap entry *)
type changetype = [ `ADD | `DELETE | `MODIFY | `MODDN | `MODRDN ]

class type ldapentry_t =
object
  method add : op_lst -> unit
  method delete : op_lst -> unit
  method replace : op_lst -> unit
  method modify : (modify_optype * string * string list) list -> unit
  method attributes : string list
  method exists : string -> bool
  method get_value : string -> string list
  method diff : ldapentry_t -> (modify_optype * string * string list) list
  method changes : (modify_optype * string * string list) list
  method changetype : changetype
  method set_changetype : changetype -> unit
  method flush_changes : unit
  method dn : string
  method set_dn : string -> unit
  method print : unit
end;;

class type ldapcon_t =
object
  method add : ldapentry_t -> unit
  method bind :
    ?cred:string -> ?meth:Ldap_funclient.authmethod -> string -> unit
  method delete : string -> unit
  method modify :
    string ->
    (Ldap_types.modify_optype * string * string list) list -> unit
  method modrdn : string -> ?deleteoldrdn:bool -> string -> unit
  method rawschema : ldapentry_t
  method schema : Ldap_schema.schema
  method search :
    ?scope:Ldap_types.search_scope ->
    ?attrs:string list ->
    ?attrsonly:bool -> ?base:string -> string -> ldapentry_t list
  method search_a :
    ?scope:Ldap_types.search_scope ->
    ?attrs:string list ->
    ?attrsonly:bool -> ?base:string -> string -> (?abandon:bool -> unit -> ldapentry_t)
  method unbind : unit
  method update_entry : ldapentry_t -> unit
end

let format_entry e =
  Format.open_box 0;
  Format.open_box 2;
  Format.print_string ("<ldapentry_t " ^ (String.escaped e#dn));
  Format.force_newline ();
  let length_attrs = List.length e#attributes in
  let j = ref 0 in
    List.iter
      (fun a ->
         let length = List.length (e#get_value a) in
         let i = ref 0 in
           Format.print_string (Printf.sprintf "(\"%s\", " (String.escaped a));
           Format.open_box 0;
           Format.print_string "[";
           List.iter
             (fun v ->
                if !i < length - 1 then
                  (Format.print_string (Printf.sprintf "\"%s\";" (String.escaped v));
                   Format.print_break 1 0)
                else
                  Format.print_string (Printf.sprintf "\"%s\"" (String.escaped v));
                i := !i + 1)
             (e#get_value a);
           Format.print_string "]";
           Format.close_box ();
           (if !j < length_attrs - 1 then
              (Format.print_string ");";
               Format.force_newline ())
            else
              Format.print_string ")");
           j := !j + 1)
      (e#attributes);
    Format.close_box ();
    Format.print_string ">";
    Format.close_box ()

let format_entries lst =
  let length = List.length lst in
  let i = ref 0 in
    Format.open_box 0;
    Format.print_string "[";
    if length > 3 then
      try
        List.iter
          (fun e ->
             if !i > 49 then failwith "limit"
             else if !i < length - 1 then begin
               Format.print_string ("<ldapentry_t " ^ (String.escaped e#dn) ^ ">; ");
               Format.print_cut ();
               i := !i + 1
             end else
               Format.print_string ("<ldapentry_t " ^ (String.escaped e#dn) ^ ">"))
          lst
      with Failure "limit" -> Format.print_string "..."
    else
      List.iter
        (fun e ->
           if !i < length - 1 then begin
             format_entry e;
             Format.print_break 1 0;
             i := !i + 1
           end else
             format_entry e)
        lst;
    Format.print_string "]";
    Format.close_box ()

module CaseInsensitiveString =
  (struct
     type t = string * string
     let of_string s = (s, String.lowercase s)
     let to_string x = fst x
     let compare x y = String.compare (snd x) (snd y)
   end
     :
   sig
     type t
     val of_string: string -> t
     val to_string: t -> string
     val compare: t -> t -> int
   end);;

module OrdStr =
struct
  type t = CaseInsensitiveString.t
  let compare = CaseInsensitiveString.compare
end;;

(* types for a set of Oids, and a set of strings *)
module Strset = Set.Make (OrdStr)

(********************************************************************************)
(********************************************************************************)
(********************************************************************************)
(* ldap entry object *)
class ldapentry =
object (self)
  val mutable dn = ""
  val mutable data = Hashtbl.create 50
  val mutable changes = []
  val mutable changetype = `ADD

  method private push_change (t:modify_optype) lst =
    match changetype with
        `MODIFY -> (match lst with
                        [] -> ()
                      | (attr, values) :: tail ->
                          changes <- (t, attr, values) :: changes; self#push_change t tail)
      | _ -> ()

  method changetype = changetype;
  method set_changetype (typ:changetype) = changetype <- typ
  method flush_changes = changes <- []
  method changes = changes

  method exists x = Hashtbl.mem data (lowercase x)
  method add (x:op_lst) =
    let rec do_add (x:op_lst) =
      match x with
          [] -> ()
        | (name, value) :: lst ->
            let lcname = lowercase name in
              try
                Ulist.addlst (Hashtbl.find data lcname) value; do_add lst
              with Not_found ->
                let current = Ulist.create 5 in
                  Hashtbl.add data lcname current; Ulist.addlst current value; do_add lst
    in
      do_add x; self#push_change `ADD x

  method diff (entry: ldapentry_t) =
    let diff_entries e1 e2 : (modify_optype * string * string list) list =
      let rec setOfList ?(set=Strset.empty) list =
        match list with
            a :: tail -> setOfList ~set:(Strset.add a set) tail
          | []  -> set
      in
      let ciStringlst list =
        List.rev_map
          CaseInsensitiveString.of_string
          list
      in
      let e1attrs = setOfList (ciStringlst e1#attributes) in
      let e2attrs = setOfList (ciStringlst e2#attributes) in
      let add_attrs =
        Strset.fold
          (fun attr mods ->
             let attr = CaseInsensitiveString.to_string attr in
               (`REPLACE, attr, e1#get_value attr) :: mods)
          (Strset.diff e1attrs (Strset.inter e1attrs e2attrs))
          []
      in
      let remove_attrs =
        Strset.fold
          (fun attr mods ->
             let attr = CaseInsensitiveString.to_string attr in
               (`DELETE, attr, []) :: mods)
          (Strset.diff e2attrs (Strset.inter e2attrs e1attrs))
          []
      in
      let sync_attrs =
        Strset.fold
          (fun attr mods ->
             let attr = CaseInsensitiveString.to_string attr in
             let e1vals = setOfList (ciStringlst (e1#get_value attr)) in
             let e2vals = setOfList (ciStringlst (e2#get_value attr)) in
               if (not (Strset.is_empty (Strset.diff e1vals (Strset.inter e1vals e2vals)))) ||
                 (not (Strset.is_empty (Strset.diff e2vals (Strset.inter e1vals e2vals))))
               then
                 (`REPLACE, attr, e1#get_value attr) :: mods
               else
                 mods)
          (Strset.inter e1attrs (Strset.inter e1attrs e2attrs))
          []
      in
        List.rev_append remove_attrs (List.rev_append sync_attrs add_attrs)
    in
      (diff_entries self entry)

  method delete (x:op_lst) =
    let rec do_delete x =
      match x with
          [] -> ()
        | (attr, values) :: lst ->
            let lcname = lowercase attr in
              match values with
                  [] -> Hashtbl.remove data lcname;do_delete lst
                | _  ->
                    (try List.iter (Ulist.remove (Hashtbl.find data lcname)) values
                     with Not_found -> ());
                    (match Ulist.tolst (Hashtbl.find data lcname) with
                         [] -> Hashtbl.remove data lcname
                       | _  -> ());
                    do_delete lst
    in
      do_delete x; self#push_change `DELETE x

  method replace (x:op_lst) =
    let rec do_replace x =
      match x with
          [] -> ()
        | (attr, values) :: lst -> let n = Ulist.create 5 in
            Ulist.addlst n values; Hashtbl.replace data (lowercase attr) n;
            do_replace lst;
    in
      do_replace x; self#push_change `REPLACE x

  method modify (x: (modify_optype * string * string list) list) =
    let rec do_modify x =
      match x with
          [] -> ()
        | (`ADD, attr, values) :: t -> self#add [(attr, values)];do_modify t
        | (`DELETE, attr, values) :: t -> self#delete [(attr, values)];do_modify t
        | (`REPLACE, attr, values) :: t -> self#replace [(attr, values)];do_modify t
    in
      do_modify x

  method attributes =
    let keys hash =
      let cur = ref [] in
      let key k _ = cur := k :: !cur in
        Hashtbl.iter key hash; !cur
    in
      keys data

  method get_value attr = Ulist.tolst (Hashtbl.find data (lowercase attr))
  method set_dn x = dn <- x
  method dn = dn
  method print =
    print_endline "THIS METHOD IS DEPRECATED, use Ldif_oo, or rely on the toplevel printers";
    print_endline ("dn: " ^ self#dn);
    (List.iter
       (fun a ->
          (List.iter
             (fun b -> print_endline (a ^ ": " ^ b))
             (self#get_value a)))
       self#attributes)

end

type changerec =
    [`Modification of string * ((Ldap_types.modify_optype * string * string list) list)
    | `Addition of ldapentry
    | `Delete of string
    | `Modrdn of string * int * string]

(********************************************************************************)
(********************************************************************************)
(********************************************************************************)
let to_entry ent =
  let rec add_attrs attrs entry =
    match attrs with
        {attr_type = name; attr_vals = values} :: tail ->
          entry#add [(name, values)]; add_attrs tail entry
      | [] -> entry#set_changetype `MODIFY; entry
  in
    match ent with
        `Entry {sr_dn = dn; sr_attributes = attrs} ->
          let entry = new ldapentry in
            entry#set_dn dn; add_attrs attrs entry
      | `Referral refs ->
          let entry = new ldapentry in
            entry#set_dn "referral";
            entry#add [("ref", refs)];
            entry#add [("objectclass", ["referral"])];
            entry

let of_entry ldapentry =
  let rec extract_attrs ?(converted=[]) entry attrs =
    match attrs with
        [] -> converted
      | attr :: tail ->
          extract_attrs
            ~converted:({attr_type=attr;
                         attr_vals=(entry#get_value attr)} :: converted)
            entry
            tail
  in
    {sr_dn=(ldapentry#dn);
     sr_attributes=(extract_attrs ldapentry ldapentry#attributes)}

let iter (f: ldapentry -> unit) (res: ?abandon:bool -> unit -> ldapentry) =
  try
    while true
    do
      f (res ());
    done
  with
      LDAP_Failure (`SUCCESS, _, _) -> ()
    | exn -> (try ignore (res ~abandon:true ()) with _ -> ());raise exn

let rev_map (f: ldapentry -> 'a) (res: ?abandon:bool -> unit -> ldapentry) =
  let lst = ref [] in
    (try while true
     do
       lst := (f (res ())) :: !lst
     done
     with
         LDAP_Failure (`SUCCESS, _, _) -> ()
       | exn -> (try ignore (res ~abandon:true ()) with _ -> ());raise exn);
    !lst

let map (f: ldapentry -> 'a) (res: ?abandon:bool -> unit -> ldapentry) =
  List.rev (rev_map f res)

let fold (f:ldapentry -> 'a -> 'a) (v:'a) (res: ?abandon:bool -> unit -> ldapentry) =
  let value = ref v in
    try
      while true
      do
        value := (f (res ()) !value)
      done;
      !value
    with
        LDAP_Failure (`SUCCESS, _, _) -> !value
      | exn -> (try ignore (res ~abandon:true ()) with _ -> ());raise exn

(* a connection to an ldap server *)
class ldapcon ?(connect_timeout=1) ?(referral_policy=`RETURN) ?(version = 3) hosts =
object (self)
  val mutable bdn = ""
  val mutable pwd = ""
  val mutable mth = `SIMPLE
  val mutable bound = true
  val mutable reconnect_successful = true
  val mutable con = init ~connect_timeout:connect_timeout ~version:version hosts
  method private reconnect =
    con >>= fun conn ->
    (if bound then unbind conn else M.return ()) >>= fun () ->
    bound <- false;
    reconnect_successful <- false;
    con <- init ~connect_timeout:connect_timeout ~version:version hosts;
    bound <- true;
    bind_s ~who: bdn ~cred: pwd ~auth_method: mth conn;
    reconnect_successful <- true;
    M.return ()

  method unbind =
    con >>= fun conn ->
    if bound then
      unbind conn >>= (fun () ->
        bound <- false;
        M.return ()
      )
    else
      M.return ()

  method update_entry (e:ldapentry) =
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    M.catch
      (fun () ->
        self#modify e#dn (List.rev e#changes) >>= fun () ->
        e#flush_changes;
        M.return ()
      )
      (function
        | LDAP_Failure(`SERVER_DOWN, _, _) ->
            self#reconnect >>= fun () -> self#update_entry e
        | exn -> M.fail exn
      )

  method bind ?(cred = "") ?(meth:authmethod = `SIMPLE) dn =
    if not bound then begin
      con <- init ~connect_timeout:connect_timeout ~version: version hosts;
      bound <- true
    end;
    con >>= fun conn ->
    bind_s ~who: dn ~cred: cred ~auth_method: meth conn;
    reconnect_successful <- true;
    bdn <- dn; pwd <- cred; mth <- meth;
    M.return ()

  method add (entry: ldapentry) =
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    M.catch
      (fun () ->
        con >>= fun conn ->
        add_s conn (of_entry entry)
      )
      (function
        | LDAP_Failure(`SERVER_DOWN, _, _) ->
            self#reconnect >>= fun () ->
            self#add entry
        | exn -> M.fail exn
      )

  method delete dn =
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    M.catch
      (fun () ->
        con >>= fun conn ->
        delete_s conn dn
      )
      (function
        | LDAP_Failure(`SERVER_DOWN, _, _) ->
            self#reconnect >>= fun () ->
            self#delete dn
        | exn -> M.fail exn
      )

  method modify dn mods =
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    M.catch
      (fun () ->
        con >>= fun conn ->
        modify_s conn dn mods
      )
      (function
        | LDAP_Failure(`SERVER_DOWN, _, _) ->
            self#reconnect >>= fun () ->
            self#modify dn mods
        | exn -> M.fail exn
      )

  method modrdn dn ?(deleteoldrdn = true) ?(newsup: string option=None) newrdn =
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    M.catch
      (fun () ->
        con >>= fun conn ->
        modrdn_s conn ~dn ~newdn:newrdn ~deleteoldrdn ~newsup
      )
      (function
        | LDAP_Failure(`SERVER_DOWN, _, _) ->
            self#reconnect >>= fun () ->
            self#modrdn dn ~deleteoldrdn:deleteoldrdn newrdn
        | exn -> M.fail exn
      )

  method search
    ?(scope = `SUBTREE)
    ?(attrs = [])
    ?(attrsonly = false)
    ?(base = "")
    ?(sizelimit = 0l)
    ?(timelimit = 0l)
    filter =
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    M.catch
      (fun () ->
        con >>= fun conn ->
        search_s
          ~scope ~base ~attrs
          ~attrsonly ~sizelimit
          ~timelimit conn filter
        >>= fun x ->
        M.return (List.rev_map to_entry x)
      )
      (function
        | LDAP_Failure(`SERVER_DOWN, _, _) ->
            self#reconnect >>= fun () ->
            self#search
              ~scope ~attrs ~attrsonly
              ~base ~sizelimit ~timelimit filter
        | exn -> M.fail exn
      )

  method search_a
    ?(scope = `SUBTREE)
    ?(attrs = [])
    ?(attrsonly = false)
    ?(base = "")
    ?(sizelimit = 0l)
    ?(timelimit = 0l)
    filter =

    (* a function which is returned by search_a, calling it will give
       the next entry due from the async search. The first_entry
       argument is there to maintain the semantics of ldapcon's
       transparent reconnection system. When search_a is called, we
       fetch the first entry, and pass it in to this function. We do
       this because, we will not know if the server actually recieved
       our search until we read the first entry. *)
    let fetch_result con (msgid:msgid) first_entry ?(abandon=false) () =
      if abandon then
        (Ldap_funclient.abandon con msgid;
         self#reconnect >>= fun () ->
         M.return (to_entry (`Entry {sr_dn="";sr_attributes=[]})))
      else
        match !first_entry with (* are we on the first entry of the search? *)
          | `No ->
              get_search_entry con msgid
              >>= fun x ->
              M.return (to_entry x)
          | `Yes e ->
              first_entry := `No;
              M.return (to_entry e)
          | `NoResults -> (* this search has no results *)
              raise
                (LDAP_Failure
                   (`SUCCESS, "success",
                    {ext_matched_dn = ""; ext_referral = None}))
    in
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    M.catch
      (fun () ->
        con >>= fun conn ->
        let first_entry = ref `No in
        let msgid =
          search
            ~scope ~base ~attrs ~attrsonly
            ~sizelimit ~timelimit
            conn filter
        in
        msgid >>= fun msgid ->
          (* make sure the server is really still there *)
          (try
             get_search_entry conn msgid >>= fun x ->
             first_entry := `Yes x;
             M.return ()
           with LDAP_Failure (`SUCCESS, _, _) ->
             (* the search is already complete and has no results *)
             first_entry := `NoResults;
             M.return ()
          );
          M.return (fetch_result conn msgid first_entry)
      )
      (function
        | LDAP_Failure(`SERVER_DOWN, _, _) ->
            self#reconnect >>= fun () ->
            self#search_a
              ~scope ~attrs ~attrsonly ~base
              ~sizelimit ~timelimit filter
        | exn -> M.fail exn
      )

  method schema =
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    try
      if version = 3 then begin
        let schema_base =
          self#search
            ~base: ""
            ~scope: `BASE
            ~attrs: ["subschemasubentry"]
            "(objectclass=*)"
          >>= function
            | [e] -> M.return (List.hd (e#get_value "subschemasubentry"))
            |  _  -> raise Not_found
        in
        schema_base >>= fun schema_base ->
        self#search
          ~base: schema_base
          ~scope: `BASE
          ~attrs: ["objectClasses";"attributeTypes";
                   "matchingRules";"ldapSyntaxes"]
          "(objectclass=subschema)"
        >>= function
          | [e] ->
              M.return
                (readSchema
                   (e#get_value "objectclasses")
                   (e#get_value "attributetypes")
                )
          |  _  -> raise Not_found
      end
      else
        raise Not_found
    with LDAP_Failure(`SERVER_DOWN, _, _) ->
      self#reconnect >>= fun () ->
      self#schema

  method rawschema =
    (if not (reconnect_successful && bound)
     then self#reconnect
     else M.return ()
    )
    >>= fun () ->
    try
      if version = 3 then begin
        let schema_base =
          self#search
            ~base: ""
            ~scope: `BASE
            ~attrs: ["subschemasubentry"]
            "(objectclass=*)"
          >>= function
            | [e] -> M.return (List.hd (e#get_value "subschemasubentry"))
            |  _  -> raise Not_found
        in
        schema_base >>= fun schema_base ->
        self#search
          ~base: schema_base
          ~scope: `BASE
          ~attrs: ["objectClasses";"attributeTypes";
                   "matchingRules";"ldapSyntaxes"]
          "(objectclass=*)"
        >>= function
          | [e] -> M.return e
          |  _  -> raise Not_found
      end
      else
        raise Not_found
    with LDAP_Failure(`SERVER_DOWN, _, _) -> self#reconnect;self#rawschema
end;;

end
