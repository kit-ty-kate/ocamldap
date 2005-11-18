(* A parser for rfc2252 format schema definitionsa

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


open Ldap_schemalexer;;

module Oid = 
  (struct
     type t = string
     let of_string s = s
     let to_string oid = oid
     let compare x y = 
       String.compare 
	 (String.lowercase (to_string x))
	 (String.lowercase (to_string y))
   end
     :
   sig
     type t
     val of_string: string -> t
     val to_string: t -> string
     val compare: t -> t -> int
   end);;

module Oidset = Set.Make (Oid)
module Oidmap = Map.Make (Oid)

let format_oid id = 
  Format.open_box 0;
  Format.print_string ("<Oid.t " ^ Oid.to_string id ^ ">");
  Format.close_box ()

let format_oidset set = 
  Format.open_box 0;
  Format.print_string "<Oidset.t [";
  Format.print_cut ();
  List.iter 
    (fun oid -> 
       format_oid oid;
       Format.print_cut ())
    (Oidset.elements set);
  Format.print_string "]>";
  Format.close_box ()

module Lcstring =
  (struct
     type t = string
     exception Different of int
     let of_string s = s
     let to_string x = x
     let compare v1 v2 =
       (* does not cons unless v1 and v2 are different, 
	  then only conses a small amount *)
       let lv1 = String.length v1 in
       let lv2 = String.length v2 in
	 if lv1 < lv2 then -1
	 else if lv1 = lv2 then begin
	   if String.compare v1 v2 = 0 then 0
	   else
	     try
	       for i=0 to lv1 - 1
	       do
		 if v1.[i] <> v2.[i] then
		   if (Char.lowercase v1.[i]) <> (Char.lowercase v2.[i]) then
		     raise 
		       (Different 
			  (Char.compare 
			     (Char.lowercase v1.[i])
			     (Char.lowercase v2.[i])))
	       done;
	       0
	     with Different i -> i
	 end else 1
   end
     :
   sig
     type t
     val of_string: string -> t
     val to_string: t -> string
     val compare: t -> t -> int
   end);;

module Lcmap = Map.Make (Lcstring)

let format_lcstring id = 
  Format.open_box 0;
  Format.print_string ("<lcstring " ^ Lcstring.to_string id ^ ">");
  Format.close_box ()

type octype = Abstract | Structural | Auxiliary;;
type objectclass = {
  oc_name: string list;
  oc_oid: Oid.t;
  oc_desc: string;
  oc_obsolete: bool;
  oc_sup: string list;
  oc_must: string list;
  oc_may: string list;
  oc_type: octype;
  oc_xattr: string list
}

type attribute = {
  at_name: string list;
  at_desc: string;
  at_oid: Oid.t;
  at_equality: Oid.t option;
  at_ordering: Oid.t option;
  at_substr: Oid.t option;
  at_syntax: Oid.t;
  at_length:  Int64.t;
  at_obsolete: bool;
  at_single_value: bool;
  at_collective: bool;
  at_no_user_modification: bool;
  at_usage: string;
  at_sup: string list;
  at_xattr: string list
}
		  
type schema = {
  objectclasses: objectclass Lcmap.t;
  objectclasses_byoid: objectclass Oidmap.t;
  attributes: attribute Lcmap.t;
  attributes_byoid: attribute Oidmap.t
}

exception Invalid_objectclass of string
exception Non_unique_objectclass_alias of string
exception Invalid_attribute of string
exception Non_unique_attribute_alias of string

(* lookup functions *)
let attrNameToAttr schema attr =
  let attr = Lcstring.of_string attr in
    try (Lcmap.find attr schema.attributes) (* try canonical name first *)
    with Not_found ->
      (match 
	 Lcmap.fold
	   (fun k v matches ->
	      if (List.exists 
		    (fun n -> Lcstring.compare attr (Lcstring.of_string n) = 0)
		    v.at_name)
	      then
		v :: matches
	      else matches)
	   schema.attributes []
       with
           [] -> raise (Invalid_attribute (Lcstring.to_string attr))
	 | [attr] -> attr
	 | _ -> raise (Non_unique_attribute_alias (Lcstring.to_string attr)))

let ocNameToOc schema oc =
  let oc = Lcstring.of_string oc in
    try Lcmap.find oc schema.objectclasses
    with Not_found ->
      (match 
	 Lcmap.fold
	   (fun k v matches ->
	      if (List.exists 
		    (fun n -> Lcstring.compare oc (Lcstring.of_string n) = 0)
		    v.oc_name)
	      then
		v :: matches
	      else matches)
	   schema.objectclasses []
       with
           [] -> raise (Invalid_objectclass (Lcstring.to_string oc))
	 | [oc] -> oc
	 | _ -> raise (Non_unique_objectclass_alias (Lcstring.to_string oc)))

let attrNameToOid schema attr = (attrNameToAttr schema attr).at_oid

let oidToAttr schema attr = Oidmap.find attr schema.attributes_byoid

let oidToAttrName schema attr = 
  List.hd (Oidmap.find attr schema.attributes_byoid).at_name

let ocNameToOid schema oc = (ocNameToOc schema oc).oc_oid

let oidToOc schema oc = Oidmap.find oc schema.objectclasses_byoid

let oidToOcName schema oc = List.hd (oidToOc schema oc).oc_name

let compareAttrs schema a1 a2 = 
  Oid.compare (attrNameToOid schema a1) (attrNameToOid schema a2)

let compareOcs schema oc1 oc2 = 
  Oid.compare (ocNameToOid schema oc1) (ocNameToOid schema oc2)

let rec lookupMatchingRule schema rtype attr =
  let mrule = 
    match rtype with
	`Ordering -> attr.at_ordering
      | `Substring -> attr.at_substr
      | `Equality -> attr.at_equality
  in
    match mrule with
	Some oid -> Some oid
      | None ->
	  List.fold_left
	    (fun mrule superior -> 
	       match mrule with
		   None -> lookupMatchingRule schema rtype (attrNameToAttr schema superior)
		 | _ -> mrule)
	    None
	    attr.at_sup

let schema_print_depth = ref 10
let format_schema s =
  let indent = 3 in
  let printtbl tbl = 
    let i = ref 0 in
      try
	Lcmap.iter
	  (fun aname aval -> 
	     if !i < !schema_print_depth then begin
	       Format.print_string ("<KEY " ^ (Lcstring.to_string aname) ^ ">");
	       Format.print_break 1 indent;
	       i := !i + 1
	     end 
	     else failwith "depth")
	  tbl
      with Failure "depth" -> Format.print_string "..."
  in
    Format.open_box 0;
    Format.print_string "{objectclasses = <HASHTBL ";
    Format.print_break 0 indent;  
    printtbl s.objectclasses;
    Format.print_string ">;";
    Format.print_break 0 1;
    Format.print_string "objectclasses_byoid = <HASHTBL ...>;";
    Format.print_break 0 1;
    Format.print_string "attributes = <HASHTBL ";
    Format.print_break 0 indent;
    printtbl s.attributes;
    Format.print_string ">;";
    Format.print_break 0 1;
    Format.print_string "attributes_byoid = <HASHTBL ...>}";
    Format.close_box ()

exception Parse_error_oc of Lexing.lexbuf * objectclass * string;;
exception Parse_error_at of Lexing.lexbuf * attribute * string;;
exception Syntax_error_oc of Lexing.lexbuf * objectclass * string;;
exception Syntax_error_at of Lexing.lexbuf * attribute * string;;

let rec readSchema oclst attrlst =
  let empty_oc = 
    {oc_name=[];oc_oid=Oid.of_string "";oc_desc="";oc_obsolete=false;oc_sup=[];
     oc_must=[];oc_may=[];oc_type=Abstract;oc_xattr=[]} 
  in
  let empty_attr = 
    {at_name=[];at_oid=Oid.of_string "";at_desc="";
     at_equality=None;at_ordering=None;
     at_usage=""; at_substr=None;
     at_syntax=(Oid.of_string "1.3.6.1.4.1.1466.115.121.1.26");
     at_length=0L;at_obsolete=false;at_single_value=false;
     at_collective=false;at_no_user_modification=false;
     at_sup=[];at_xattr=[]} 
  in
  let readOc lxbuf oc =
    let rec readOptionalFields lxbuf oc =
      try match (lexoc lxbuf) with
	  Name s                -> readOptionalFields lxbuf {oc with oc_name=s}
	| Desc s                -> readOptionalFields lxbuf {oc with oc_desc=s}
	| Obsolete              -> readOptionalFields lxbuf {oc with oc_obsolete=true}
	| Sup s                 -> readOptionalFields lxbuf {oc with oc_sup=s}
	| Ldap_schemalexer.Abstract   -> readOptionalFields lxbuf {oc with oc_type=Abstract}
	| Ldap_schemalexer.Structural -> readOptionalFields lxbuf {oc with oc_type=Structural}
	| Ldap_schemalexer.Auxiliary  -> readOptionalFields lxbuf {oc with oc_type=Auxiliary}
	| Must s                -> readOptionalFields lxbuf {oc with oc_must=s}
	| May s                 -> readOptionalFields lxbuf {oc with oc_may=s}
	| Xstring t             -> readOptionalFields lxbuf {oc with oc_xattr=(t :: oc.oc_xattr)}
	| Rparen                -> oc
	| _                     -> raise (Parse_error_oc (lxbuf, oc, "unexpected token"))
      with Failure(_) -> raise (Parse_error_oc (lxbuf, oc, "Expected right parenthesis"))
    in
    let readOid lxbuf oc = 
      try match (lexoc lxbuf) with
	  Numericoid(s) -> readOptionalFields lxbuf {oc with oc_oid=Oid.of_string s}
	| _ -> raise (Parse_error_oc (lxbuf, oc, "missing required field, numericoid"))
      with Failure(_) -> raise (Syntax_error_oc (lxbuf, oc, "Syntax error")) 
    in
    let readLparen lxbuf oc =
      try match (lexoc lxbuf) with
	  Lparen -> readOid lxbuf oc
	| _ -> raise (Parse_error_oc (lxbuf, oc, "Expected left paren"))
      with Failure(_) -> raise (Syntax_error_oc (lxbuf, oc, "Syntax error")) 
    in
      readLparen lxbuf oc
  in
  let rec readAttr lxbuf attr =
    let rec readOptionalFields lxbuf attr =
      try match (lexattr lxbuf) with	  
	  Name s              -> readOptionalFields lxbuf {attr with at_name=s}
	| Desc s              -> readOptionalFields lxbuf {attr with at_desc=s}
	| Obsolete            -> readOptionalFields lxbuf {attr with at_obsolete=true}
	| Sup s               -> readOptionalFields lxbuf {attr with at_sup=s}
	| Equality s          -> 
	    readOptionalFields lxbuf 
	      {attr with at_equality=(Some (Oid.of_string s))}
	| Substr s            -> 
	    readOptionalFields lxbuf 
	      {attr with at_substr=(Some (Oid.of_string s))}
	| Ordering s          -> 
	    readOptionalFields lxbuf 
	      {attr with at_ordering=(Some (Oid.of_string s))}
	| Syntax (s, l)       -> 
	    readOptionalFields lxbuf {attr with at_syntax=Oid.of_string s;at_length=l}
	| Single_value         -> readOptionalFields lxbuf {attr with at_single_value=true}
	| Collective           -> readOptionalFields lxbuf {attr with at_collective=true}
	| No_user_modification -> readOptionalFields lxbuf {attr with at_no_user_modification=true}
	| Usage s              -> readOptionalFields lxbuf {attr with at_usage=s}
	| Rparen               -> attr
	| Xstring t            -> 
	    (readOptionalFields 
	       lxbuf 
	       {attr with at_xattr=(t :: attr.at_xattr)})
	| _                    -> raise (Parse_error_at (lxbuf, attr, "unexpected token"))
      with Failure(f) -> raise (Parse_error_at (lxbuf, attr, f))
    in
    let readOid lxbuf attr = 
      try match (lexoc lxbuf) with
	  Numericoid(s) -> readOptionalFields lxbuf {attr with at_oid=Oid.of_string s}
	| _ -> raise (Parse_error_at (lxbuf, attr, "missing required field, numericoid"))
      with Failure(_) -> raise (Syntax_error_at (lxbuf, attr, "Syntax error")) 
    in
    let readLparen lxbuf attr =
      try match (lexoc lxbuf) with
	  Lparen -> readOid lxbuf attr
	| _ -> raise (Parse_error_at (lxbuf, attr, "Expected left paren"))
      with Failure(_) -> raise (Syntax_error_at (lxbuf, attr, "Syntax error"))
    in
      readLparen lxbuf attr
  in
  let schema = 
    {objectclasses=Lcmap.empty;
     objectclasses_byoid=Oidmap.empty;		
     attributes=Lcmap.empty;
     attributes_byoid=Oidmap.empty} 
  in
    List.fold_left (* add attributes to the map *)
      (fun schema a ->
	 let attr = readAttr (Lexing.from_string a) empty_attr in
	   {schema with
	      attributes=
	       (List.fold_left 
		  (fun map n -> Lcmap.add (Lcstring.of_string n) attr map)
		  schema.attributes
		  attr.at_name);
	      attributes_byoid=
	       (Oidmap.add attr.at_oid attr schema.attributes_byoid)})
      (List.fold_left (* add objectclasses to the map *)
	 (fun schema a ->
	    let oc = readOc (Lexing.from_string a) empty_oc in 
	      {schema with
		 objectclasses=
		  (List.fold_left 
		     (fun map n -> Lcmap.add (Lcstring.of_string n) oc map)
		     schema.objectclasses
		     oc.oc_name);
		 objectclasses_byoid=
		  (Oidmap.add oc.oc_oid oc schema.objectclasses_byoid)})
	 schema
	 oclst)
      attrlst
