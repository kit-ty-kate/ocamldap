(* An object oriented interface for parsing Lightweight Directory
   Interchange Format file

   Copyright (C) 2004 Eric Stokes, Matthew Backes, and The California
   State University at Northridge

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


open Str;;
open Netencoding;;
open Ldap_ooclient;;
open Ldif_parser;;

let safe_string_regex = 
  Str.regexp "[^ :<\x0d\x0a\x00]\\([^\x0d\x0a\x00]*[^ \x0d\x0a\x00]\\)?$"

let password_regex =
  Str.regexp_case_fold ".*p\\(ass\\)?w\\(or\\)?d$"

let safe_val s = 
  if Str.string_match safe_string_regex s 0 then ": " ^ s ^ "\n"
  else ":: " ^ Base64.encode s ^ "\n"

let safe_attr_val a v =
  if Str.string_match password_regex a 0 then
    a ^ ":: " ^ Base64.encode v ^ "\n"
  else a ^ safe_val v
    
class ldif ?(in_ch=stdin) ?(out_ch=stdout) () =
object (self)
  val in_ch  = {stream=(Stream.of_channel in_ch);buf=Buffer.create 256;line=1}
  val out_ch = out_ch
  method read_entry =
    match (ldif_attrval_record in_ch) with
	{dn=dn;attrs=attrs} -> 
	  let e = new ldapentry in
	    e#set_dn dn;
	    List.iter 
	      (fun attr ->
		 match attr with
		     (name, value) -> e#add [(name, [value])])
	      attrs;
	    e
  method of_string s =
    let strm = {stream=(Stream.of_string s);buf=Buffer.create 256;line=1} in
      match (ldif_attrval_record strm) with
	  {dn=dn;attrs=attrs} -> 
	    let e = new ldapentry in
	      e#set_dn dn;
	      List.iter 
		(fun attr ->
		   match attr with
		       (name, value) -> e#add [(name, [value])])
		attrs;
	      e
  method to_string (e:ldapentry_t) =
    ("dn" ^ (safe_val (e#dn)) ^ 
     (String.concat ""
        (List.map 
           (fun attr ->
              (String.concat "" 
                 (List.map 
                    (fun value -> safe_attr_val attr value)
                    (e#get_value attr))))
           (e#attributes))) ^
     "\n")
  method write_entry (e:ldapentry_t) =
    output_string out_ch (self#to_string e)      
end
