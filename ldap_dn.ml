(* Utility functions for operating on dns

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

open Ldap_types
open Ldap_dnparser

let of_string dn_string = 
  try Ldap_dnparser.dn dn_string
  with Parsing.Parser_error | Failure _ ->
    raise (Invalid_dn "parse error")

let escape_value valu = 
  let strm = Stream.of_string valu in
  let buf = Buffer.create ((String.length valu) + 10) in
  let rec escape strm buf = 
    match Stream.next strm with
	(',' | '=' | '+' | '<' | '>' | '#' | ';' | '\\' | '"') as c ->
	  Buffer.add_char buf '\\';
	  Buffer.add_char buf c;
	  escape strm buf
      | c -> Buffer.add_char buf c;escape strm buf

let to_string dn = 
  List.fold_left
    (fun s {attr_type=attr;attr_vals=vals} ->
       
