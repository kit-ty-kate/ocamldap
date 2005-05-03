(* create an ldap changerec factory from a channel attached to an ldif
   changerec source default is stdin and stdout.

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

open Ldap_ooclient
open Ldif_changerec_parser
open Ldif_changerec_lexer

exception Invalid_changerec of string
exception End_of_changerecs

class change ?(in_ch=stdin) ?(out_ch=stdout) () =
object (self)
  val lxbuf = Lexing.from_channel in_ch
  method read_changerec =       
    try changerec lexcr lxbuf
    with
	Failure "end" -> raise End_of_changerecs
      | Failure s -> raise (Invalid_changerec s)
  method of_string (s:string) =
    let lx = Lexing.from_string s in
      try changerec lexcr lx
      with
	  Failure "end" -> raise End_of_changerecs
	| Failure s -> raise (Invalid_changerec s)
  method to_string (e:changerec) = failwith "not implemented";""
  method write_changerec (e:changerec) = failwith "not implemented";()
end
