(* a test program for ldap_funclient

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

(* $Id$ *)

open Ocamldap

let _ = 
  try
    let ld = init ~port: 2389 "yourhost" in
    let _ = bind_s ~who:"cn=directory manager,o=csun" ~cred:"yourpas" ld in
    let add_res = add_s ld "cn=test,o=yourorg" 
      [ 
        (`ADD, "objectClass", ["top";"person"]);
        (`ADD, "cn", ["test"]);
        (`ADD, "sn", ["constructor"]);
      ] in
    let mod_res = modify_s ld "cn=test,o=yourorg"
      [ 
        (`REPLACE, "sn", ["modified"]);
      ] in
    let modrdn_res = modrdn_s ld "cn=test,o=yourorg" "cn=test1" in
    let search_res = search_s ~base:"o=yourorg" ~scope:`ONELEVEL ld "(cn=test1)" in
    let delete_res = delete_s ld "cn=test1,o=yourorg" in
    match search_res with
      [] -> print_endline "got no results"
    | _ -> List.iter print_entry search_res;
    unbind ld
  with
    LDAP_Failure x -> Printf.printf "caught ldap error: %s\n" (err2string x)
