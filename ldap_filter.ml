(* Ldap filter parser driver.

   Copyright (C) 2004 Eric Stokes, and The California State University at
   Northridge

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

open Ldap_filterparser
open Ldap_filterlexer
open Str

let of_string f = 
  let lxbuf = Lexing.from_string f in
    filter lexfilter lxbuf

(* todo, deal with escaping
let to_string f =
  let rec to_string' buf f =
    match f with
	`And lst -> 
	  Buffer.add_string buf "(&";
	  List.iter
	    (fun f_component -> to_string' buf f_component)
	    lst;
	  Buffer.add_char buf ')'
      | `Or lst ->
	  Buffer.add_string buf "(|";
	  List.iter
	    (fun f_component -> to_string' buf f_component)
	    lst;
	  Buffer.add_char buf ')'
      | `Not f_component ->
	  Buffer.add_string buf "(!";
	  to_string' buf f_component;
	  Buffer.add_char buf ')'
      | `EqualityMatch {attributeDesc=attrname;assertionValue=valu} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_char buf '=';
	  Buffer.add_string buf valu;
	  Buffer.add_char buf ')'
      | `Substrings {attrtype=attrname;
		     substrings={substr_initial=initial;
				 substr_any=any;
				 substr_final=final}} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_char buf '=';
	  Buffer.add_string buf
	    (global_replace (regexp "\\*\\*") "*"
	       ((match initial with
		     Some s -> s ^ "*"
		   | None -> "") ^
		(match any with
		     Some s -> "*" ^ s ^ "*"
		   | None -> "") ^
		(match final with
		     Some s -> "*" ^ s
		   | None -> "")))
      | `GreaterOrEqual {attributeDesc=attrname;assertionValue=valu} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_string buf ">=";
	  Buffer.add_string buf valu;
	  Buffer.add_char buf ')'
      | `LessOrEqual {attributeDesc=attrname;assertionValue=valu} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_string buf "<=";
	  Buffer.add_string buf valu;
	  Buffer.add_char buf ')'
      | `ApproxMatch {attributeDesc=attrname;assertionValue=valu} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_char buf '~';
	  Buffer.add_string buf valu;
	  Buffer.add_char buf ')'	  
      | `Present attr ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_string buf "=*";
	  Buffer.add_char buf ')'
      | `ExtensibleMatch {matchingRule=rule;ruletype=rtype;matchValue=}
*)

let f_not filterlist = `Not filterlist
let f_or filterlist = `Or filterlist
let f_and filterlist = `And filterlist
