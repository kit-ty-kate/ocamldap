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

open Ldap_types
open Ldap_filterparser
open Ldap_filterlexer
open Str

(* escape a string to be put in a string representation of a search
   filter *)
let star_rex = Pcre.regexp ~study:true "\\*"
let lparen_rex = Pcre.regexp ~study:true "\\("
let rparen_rex = Pcre.regexp ~study:true "\\)"
let backslash_rex = Pcre.regexp ~study:true "\\Q\\\\E"
let null_rex = Pcre.regexp ~study:true "\\000"
let escape_filterstring s = 
  (Pcre.qreplace ~rex:star_rex ~templ:"\\2a"
     (Pcre.qreplace ~rex:lparen_rex ~templ:"\\28"
	(Pcre.qreplace ~rex:rparen_rex ~templ:"\\29"
	   (Pcre.qreplace ~rex:null_rex ~templ:"\\00"
	      (Pcre.qreplace ~rex:backslash_rex ~templ:"\\5c" s)))))

let of_string f = 
  let lxbuf = Lexing.from_string f in
    filter lexfilter lxbuf

let double_star_rex = regexp "\\*\\*"
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
	  Buffer.add_string buf (escape_filterstring valu);
	  Buffer.add_char buf ')'
      | `Substrings {attrtype=attrname;
		     substrings={substr_initial=initial;
				 substr_any=any;
				 substr_final=final}} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_char buf '=';
	  Buffer.add_string buf
	    (global_replace double_star_rex "*"
	       ((match initial with
		     Some s -> (escape_filterstring s) ^ "*"
		   | None -> "") ^
		  (match any with
		       Some s -> "*" ^ (escape_filterstring s) ^ "*"
		     | None -> "") ^
		     (match final with
			  Some s -> "*" ^ (escape_filterstring s)
			| None -> "")));
	  Buffer.add_char buf ')';
      | `GreaterOrEqual {attributeDesc=attrname;assertionValue=valu} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_string buf ">=";
	  Buffer.add_string buf (escape_filterstring valu);
	  Buffer.add_char buf ')'
      | `LessOrEqual {attributeDesc=attrname;assertionValue=valu} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_string buf "<=";
	  Buffer.add_string buf (escape_filterstring valu);
	  Buffer.add_char buf ')'
      | `ApproxMatch {attributeDesc=attrname;assertionValue=valu} ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attrname;
	  Buffer.add_string buf "~=";
	  Buffer.add_string buf (escape_filterstring valu);
	  Buffer.add_char buf ')'	  
      | `Present attr ->
	  Buffer.add_char buf '(';
	  Buffer.add_string buf attr;
	  Buffer.add_string buf "=*";
	  Buffer.add_char buf ')'
      | `ExtensibleMatch {matchingRule=rul;ruletype=rtype;
			  matchValue=matchval;dnAttributes=dnattrs} ->
	  Buffer.add_char buf '(';
	  (match rtype with
	       Some attrname -> 
		 Buffer.add_string buf attrname;
		 (if dnattrs then
		    Buffer.add_string buf ":dn");
		 (match rul with
		      Some r ->
			Buffer.add_char buf ':';
			Buffer.add_string buf r
		    | None -> ());
		 Buffer.add_string buf ":=";
		 Buffer.add_string buf (escape_filterstring matchval)
	     | None ->
		 ((if dnattrs then
		     Buffer.add_string buf ":dn");
		  (match rul with
		       Some r -> 
			 Buffer.add_char buf ':';
			 Buffer.add_string buf r;
			 Buffer.add_string buf ":=";
			 Buffer.add_string buf (escape_filterstring matchval)
		     | None -> failwith "matchingRule is required if type is unspecified")));
	  Buffer.add_char buf ')'
  in
  let buf = Buffer.create 100 in
    to_string' buf f;
    Buffer.contents buf

let f_not filterlist = `Not filterlist
let f_or filterlist = `Or filterlist
let f_and filterlist = `And filterlist
