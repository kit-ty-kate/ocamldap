(* A lexer and parser for ldif format files 

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)


open Stream;;
open Netencoding;;

exception Illegal_char of char * int;;
exception End;;

type attribute   = string * string;;
type ldif_record = {dn: string; attrs: attribute list};;
type stream_rec  = {stream: char Stream.t;buf:Buffer.t;mutable line: int};;

let optval o =
  match o with
      Some(c) -> c
    | None -> raise End;;

let rec read_comment s =  
  let check_next s = 
    match (optval (peek s.stream)) with
	' ' | '#' -> (junk s.stream);read_comment s (* line folded, or another comment *)
      |  _ -> ()
  in 
    match (optval (peek s.stream)) with
	'\n' -> (junk s.stream);s.line <- s.line + 1;check_next s
      | '\r' -> (junk s.stream);(junk s.stream);s.line <- s.line + 1;check_next s
      |  _   -> (junk s.stream);read_comment s
	   
let comment s =
  match (optval (peek s.stream)) with
      '#' -> (junk s.stream);read_comment s
    |  _  -> ()

let sep s = 
  match (optval (peek s.stream)) with
      '\n' -> (junk s.stream);s.line <- s.line + 1;"\n"
    | '\r' -> (junk s.stream);(junk s.stream);s.line <- s.line + 1;"\n"
    |  c   -> raise (Illegal_char (c,s.line));;

let seps s = 
  try
    (while true
     do
       ignore (sep s)
     done)
  with Illegal_char(_,_) -> ();;

let digit s =
  match (optval (peek s.stream)) with
      '0'..'9' -> (next s.stream)
    | c -> raise (Illegal_char (c,s.line));;

let safe_char s =
  match (optval (peek s.stream)) with
      ' '..'~' -> (next s.stream)
    | c -> raise (Illegal_char (c,s.line));;

let safe_init_char s =
  match (optval (peek s.stream)) with
      '!'..'9'|';'..'~'  -> (next s.stream)
    | c -> raise (Illegal_char (c,s.line));;

let alpha s =
  match (optval (peek s.stream)) with
      'a'..'z'|'A'..'Z' -> (next s.stream)
    | c -> raise (Illegal_char (c,s.line));;

let safe_chars s =
  let rec do_safe_chars s =
    try 
      while true
      do
	Buffer.add_char s.buf (safe_char s)
      done
    with 
	Illegal_char('\n',_) -> 
	  (match (npeek 2 s.stream) with
	       ['\n';' '] -> 
		 (junk s.stream);(junk s.stream);
		 s.line <- s.line + 1;
		 (do_safe_chars s)
	     | _ -> ())
      | Illegal_char('\r',_) -> 
	  (match (npeek 3 s.stream) with
	       ['\r';'\n';' '] -> 
		 (junk s.stream);(junk s.stream);(junk s.stream);
		 s.line <- s.line + 1;
		 (do_safe_chars s)
	     | _ -> ())
      | Illegal_char(_,_) -> ()
      | End -> ()
  in
    do_safe_chars s;;

let safe_string s =
  Buffer.clear s.buf;
  Buffer.add_char s.buf (safe_init_char s);
  safe_chars s;
  Buffer.contents s.buf;;

(*let ldap_oid s =;; *)

let attr_type_char s =
  match (optval (peek s.stream)) with
      'A'..'Z'|'a'..'z'|'0'..'9'|'-' -> (next s.stream)
    | c -> raise (Illegal_char (c, s.line));;

let attr_type_chars s =
  try
    while true
    do
      Buffer.add_char s.buf (attr_type_char s)
    done; 
  with Illegal_char(_,_) -> ()


let option s =
  Buffer.clear s.buf;
  Buffer.add_char s.buf (attr_type_char s);
  attr_type_chars s;
  Buffer.contents s.buf;;

let rec options s =
  match (optval (peek s.stream)) with
      ';' -> let thisone = (junk s.stream);(option s) in thisone ^ (options s)
    | ':' -> ""
    |  c  -> raise (Illegal_char (c, s.line));; (* syntax error *)

let attributeType s =
  Buffer.clear s.buf;
  Buffer.add_char s.buf (alpha s);
  attr_type_chars s;
  Buffer.contents s.buf;;

let attributeDescription s =
  let name = (attributeType s) in
  let options = (match (optval (peek s.stream)) with
		     ';' -> options s (* there are options *)
		   |  _  -> "") in
  let colon = (match (optval (peek s.stream)) with
		   ':' -> (junk s.stream);""
		 |  _  -> failwith "Parse, error. Missing colon in attribute spec")
  in
    name;;

let value_spec s =
  match (optval (peek s.stream)) with
      ':' -> (junk s.stream);
        (match (optval (peek s.stream)) with
	     ' ' -> (junk s.stream);
               (Base64.decode (safe_string s))
	   |  c  -> raise (Illegal_char (c, s.line)))
    | '<' -> (junk s.stream);(match (optval (peek s.stream)) with
				  ' ' -> (junk s.stream);(safe_string s) (* a url *)
				|  c  -> raise (Illegal_char (c, s.line)))
    | ' ' -> (junk s.stream);(safe_string s)
    |  c  -> raise (Illegal_char (c, s.line));;

let rec attrval_spec s =
  try
    ignore (sep s);[]
  with 
      Illegal_char(_,_) ->
	let attr = (attributeDescription s) in
	let valu = (value_spec s) in
	let sep  = (sep s) in
	  (attr, valu) :: (attrval_spec s)
    | End -> [] (* its ok to hit end of file *)

let distinguishedName s =
  match (optval (peek s.stream)) with
      ':' -> (junk s.stream);
        (match (optval (peek s.stream)) with
	     ' ' -> (junk s.stream);
               (Base64.decode (safe_string s)) 
	   |  c  -> raise (Illegal_char (c, s.line)))
    | ' ' -> (junk s.stream);safe_string s
    |  c  -> raise (Illegal_char (c, s.line));;

let dn_spec s =
  match (npeek 3 s.stream) with
      ['d';'n';':'] -> (junk s.stream);(junk s.stream);(junk s.stream);
	(distinguishedName s)
    | _ -> failwith ("invalid dn on line: " ^ (string_of_int s.line));;

let ldif_attrval_record s =
  let _  = comment s in
  let _  = seps s in
  let dn = dn_spec s in
  let _  = try seps s with End -> () in (* just a dn is a valid ldif file *)
  let attrs = attrval_spec s in
    {dn=dn;attrs=attrs};;    
