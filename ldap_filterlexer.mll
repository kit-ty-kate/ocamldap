(* a lexer for rfc2254 human readable search filters

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

{
  open Ldap_filterparser
}

let whsp = [ ' '  '\t' ] +
let lparen = '('
let rparen = ')'
let andop = '&'
let orop = '|'
let notop = '!'
let equalop = '='
let approxop = '~' equalop
let gteop = '>' equalop
let lteop = '<' equalop
let star = '*'
let attr = [ '0' - '9' 'a' - 'z' 'A' - 'Z' ] +
let hexdigit = [ '0' - '9' 'a' - 'f' 'A' - 'F' ]
let escape = '\\' hexdigit hexdigit
let value = escape | ( [ '\t' ' ' '!' - '~' ] # [ '(' ')' '&' '|' '=' '~' '>' '<' '*' ] )
let valuelst = value +
let colon = ':'
let oid = ( [ '0' - '9' '.' ] + as oid) colon
let dn = colon "dn" ( colon ) ?
let matchingrule = colon oid
let extendedmatchattr = (attr as attrname) matchingrule
let extendeddnattr = (attr as attrname) dn (oid)?

rule lexfilter = parse
    whsp {WHSP}
  | lparen {LPAREN}
  | rparen {RPAREN}
  | andop {AND}
  | orop {OR}
  | notop {NOT}
  | equalop {EQUAL}
  | approxop {APPROX}
  | gteop {GTE}
  | lteop {LTE}
  | star {STAR}
  | attr {ATTR (Lexing.lexeme lexbuf)}
  | extendedmatchattr {EXTENDEDMATCHATTR (attrname, oid)}
  | extendeddnattr {EXTENDEDDNATTR (attrname, oid)}
  | valuelst {VALUE (Lexing.lexeme lexbuf)}
  | eof {EOF}
