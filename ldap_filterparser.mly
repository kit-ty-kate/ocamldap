/* a parser for rfc2254 ldap filters

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
*/


%{
  open Ldap_filterlexer
  open Ldap_types

  let star_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\2a")
  let lparen_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\28")
  let rparen_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\29")
  let backslash_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\5c")
  let null_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\00")
  let unescape s =
    (Pcre.qreplace ~rex:star_escape_rex ~templ:"*"
       (Pcre.qreplace ~rex:lparen_escape_rex ~templ:"("
	  (Pcre.qreplace ~rex:rparen_escape_rex ~templ:")"
	     (Pcre.qreplace ~rex:null_escape_rex ~templ:"\000"
		(Pcre.qreplace ~rex:backslash_escape_rex ~templ:"\\" s)))))  

%}

%token WHSP LPAREN RPAREN AND OR NOT EQUAL APPROX GTE LTE STAR EOF
%token <string> ATTR
%token <string * string> EXTENDEDMATCHATTR
%token <string * string option> EXTENDEDDNATTR
%token <string> VALUE
%start filter_and_eof
%type <Ldap_types.filter> filter_and_eof
%%

filterlist:
  filterlist filter {$2 :: $1}
| filter {[$1]}
;

extvalue:
  ATTR {$1}
| VALUE {$1}
;
filter:
  LPAREN AND filterlist RPAREN {`And $3}
| LPAREN OR filterlist RPAREN {`Or $3}
| LPAREN NOT filter RPAREN {`Not $3}
| LPAREN filter RPAREN {$2}
| ATTR EQUAL extvalue {`EqualityMatch {attributeDesc=$1;assertionValue=(unescape $3)}}
| ATTR EQUAL STAR extvalue {`Substrings 
			      {attrtype=$1;
			       substrings={substr_initial=[];
					   substr_any=[];
					   substr_final=[unescape $4]}}}
| ATTR EQUAL extvalue STAR {`Substrings 
			      {attrtype=$1;
			       substrings={substr_initial=[unescape $3];
					   substr_any=[];
					   substr_final=[]}}}
| ATTR EQUAL STAR extvalue STAR {`Substrings
				   {attrtype=$1;
				    substrings={substr_initial=[];
						substr_any=[unescape $4];
						substr_final=[]}}}
| ATTR EQUAL extvalue STAR extvalue STAR extvalue {`Substrings
						     {attrtype=$1;
						      substrings=
							 {substr_initial=[unescape $3];
							 substr_any=[unescape $5];
							 substr_final=[unescape $7]}}}
| ATTR EQUAL extvalue STAR extvalue {`Substrings
				       {attrtype=$1;
					substrings=
					   {substr_initial=[unescape $3];
					    substr_any=[];
					    substr_final=[unescape $5]}}}
| EXTENDEDMATCHATTR EQUAL extvalue {`ExtensibleMatch 
				      {matchingRule=(Some (unescape (snd $1)));
				       ruletype=(Some (unescape (fst $1)));
				       matchValue=(unescape $3);
				       dnAttributes=false}}
| EXTENDEDDNATTR EQUAL extvalue {`ExtensibleMatch {matchingRule=(match (snd $1) with
								     Some s -> Some (unescape s)
								   | None -> None);
						   ruletype=(Some (unescape (fst $1)));
						   matchValue=(unescape $3);
						   dnAttributes=true}}
| ATTR APPROX extvalue {`ApproxMatch {attributeDesc=$1;assertionValue=(unescape $3)}}
| ATTR GTE extvalue {`GreaterOrEqual {attributeDesc=$1;assertionValue=(unescape $3)}}
| ATTR LTE extvalue {`LessOrEqual {attributeDesc=$1;assertionValue=(unescape $3)}}
| ATTR EQUAL STAR {`Present $1}

/* used to enforce EOF at the end of the filter */
filter_and_eof:
  filter EOF {$1}
;
