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
%}

%token WHSP LPAREN RPAREN AND OR NOT EQUAL APPROX GTE LTE STAR EOF
%token <string> ATTR
%token <string * string> EXTENDEDMATCHATTR
%token <string * string option> EXTENDEDDNATTR
%token <string> VALUE
%start filter
%type <Ldap_types.filter> filter
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
| ATTR EQUAL extvalue {`EqualityMatch {attributeDesc=$1;assertionValue=$3}}
| ATTR EQUAL STAR extvalue {`Substrings {attrtype=$1;
					 substrings={substr_initial=None;
						    substr_any=None;
						    substr_final=(Some $4)}}}
| ATTR EQUAL extvalue STAR {`Substrings {attrtype=$1;
					 substrings={substr_initial=(Some $3);
						     substr_any=None;
						     substr_final=None}}}
| ATTR EQUAL STAR extvalue STAR {`Substrings {attrtype=$1;
					      substrings={substr_initial=None;
							  substr_any=(Some $4);
							  substr_final=None}}}
| ATTR EQUAL extvalue STAR extvalue STAR extvalue {`Substrings
						     {attrtype=$1;
						      substrings=
							{substr_initial=(Some $3);
							 substr_any=(Some $5);
							 substr_final=(Some $7)}}}
| ATTR EQUAL extvalue STAR extvalue {`Substrings
				       {attrtype=$1;
					substrings=
					  {substr_initial=(Some $3);
					   substr_any=None;
					   substr_final=(Some $5)}}}
| EXTENDEDMATCHATTR EQUAL extvalue {`ExtensibleMatch {matchingRule=(Some (snd $1));
						      ruletype=(Some (fst $1));
						      matchValue=$3;
						      dnAttributes=false}}
| EXTENDEDDNATTR EQUAL extvalue {`ExtensibleMatch {matchingRule=(snd $1);
						   ruletype=(Some (fst $1));
						   matchValue=$3;
						   dnAttributes=true}}
| ATTR APPROX extvalue {`ApproxMatch {attributeDesc=$1;assertionValue=$3}}
| ATTR GTE extvalue {`GreaterOrEqual {attributeDesc=$1;assertionValue=$3}}
| ATTR LTE extvalue {`LessOrEqual {attributeDesc=$1;assertionValue=$3}}
| ATTR EQUAL STAR {`Present $1}
