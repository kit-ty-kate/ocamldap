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
  open Ldap_dnlexer
  open Ldap_types
%}

%token Equals Plus Comma End_of_input
%token <string> AttributeType
%token <string> Oid
%token <string> AttributeValue
%type <Ldap_types.dn> dn

attrval:
   AttributeType {$1}
 | AttributeValue {$1}
 | Oid {$1}

attrname:
   AttributeType {$1}
 | Oid {$1}

dn:
   dn Plus attrname Equals attrval {{attr_type=$3;attr_vals=[$5]} :: $1}
 | dn Comma attrname Equals attrval {{attr_type=$3;attr_vals=[$5]} :: $1}
 | attrname Equals attrval {[{attr_type=$1;attr_vals=[$3]}]}
 | End_of_input {[]}
