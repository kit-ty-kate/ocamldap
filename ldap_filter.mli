(** operations on ldap search filters

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

(** raised when something goes wrong in to_string or of_string. The
    integer argument is, in the case of of_string, the position in the
    string at which the error occurred. It has no meaning in to_string,
    and may take any value. *)
exception Invalid_filter of int * string

(** turn the string representation into the internal representation
    defined in ldap_types.ml. This representation is suitable for
    sending on the wire, and can also have all sorts of operations
    performed on it.  play around with it in the toplevel to get a feel
    for it *)
val of_string : string -> Ldap_types.filter

(** turn an internal representaion of a filter into a string
    representaion compliant with rfc2254*)
val to_string : Ldap_types.filter -> string
