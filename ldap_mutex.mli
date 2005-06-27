(* A library for implementing mutexes on top of LDAP's built in test
   and set mechanism

   Copyright (C) 2005 Eric Stokes, and The California State University
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

(** A library for implementing mutexes on top of LDAP's built in test
    and set mechanism *)

(** raised when a mutex operation fails. The string argument contains
    the name of the method which failed, and the exception contains
    details about what failed. *)
exception Ldap_mutex of string * exn

class type mutex_t =
object
  method lock: unit
  method unlock: unit
end

(**  new mutex ldapurls binddn bindpw mutexdn *)
class mutex: string list -> string -> string -> string ->
object
  method lock: unit
  method unlock: unit
end
