(* create an ldap entry factory from a channel attached to an ldif
   source default is stdin and stdout.

   Copyright (C) 2004 Eric Stokes, Matthew Backes, and The California
   State University at Northridge

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

(** an object oriented interface to the ldif parser *)

(** an exception raised when there is a parse error *)
exception Invalid_changerec of string

(** raised at the end of the change records *)
exception End_of_changerecs

(** The type of a change record, extended ldif *)
type changerec = 
    [`Modification of string * ((Ldap_types.modify_optype * string * string list) list)
    | `Addition of Ldap_ooclient.ldapentry
    | `Delete of string
    | `Modrdn of string * int * string]

(*
(** Ldif_oo.iter f ldif, iterate accross all ldif entries in the
    specified ldif object, applying f to each one *)
val iter : ('a -> unit) -> < read_entry : 'a; .. > -> unit

(** Ldif_oo.fold f ldif value, for each ldif entry en in the ldif
    object fold computes f (... (f (f value e1) e2) ...) en *)
val fold : ('a -> 'b -> 'a) -> < read_entry : 'b; .. > -> 'a -> 'a
*)

class change: 
  ?in_ch:Pervasives.in_channel -> 
  ?out_ch:Pervasives.out_channel -> 
  unit ->
object
  method read_changerec: changerec
  method of_string: string -> changerec
  method to_string: changerec -> string
  method write_changerec: changerec -> unit
end
