(* an object oriented interface to ldap

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

(** an object oriented ldap client interface *)

open Ldap_types

type op = string * string list
type op_lst = op list
type referral_policy = [ `FOLLOW | `RETURN ]
type changetype = [ `ADD | `DELETE | `MODDN | `MODIFY | `MODRDN ]

class type ldapentry_t =
  object
    method add : op_lst -> unit
    method attributes : string list
    method changes : (Ldap_types.modify_optype * string * string list) list
    method changetype : changetype
    method delete : op_lst -> unit
    method dn : string
    method exists : string -> bool
    method flush_changes : unit
    method get_value : string -> string list
    method modify :
      (Ldap_types.modify_optype * string * string list) list -> unit
    method print : unit
    method replace : op_lst -> unit
    method set_changetype : changetype -> unit
    method set_dn : string -> unit
  end

(** this object represents a remote object within local memory. It
  records all local changes made to it (if it's changetype is set to
  `MODIFY), and can commit them to the server at a later time. This
  can significantly improve performance by reducing traffic to the
  server. *)
class ldapentry :
  object
    (** add values to an attribute (or create a new attribute). Does
      not change the server until you update *)
    method add : op_lst -> unit

    (** return a list of the type (name) of all the attributes present
    on the object *)
    method attributes : string list

    (** return a list of changes made to the object in a format suitable for
      sending directly to modify_s *)
    method changes : (Ldap_types.modify_optype * string * string list) list

    (** return the changetype of the object *)
    method changetype : changetype

    (** delete attributes from the object, does not change the
    directory until you update *)
    method delete : op_lst -> unit

    (** return the dn of the object *)
    method dn : string

    (** query whether the attribute type (name) exists in the object *)
    method exists : string -> bool

    (** clear all accumulated changes *)
    method flush_changes : unit

    (** get the value of an attribute *)
    method get_value : string -> string list

    (** modify the object (same as modify_s), does not change the
    database until you update *)
    method modify :
      (Ldap_types.modify_optype * string * string list) list -> unit

    (** print an ldif like representation of the object to stdout, see
      Ldif_oo for standards compliant ldif. Usefull for toplevel
      sessions. *)
    method print : unit

    (** replace values in the object, does not change the database
    until you call update *)
    method replace : op_lst -> unit

    (** set the changetype of the object *)
    method set_changetype : changetype -> unit

    (** set the dn of the object *)
    method set_dn : string -> unit
  end

(** given a search_result_entry as returned by ldap_funclient, produce an
    ldapentry containing either the entry, or the referral object *)
val to_entry : 
  [< `Entry of Ldap_types.search_result_entry | `Referral of string list ] 
  -> ldapentry

(** given an ldapentry as returned by ldapcon, or constructed manually,
    produce a search_result_entry suitable for ldap_funclient, or
    ldap_funserver. *)
val of_entry : ldapentry -> search_result_entry

(** given a source of ldapentry objects (unit -> ldapentry), such as
    the return value of ldapcon#search_a apply f (first arg) to each entry
    See List.iter *)
val iter : (ldapentry -> unit) -> (?abandon:bool -> unit -> ldapentry) -> unit

(** given a source of ldapentry objects (unit -> ldapentry), such as
  the return value of ldapcon#search_a apply f (first arg) to each
  entry in reverse, and return a list containing the result of each
  application. See List.map *)
val rev_map : (ldapentry -> 'a) -> (?abandon:bool -> unit -> ldapentry) -> 'a list

(** same as rev_map, but does it in order *)
val map : (ldapentry -> 'a) -> (?abandon:bool -> unit -> ldapentry) -> 'a list

(** given a source of ldapentry objects (unit -> ldapentry), such as
  the return value of ldapcon#search_a compute (f eN ... (f e2 (f e1
  intial))) see List.fold_right. *)
val fold : (ldapentry -> 'a -> 'a) -> 'a -> (?abandon:bool -> unit -> ldapentry) -> 'a

class ldapcon :
  ?referral_policy:[> `RETURN ] ->
  ?version:int ->
  string list ->
  object
    method add : ldapentry -> unit
    method bind :
      ?cred:string -> ?meth:Ldap_funclient.authmethod -> string -> unit
    method delete : string -> unit
    method modify :
      string ->
      (Ldap_types.modify_optype * string * string list) list -> unit
    method modrdn : string -> ?deleteoldrdn:bool -> string -> unit
    method rawschema : ldapentry
    method schema : Ldap_schemaparser.schema
    method search :
      ?scope:Ldap_types.search_scope ->
      ?attrs:string list ->
      ?attrsonly:bool -> ?base:string -> string -> ldapentry list
    method search_a :
      ?scope:Ldap_types.search_scope ->
      ?attrs:string list ->
      ?attrsonly:bool -> ?base:string -> string -> (?abandon:bool -> unit -> ldapentry)
    method unbind : unit
    method update_entry : ldapentry -> unit
  end

module OrdStr :
  sig
    type t = Ldap_schemaparser.Oid.t
    val compare : Ldap_schemaparser.Oid.t -> Ldap_schemaparser.Oid.t -> int
  end

module Setstr :
  sig
    type elt = OrdStr.t
    type t = Set.Make(OrdStr).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end

type scflavor = Optimistic | Pessimistic

(** given a name of an attribute, return its oid *)
val attrToOid :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.Oid.t

(** given the oid of an attribute, return its canonical name *)
val oidToAttr : Ldap_schemaparser.schema -> Ldap_schemaparser.Oid.t -> string

(** given a name of an objectclass, return its oid *)
val ocToOid :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.Oid.t

(** given the oid of an objectclass, return its canonical name *)
val oidToOc : Ldap_schemaparser.schema -> Ldap_schemaparser.Oid.t -> string

(** get an objectclass structure by one of its names *)
val getOc :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.objectclass

(** get an attr structure by one of its names *)
val getAttr :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.attribute

(** equate attributes by oid. This allows aliases to be handled
  correctly, for example "uid" and "userID" are actually the same
  attribute. *)
val equateAttrs :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.Lcstring.t -> bool
 
exception Invalid_objectclass of string
exception Invalid_attribute of string
exception Single_value of string
exception Objectclass_is_required

class scldapentry :
  Ldap_schemaparser.schema ->
  object
    method add : op_lst -> unit
    method attributes : string list
    method changes : (Ldap_types.modify_optype * string * string list) list
    method changetype : changetype
    method delete : op_lst -> unit
    method dn : string
    method exists : string -> bool
    method flush_changes : unit
    method get_value : string -> string list
    method is_allowed : string -> bool
    method is_missing : string -> bool
    method list_allowed : Setstr.elt list
    method list_missing : Setstr.elt list
    method list_present : Setstr.elt list
    method modify :
      (Ldap_types.modify_optype * string * string list) list -> unit
    method of_entry : ldapentry -> unit
    method print : unit
    method replace : op_lst -> unit
    method set_changetype : changetype -> unit
    method set_dn : string -> unit
  end

type generator = {
  gen_name : string;
  required : string list;
  genfun : ldapentry_t -> string list;
}

type service = {
  svc_name : string;
  static_attrs : (string * string list) list;
  generate_attrs : string list;
  depends : string list;
}
type generation_error =
    Missing_required of string list
  | Generator_error of string

exception No_generator of string
exception Generation_failed of generation_error
exception No_service of string
exception Service_dep_unsatisfiable of string
exception Generator_dep_unsatisfiable of string * string
exception Cannot_sort_dependancies of string list

class ldapaccount :
  Ldap_schemaparser.schema ->
  (string, generator) Hashtbl.t ->
  (string, service) Hashtbl.t ->
  object
    method adapt_service : service -> service
    method add : op_lst -> unit
    method add_generate : string -> unit
    method add_service : string -> unit
    method attributes : string list
    method changes : (Ldap_types.modify_optype * string * string list) list
    method changetype : changetype
    method delete : op_lst -> unit
    method delete_generate : string -> unit
    method delete_service : string -> unit
    method dn : string
    method exists : string -> bool
    method flush_changes : unit
    method generate : unit
    method get_value : string -> string list
    method is_allowed : string -> bool
    method is_missing : string -> bool
    method list_allowed : Setstr.elt list
    method list_missing : Setstr.elt list
    method list_present : Setstr.elt list
    method modify :
      (Ldap_types.modify_optype * string * string list) list -> unit
    method of_entry : ldapentry -> unit
    method print : unit
    method replace : op_lst -> unit
    method service_exists : string -> bool
    method services_present : string list
    method set_changetype : changetype -> unit
    method set_dn : string -> unit
  end
