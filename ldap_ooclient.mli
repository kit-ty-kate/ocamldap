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
    method diff : ldapentry_t -> (modify_optype * string * string list) list
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

val format_entry :
    < attributes : string list; dn : string;
      get_value : string -> string list; .. > ->
    unit

val format_entries :
    < attributes : string list; dn : string;
      get_value : string -> string list; .. > list ->
    unit

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

    (** given an ldapentry, return the differences between the current
	entry and the specified entry *)
    method diff : ldapentry_t -> (modify_optype * string * string list) list

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
      sessions. DEPRECATED. *)
    method print : unit

    (** replace values in the object, does not change the database
    until you call update *)
    method replace : op_lst -> unit

    (** set the changetype of the object *)
    method set_changetype : changetype -> unit

    (** set the dn of the object *)
    method set_dn : string -> unit
  end

type changerec = 
    [`Modification of string * ((Ldap_types.modify_optype * string * string list) list)
    | `Addition of ldapentry
    | `Delete of string
    | `Modrdn of string * int * string]

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
  ?connect_timeout:int ->
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
    method modrdn : string -> ?deleteoldrdn:bool -> ?newsup:string option -> string -> unit
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

(** functions for implementing mutexes on top of LDAP's built in test
    and set mechanism. In order to use this module you must load
    mutex.schema, which is an rfc2252 format schema file.  raised when
    a mutex operation fails. The string argument contains the name of
    the method which failed, and the exception contains details about
    what failed. *)
exception Ldap_mutex of string * exn

(** the class type of a single mutex, used for performing
    advisory locking of some action *)
class type mutex_t =
object
  method lock: unit
  method unlock: unit
end

(** the class type of an object lock table which allows for advisory
    locking of objects by dn *)
class type object_lock_table_t =
object
  method lock: Ldap_types.dn -> unit
  method unlock: Ldap_types.dn -> unit
end

(**  new mutex ldapurls binddn bindpw mutexdn *)
class mutex: string list -> string -> string -> string ->
object
  (** lock the mutex. This WILL block if the mutex is already locked *)
  method lock: unit
  (** unlock the mutex *)
  method unlock: unit
end

(** used to apply some function, first locking the mutex, unlocking it
    only after the function has been applied. If the function
    generates any exception, this wrapper catches that exception, and
    unlocks the mutex before reraising the exception. Generally
    garentees that the mutex will always be used consistantly when
    performing an action. *)
val apply_with_mutex: mutex -> (unit -> 'a) -> 'a

(** new object_lock_table ldapurls binddn bindpw mutexdn *)
class object_lock_table: string list -> string -> string -> string ->
object
  (** lock the specified dn, if it is already locked, then block until the lock can be aquired *)
  method lock: Ldap_types.dn -> unit
  (** unlock the specified dn, if it is not locked do nothing *)
  method unlock: Ldap_types.dn -> unit
end

(** the abstract type of a transaction *)
type txn

(** raised when a commit fails, contains a list of entries which were
    not rolled back successfully only if rollback failed as well,
    otherwise None *)
exception Txn_commit_failure of string * exn * ldapentry_t list option

(** raised when an explicit rollback fails *)
exception Txn_rollback_failure of string * exn

(** A subclass of ldapcon which implements an experimental interface
    to draft_zeilenga_ldap_txn. A draft standard for multi object
    transactions over the ldap protocol. This class can only implement
    advisory transactions because it must depend on the advisory
    locking mechanisms for the transactions to be consistant. You use
    this class by calling begin_txn to get a transaction id, and then
    associating a set of ldapentry objects with the transaction by
    calling associate_entry_with_txn. You are then free to modify
    those entries in any way you like, and when you are done, you can
    either call commit_txn, or rollback_txn. Commit will commit the
    changes of all the entries associated with the transaction to the
    database. For other writers which obey advisory locking the commit
    operation is atomic. For readers which are willing to obey
    advisory locking is atomic. If the commit fails, a full rollback
    occurrs, including all changes made to the directory. For example
    in a set of N entries in a transaction, if the modificiation of
    the nth entry fails to commit, then the modifications to all the
    previous entries, which have already been made in the directory,
    are undone. It is important to note that if advisory locking is
    not obeyed, rollback may not be successful. Rollback undoes all
    the changes you've made in memory, and unlocks all the objects in
    the transaction. After a transaction object has been commited or
    rolled back it is considered "dead", and cannot be used again. *)
class ldapadvisorytxcon :
  ?connect_timeout:int ->
  ?referral_policy:[> `RETURN ] ->
  ?version:int ->
  string list -> string -> string -> string -> (* hosts binddn bindpw mutextbldn *)
  object
    method add : ldapentry -> unit
    method bind :
      ?cred:string -> ?meth:Ldap_funclient.authmethod -> string -> unit
    method delete : string -> unit
    method modify :
      string ->
      (Ldap_types.modify_optype * string * string list) list -> unit
    method modrdn : string -> ?deleteoldrdn:bool -> ?newsup:string option -> string -> unit
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
    method begin_txn : txn
    method associate_entry : txn -> ldapentry_t -> unit
    method associate_entries : txn -> ldapentry_t list -> unit
    method disassociate_entry : txn -> ldapentry_t -> unit
    method disassociate_entries : txn -> ldapentry_t list -> unit
    method commit_txn : txn -> unit
    method rollback_txn : txn -> unit
  end

module OrdOid :
sig
  type t = Ldap_schemaparser.Oid.t
  val compare : t -> t -> int
end

module Setstr :
  sig
    type elt = OrdOid.t
    type t = Set.Make(OrdOid).t
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
    method diff : ldapentry_t -> (Ldap_types.modify_optype * string * string list) list
    method is_allowed : string -> bool
    method is_missing : string -> bool
    method list_allowed : Setstr.elt list
    method list_missing : Setstr.elt list
    method list_present : Setstr.elt list
    method modify :
      (Ldap_types.modify_optype * string * string list) list -> unit
    method of_entry : ?scflavor:scflavor -> ldapentry -> unit
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
    method diff : ldapentry_t -> (Ldap_types.modify_optype * string * string list) list
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
    method of_entry : ?scflavor:scflavor -> ldapentry -> unit
    method print : unit
    method replace : op_lst -> unit
    method service_exists : string -> bool
    method services_present : string list
    method set_changetype : changetype -> unit
    method set_dn : string -> unit
  end
