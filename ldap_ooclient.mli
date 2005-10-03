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

(** {0 Basic Data Types} *)

(** the type of an operation, eg. [("cn", ["foo";"bar"])] *)
type op = string * string list
type op_lst = op list

(** The policy the client should take when it encounteres a
    referral. This is currently not used *)
type referral_policy = [ `FOLLOW | `RETURN ]

(** The change type of an ldapentry. This controls some aspects of
    it's behavior *)
type changetype = [ `ADD | `DELETE | `MODDN | `MODIFY | `MODRDN ]

(** The base type of an ldap entry represented in memory. *)
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
	entry and the specified entry in the form of a modify
	operation which would make the specified entry the same as the
	current entry. *)
    method diff : ldapentry_t -> (modify_optype * string * string list) list

    (** query whether the attribute type (name) exists in the object *)
    method exists : string -> bool

    (** clear all accumulated changes *)
    method flush_changes : unit

    (** get the value of an attribute @raise Not_found If the
	attribute does not exist. *)
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

(** toplevel formatter for ldapentry, prints the whole entry with a
    nice structure. Each attribute is in the correct syntax to be
    copied and pasted into a modify operation. *)
val format_entry :
  < attributes : string list; dn : string;
 get_value : string -> string list; .. > ->
   unit

(** format lists of entries, in this case only print the dn *)
val format_entries :
  < attributes : string list; dn : string;
 get_value : string -> string list; .. > list ->
   unit

(** The type of an ldap change record, used by extended LDIF *)
type changerec = 
    [`Modification of string * ((Ldap_types.modify_optype * string * string list) list)
    | `Addition of ldapentry
    | `Delete of string
    | `Modrdn of string * int * string]

(** {1 Communication With {!Ldap_funclient}} *)

(** given a search_result_entry as returned by ldap_funclient, produce an
    ldapentry containing either the entry, or the referral object *)
val to_entry : 
  [< `Entry of Ldap_types.search_result_entry | `Referral of string list ] 
  -> ldapentry

(** given an ldapentry as returned by ldapcon, or constructed manually,
    produce a search_result_entry suitable for ldap_funclient, or
    ldap_funserver. *)
val of_entry : ldapentry -> search_result_entry

(** {0 Interacting with LDAP Servers} *)

(** This class abstracts a connection to an LDAP server (or servers),
    an instance will be connected to the server you specify and can be
    used to perform operations on that server. [new ldapcon
    ~connect_timeout:5 ~version:3
    ["ldap://first.ldap.server";"ldap://second.ldap.server"]]. 3 is
    the default protcol version. In addition to specifying multiple
    urls if DNS names are given, and those names are bound to multiple
    addresses, then all possible addresses will be tried. eg. [new
    ldapcon ["ldaps://rrldap.csun.edu"]] is equivelant to [new ldapcon
    ["ldap://130.166.1.30";"ldap://130.166.1.31";"ldap://130.166.1.32"]]
    This means that if any host in the rr fails, the ldapcon will
    transparently move on to the next host, and you will never know
    the difference. @param connect_timeout Default [1], an integer
    which specifies how long to wait for any given server in the list
    to respond before trying the next one. After all the servers have
    been tried for [connect_timeout] seconds [LDAP_Failure
    (`SERVER_DOWN, ...)]  will be raised. @param referral_policy In a
    future version of ocamldap this will be used to specify what you
    would like to do in the event of a referral. Currently it does
    nothing and is ignored see
    {!Ldap_ooclient.referral_policy}. @param version The protocol
    version to use, the default is [3], the other recognized value is
    [2]. @raise Ldap_types.LDAP_Failure All methods raise LDAP_Failure
    on error *)
class ldapcon :
  ?connect_timeout:int ->
  ?referral_policy:[> `RETURN ] ->
  ?version:int ->
  string list ->
object
  (** add an entry to the database *)
  method add : ldapentry -> unit

  (** bind to the database [#bind ~cred:"password" dn] using dn. To
      bind anonymously, omit ~cred, and leave dn blank eg. [#bind
      ""]. @param cred The credentials to provide for binding. @param
      meth The method to use when binding See
      {!Ldap_funclient.authmethod} the default is [`SIMPLE]. If [`SASL] is
      used then the [dn] becomes the username, and [~cred] may or may
      not be required. SASL binds have not been tested extensively. *)
  method bind :
    ?cred:string -> ?meth:Ldap_funclient.authmethod -> string -> unit

  (** Delete the object named by dn from the database [#delete dn] *)
  method delete : string -> unit

  (** Modify the entry named by dn, applying mods eg. [#modify dn
      [(`DELETE, "cn", ["foo";"bar"])]] *)
  method modify :
    string ->
    (Ldap_types.modify_optype * string * string list) list -> unit

  (** Modify the rdn of the object named by dn, if the protocol
      version is 3 you may additionally change the superior [#modrdn
      ~deleteoldrdn:true ~newsup:(Some "o=csun") dn newrdn], the rdn
      will be changed to the attribute represented (as a string) by
      newrdn, (simple example ["cn=foo"], more complex example
      ["uid=foo+bar+baz"]). @param deleteoldrdn Default [true], delete
      the old rdn value as part of the modrdn. @param newsup Default
      [None], only valid when the protocol version is 3, change the
      object's location in the tree, making its superior equal to the
      specified object. *)
  method modrdn : string -> ?deleteoldrdn:bool -> ?newsup:string option -> string -> unit

  (** Fetch the raw (unparsed) schema from the directory using the
      standard mechanism (requires protocol version 3) *)
  method rawschema : ldapentry

  (** Fetch and parse the schema from the directory via the standard
      mechanism (requires version 3). Return a structured
      representation of the schema indexed by canonical name, and oid. *)
  method schema : Ldap_schemaparser.schema

  (** Search the directory syncronously for an entry which matches the
      search criteria. [#search ~base ldapfilter], eg. [#search
      ~base:"dc=foo,dc=bar" ~attrs:["cn"] "uid=*"] @param scope
      Default [`SUBTREE], defines the scope of the search. see
      {!Ldap_types.search_scope} @param attrs Default [[]] (means all
      attributes) @param attrsonly Default [false] If true, asks the
      server to return only the attribute names, not their
      values. @param base Default [""], The search base, which is the
      dn of the object from which you want to start your search. Only
      that object, and it's children will be included in the
      search. Further controlled by [~scope]. *)
  method search :
    ?scope:Ldap_types.search_scope ->
    ?attrs:string list ->
    ?attrsonly:bool -> ?base:string -> string -> ldapentry list

  (** Search the directory asyncronously, otherwise the same as
      search. *)
  method search_a :
    ?scope:Ldap_types.search_scope ->
    ?attrs:string list ->
    ?attrsonly:bool -> ?base:string -> string -> (?abandon:bool -> unit -> ldapentry)

  (** Close the connection to the directory *)
  method unbind : unit

  (** Syncronize changes made locally to an ldapentry with the
      directory. *)
  method update_entry : ldapentry -> unit
end

(** given a source of ldapentry objects (unit -> ldapentry), such as
    the return value of ldapcon#search_a, apply f (first arg) to each entry
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

(** {0 Schema Aware {!Ldap_ooclient.ldapentry} Derivatives} *)

(** {1 Supporting Types} *)

(** an ordered oid type, for placing oids in sets *)
module OrdOid :
sig
  type t = Ldap_schemaparser.Oid.t
  val compare : t -> t -> int
end

(** A set of Oids, @deprecated the name is historical, and may be changed *)
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

(** The type of schema checking to perform in
    {!Ldap_ooclient.scldapentry}. *)
type scflavor = 
    Optimistic 
      (** Add missing attributes to make the object consistant, or add
	  objectclasses in order to make illegal attribues legal *)
  | Pessimistic
      (** Delete objectclasses which must attributes which are
	  missing, and delete illegal attributes. *)

(** given a name of an attribute name (canonical or otherwise), return
    its oid @raise Invalid_attribute If the attribute is not found in the schema. *)
val attrToOid :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.Oid.t

(** given the oid of an attribute, return its canonical name @raise
    Invalid_attribute If the attribute is not found in the schema. *)
val oidToAttr : Ldap_schemaparser.schema -> Ldap_schemaparser.Oid.t -> string

(** given a name of an objectclass (canonical or otherwise), return
    its oid. @raise Invalid_objectclass If the objectclass is not
    found in the schema. *)
val ocToOid :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.Oid.t

(** given the oid of an objectclass, return its canonical name @raise
    Invalid_objectclass If the objectclass is not found in the
    schema. *)
val oidToOc : Ldap_schemaparser.schema -> Ldap_schemaparser.Oid.t -> string

(** get an objectclass structure by one of its names (canonical or
    otherwise, however getting it by canonical name is currently much
    faster) @raise Invalid_objectclass If the objectclass is not found
    in the schema. *)
val getOc :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.objectclass

(** get an attr structure by one of its names (canonical or otherwise,
    however getting it by canonical name is currently much faster)
    @raise Invalid_attribute If the attribute is not found in the
    schema. @raise Invalid_objectclass If the objectclass is not found
    in the schema. *)
val getAttr :
  Ldap_schemaparser.schema ->
  Ldap_schemaparser.Lcstring.t -> Ldap_schemaparser.attribute

(** equate attributes by oid. This allows non canonical names to be
    handled correctly, for example "uid" and "userID" are actually the
    same attribute. @raise Invalid_attribute If either attribute is
    not found in the schema. *)
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
