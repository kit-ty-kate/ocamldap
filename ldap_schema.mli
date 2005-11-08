(** A library for parsing rfc2252 schemas as returned by directory
    servers *)

module Oid :
sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
end

module Oidset :
sig
  type elt = Oid.t
  type t = Set.Make(Oid).t
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

module Oidmap :
sig
  type key = Oid.t
  type 'a t = 'a Map.Make(Oid).t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

val format_oid : Oid.t -> unit

module Lcstring :
sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
end

val format_lcstring : Lcstring.t -> unit

type octype = Abstract | Structural | Auxiliary

(** The type representing an objectclass definition *)
type objectclass = {
  oc_name : string list;
  oc_oid : Oid.t;
  oc_desc : string;
  oc_obsolete : bool;
  oc_sup : Lcstring.t list;
  oc_must : Lcstring.t list;
  oc_may : Lcstring.t list;
  oc_type : octype;
  oc_xattr : string list;
}

(** The type representing an attribute definition *)
type attribute = {
  at_name:string list;
  at_desc:string;
  at_oid:Oid.t;
  at_equality:Oid.t option;
  at_ordering:Oid.t option;
  at_substr:Oid.t option;
  at_syntax:Oid.t;
  at_length: Int64.t;
  at_obsolete:bool;
  at_single_value:bool;
  at_collective:bool;
  at_no_user_modification:bool;
  at_usage:string;
  at_sup:Lcstring.t list;
  at_xattr:string list
}

(** The type representing the whole schema. Consists of hashtbls
    indexed by two useful keys. For both attributes and objectclasses
    there exists a hashtbl indexed by OID, and one indexed by lower case
    canonical name. There exist functions in Ldap_ooclient to look up
    attributes and objectclasses by non canonical names if that is
    necessary for you to do. see attrToOid, and ocToOid. They will find
    the oid of an attribute or objectclass given any name, not just the
    canonical one. Not that this is somewhat (like several orders of
    magnitude) slower than lookups by canonical name.*)
type schema = {
  objectclasses : (Lcstring.t, objectclass) Hashtbl.t;
  objectclasses_byoid : (Oid.t, objectclass) Hashtbl.t;
  attributes : (Lcstring.t, attribute) Hashtbl.t;
  attributes_byoid : (Oid.t, attribute) Hashtbl.t;
}

(** This reference controls the dept of printing for the schema in the
    toplevel. The default is 10 keys from each table will be printed. OID
    tables are not currently printed. *)
val schema_print_depth : int ref

(** A formatter for the schema, prints the structure, and expands the
    hashtbls to show the keys. The number of keys printed is controled by
    schema_print_depth. *)
val format_schema : schema -> unit

exception Parse_error_oc of Lexing.lexbuf * objectclass * string
exception Parse_error_at of Lexing.lexbuf * attribute * string
exception Syntax_error_oc of Lexing.lexbuf * objectclass * string
exception Syntax_error_at of Lexing.lexbuf * attribute * string

(** readSchema attribute_list objectclass_list, parse the schema into
    a schema type given a list of attribute definition lines, and
    objectclass definition lines. *)
val readSchema : string list -> string list -> schema

(** given a name of an attribute name (canonical or otherwise), return
    its oid @raise Invalid_attribute If the attribute is not found in the schema. *)
val attrToOid :
  schema ->
  Lcstring.t -> Oid.t

(** given the oid of an attribute, return its record @raise
    Invalid_attribute If the attribute is not found in the schema. *)
val oidToAttr : schema -> Oid.t -> attribute

(** given the oid of an attribute, return its canonical name @raise
    Invalid_attribute If the attribute is not found in the schema. *)
val oidToAttrName : schema -> Oid.t -> string

(** given a name of an objectclass (canonical or otherwise), return
    its oid. @raise Invalid_objectclass If the objectclass is not
    found in the schema. *)
val ocToOid :
  schema ->
  Lcstring.t -> Oid.t

(** given the oid of an objectclass, return its canonical name @raise
    Invalid_objectclass If the objectclass is not found in the
    schema. *)
val oidToOc : schema -> Oid.t -> string

(** get an objectclass structure by one of its names (canonical or
    otherwise, however getting it by canonical name is currently much
    faster) @raise Invalid_objectclass If the objectclass is not found
    in the schema. *)
val getOc :
  schema ->
  Lcstring.t -> objectclass

(** get an attr structure by one of its names (canonical or otherwise,
    however getting it by canonical name is currently much faster)
    @raise Invalid_attribute If the attribute is not found in the
    schema. *)
val getAttr :
  schema ->
  Lcstring.t -> attribute

(** equate attributes by oid. This allows non canonical names to be
    handled correctly, for example "uid" and "userID" are actually the
    same attribute. @raise Invalid_attribute If either attribute is
    not found in the schema. *)
val equateAttrs :
  schema ->
  Lcstring.t -> Lcstring.t -> bool
