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
val format_oidset : Oidset.t -> unit

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
  oc_name: string list;
  oc_oid: Oid.t;
  oc_desc: string;
  oc_obsolete: bool;
  oc_sup: string list;
  oc_must: string list;
  oc_may: string list;
  oc_type: octype;
  oc_xattr: string list
}

(** the type representing an attribute definition *)
type attribute = {
  at_name: string list;
  at_desc: string;
  at_oid: Oid.t;
  at_equality: Oid.t option;
  at_ordering: Oid.t option;
  at_substr: Oid.t option;
  at_syntax: Oid.t;
  at_length:  Int64.t;
  at_obsolete: bool;
  at_single_value: bool;
  at_collective: bool;
  at_no_user_modification: bool;
  at_usage: string;
  at_sup: string list;
  at_xattr: string list
}

type schema = {
  objectclasses: (Lcstring.t, objectclass) Hashtbl.t;
  objectclasses_byoid: (Oid.t, objectclass) Hashtbl.t;
  attributes: (Lcstring.t, attribute) Hashtbl.t;
  attributes_byoid: (Oid.t, attribute) Hashtbl.t
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

(** {1 Schema Access Functions} A set of functions which should be
    used to access the schema. All functions on Oid types are O(1),
    all functions dealing with names are O(1) for canonical names, and
    O(n) for non canonical names. Functions which return names will
    always return the canonical name. *)

exception Invalid_objectclass of string
exception Non_unique_objectclass_alias of string
exception Invalid_attribute of string
exception Non_unique_attribute_alias of string

(** readSchema attribute_list objectclass_list, parse the schema into
    a schema type given a list of attribute definition lines, and
    objectclass definition lines. *)
val readSchema : string list -> string list -> schema

(** given a name of an attribute name (canonical or otherwise), return
    its oid *)
val attrNameToOid : schema -> string -> Oid.t

(** given the oid of an attribute, return its record *)
val oidToAttr : schema -> Oid.t -> attribute

(** given the oid of an attribute, return its canonical name *)
val oidToAttrName : schema -> Oid.t -> string

(** get an attr structure by one of its names (canonical or otherwise) *)
val attrNameToAttr : schema -> string -> attribute

(** get an objectclass structure by one of its names (canonical or
    otherwise). *)
val ocNameToOc : schema -> string -> objectclass

(** given a name of an objectclass (canonical or otherwise), return
    its oid. *)
val ocNameToOid : schema -> string -> Oid.t

(** given the oid of an objectclass, return its objectclass structure  *)
val oidToOc : schema -> Oid.t -> objectclass

(** given the oid of an objectclass, return its canonical name *)
val oidToOcName : schema -> Oid.t -> string

(** compare attributes by oid. *)
val compareAttrs : schema -> string -> string -> int

(** compare objectclasses by oid. *)
val compareOcs : schema -> string -> string -> int

(** lookup matching rules, taking into account the higherarchical
    relationship of attributes *)
val lookupMatchingRule : schema -> [< `Equality | `Ordering | `Substring ] ->
  attribute -> Oid.t option

(** {1 Schema Validation} A function to check for errors in the schema
    and report them *)

type schema_error = 
    Undefined_attr_reference of string
  | Non_unique_attr_alias of string
  | Non_unique_oc_alias of string
  | Undefined_oc_reference of string
  | Cross_linked_oid of string list

val typecheck : schema -> (string * schema_error) list
