(** A library for parsing rfc2252 schemas as returned by directory
  servers *)

module Oid :
  sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

module Lcstring :
  sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

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
  at_name : string list;
  at_desc : string;
  at_oid : Oid.t;
  at_equality : string;
  at_ordering : string;
  at_substr : Oid.t;
  at_syntax : Oid.t;
  at_length : int;
  at_obsolete : bool;
  at_single_value : bool;
  at_collective : bool;
  at_no_user_modification : bool;
  at_usage : string;
  at_sup : Lcstring.t list;
  at_xattr : string list;
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
exception Parse_error_oc of Lexing.lexbuf * objectclass * string
exception Parse_error_at of Lexing.lexbuf * attribute * string
exception Syntax_error_oc of Lexing.lexbuf * objectclass * string
exception Syntax_error_at of Lexing.lexbuf * attribute * string

(** readSchema attribute_list objectclass_list, parse the schema into
  a schema type given a list of attribute definition lines, and
  objectclass definition lines. *)
val readSchema : string list -> string list -> schema
