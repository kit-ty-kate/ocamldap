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
val readSchema : string list -> string list -> schema
