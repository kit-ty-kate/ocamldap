open Ldap_types
open Ldap_schema

exception Value_exists
exception Value_does_not_exist
exception Substring_matching_rule_not_defined
exception Ordering_matching_rule_not_defined

(* imperative implementation *)	  
(*
class type attribute_t =
object
  method add: ?idempotent:bool -> string -> unit
  method delete: ?idempotent:bool -> string -> unit
  method replace: string list -> unit
  method exists: string -> bool
  method values: string list
  method cardinal: int

  (* for search filter evaluation *)
  method equality_match: string -> bool
  method substrings_match: substring_component -> bool
  method greater_than_or_equal_match: string -> bool
  method less_than_or_equal_match: string -> bool
  method approximate_match: string -> bool
  method extensible_match: string -> (string -> string -> int) -> bool
end

class ['a] attribute 
  (add: string -> 'a -> 'a) 
  (mem: string -> 'a -> bool)
  (remove: string -> 'a -> 'a)
  (empty: 'a)
  (elements: 'a -> string list)
  (cardinal: 'a -> int)
  (exists: (string -> bool) -> 'a -> bool)
  (ordering: (string -> string -> int) option)
  (substrings: (substring_component -> string -> bool) option)
  (syntax: string -> unit) =
object (self)
  val mutable data = empty
  method add ?(idempotent=false) v =
    syntax v;
    if idempotent then
      data <- add v data
    else if mem v data then
      raise Value_exists
    else
      data <- add v data
  method delete ?(idempotent=false) v =
    if idempotent then
      data <- remove v data
    else if not (mem v data) then
      raise Value_does_not_exist
    else
      data <- remove v data
  method replace vals =
    data <- 
      (List.fold_left
	 (fun s v -> syntax v;add v s)
	 empty
	 vals)
  method exists v = mem v data
  method values = elements data
  method cardinal = cardinal data
  method equality_match v = mem v data
  method substrings_match subs = 
    match substrings with
	Some substrings_rule -> exists (substrings_rule subs) data
      | None -> raise Substring_matching_rule_not_defined
  method private ordering_match i v =
    match ordering with
	Some ordering_rule ->
	  exists 
	    (fun elt -> ordering_rule elt v <> i)
	    data
      | None -> raise Ordering_matching_rule_not_defined
  method greater_than_or_equal_match v = self#ordering_match (-1) v
  method less_than_or_equal_match v = self#ordering_match 1 v
  method approximate_match (v: string) = false
  method extensible_match (v: string) (r: string -> string -> int) = false
end
*)

(* functional implementation *)
class type attribute_t =
object
  method add: ?idempotent:bool -> string -> attribute_t
  method delete: ?idempotent:bool -> string -> attribute_t
  method replace: string list -> attribute_t
  method exists: string -> bool
  method values: string list
  method cardinal: int

  (* for search filter evaluation *)
  method equality_match: string -> bool
  method substrings_match: substring_component -> bool
  method greater_than_or_equal_match: string -> bool
  method less_than_or_equal_match: string -> bool
  method approximate_match: string -> bool
  method extensible_match: string -> (string -> string -> int) -> bool
end

class ['a] attribute 
  (add: string -> 'a -> 'a) 
  (mem: string -> 'a -> bool)
  (remove: string -> 'a -> 'a)
  (empty: 'a)
  (elements: 'a -> string list)
  (cardinal: 'a -> int)
  (exists: (string -> bool) -> 'a -> bool)
  (ordering: (string -> string -> int) option)
  (substrings: (substring_component -> string -> bool) option)
  (syntax: string -> unit) =
object (self)
  val data = empty
  method add ?(idempotent=false) v =
    syntax v;
    if idempotent then {< data = add v data >}
    else if mem v data then raise Value_exists
    else {< data = add v data >}
  method delete ?(idempotent=false) v =
    if idempotent then {< data = remove v data >}
    else if not (mem v data) then raise Value_does_not_exist
    else {< data = remove v data >}
  method replace vals =
    {< data =
	(List.fold_left
	   (fun s v -> syntax v;add v s)
	   empty
	   vals) >}
  method exists v = mem v data
  method values = elements data
  method cardinal = cardinal data
  method equality_match v = mem v data
  method substrings_match subs = 
    match substrings with
	Some substrings_rule -> exists (substrings_rule subs) data
      | None -> raise Substring_matching_rule_not_defined
  method private ordering_match i v =
    match ordering with
	Some ordering_rule ->
	  exists 
	    (fun elt -> ordering_rule elt v <> i)
	    data
      | None -> raise Ordering_matching_rule_not_defined
  method greater_than_or_equal_match v = self#ordering_match (-1) v
  method less_than_or_equal_match v = self#ordering_match 1 v
  method approximate_match (v: string) = false
  method extensible_match (v: string) (r: string -> string -> int) = false
end

(* equality matching rules *)

(* used to normalize whitespace for caseIgnoreMatch and friends *)
exception Break of int
let strip_edge_whitespace s =
  let isspace c = 
    if (c = '\t' || c = '\n' || c = '\011' || c = '\012' || c = '\r' || c = ' ') then true
    else false
  in
  let l = String.length s in
  let first_non_space_char =
    try for i=0 to l - 1 do if not (isspace s.[i]) then raise (Break i) done;l
    with Break i -> i
  in
  let last_non_space_char = 
    try for i=(l - 1) downto 0 do if not (isspace s.[i]) then raise (Break i) done;0
    with Break i -> i
  in
    if first_non_space_char <= last_non_space_char then
      String.sub s first_non_space_char (last_non_space_char - first_non_space_char + 1)
    else ""
      

let whsp = Pcre.regexp ~study:true "\\s+"
let leading_or_trailing_whsp = Pcre.regexp ~study:true "(^\\s+|\\s+$)"
let collapse_whitespace v =
  (Pcre.replace ~rex:leading_or_trailing_whsp ~templ:""
     (Pcre.replace ~rex:whsp ~templ:" " v))
    
let remove_whitespace v = Pcre.replace ~rex:whsp ~templ:"" v

(* 2.5.13.0 NAME 'objectIdentifierMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.38 *)
let object_identifier_equality_match v1 v2 = String.compare v1 v2

module ObjectIdentifierMatch = Set.Make
  (struct
     type t = String.t
     let compare = object_identifier_equality_match
   end)

let new_object_identifier_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     ObjectIdentifierMatch.add ObjectIdentifierMatch.mem
     ObjectIdentifierMatch.remove ObjectIdentifierMatch.empty
     ObjectIdentifierMatch.elements ObjectIdentifierMatch.cardinal
     ObjectIdentifierMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.1 NAME 'distinguishedNameMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.12 *)
let distinguished_name_equality_match v1 v2 = String.compare v1 v2

module DistinguishedNameMatch = Set.Make
  (struct
     type t = String.t
     let compare = distinguished_name_equality_match
   end)

let new_distinguished_name_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     DistinguishedNameMatch.add DistinguishedNameMatch.mem
     DistinguishedNameMatch.remove DistinguishedNameMatch.empty
     DistinguishedNameMatch.elements DistinguishedNameMatch.cardinal
     DistinguishedNameMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.8 NAME 'numericStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.36 *)
let numeric_string_equality_match v1 v2 = 
  String.compare 
    (remove_whitespace v1)
    (remove_whitespace v2)

module NumericStringMatch = Set.Make
  (struct
     type t = String.t
     let compare = numeric_string_equality_match
   end)

let new_numeric_string_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     NumericStringMatch.add NumericStringMatch.mem
     NumericStringMatch.remove NumericStringMatch.empty
     NumericStringMatch.elements NumericStringMatch.cardinal
     NumericStringMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.14 NAME 'integerMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.27 *)
let integer_equality_match v1 v2 = String.compare v1 v2

module IntegerMatch = Set.Make
  (struct
     type t = String.t
     let compare = integer_equality_match
   end)

let new_integer_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     IntegerMatch.add IntegerMatch.mem
     IntegerMatch.remove IntegerMatch.empty
     IntegerMatch.elements IntegerMatch.cardinal
     IntegerMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.16 NAME 'bitStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.6 *)
let bit_string_equality_match v1 v2 = String.compare v1 v2

module BitStringMatch = Set.Make
  (struct
     type t = String.t
     let compare = bit_string_equality_match
   end)

let new_bit_string_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     BitStringMatch.add BitStringMatch.mem
     BitStringMatch.remove BitStringMatch.empty
     BitStringMatch.elements BitStringMatch.cardinal
     BitStringMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.22 NAME 'presentationAddressMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.43 *)
let presentation_address_equality_match v1 v2 = String.compare v1 v2

module PresentationAddressMatch = Set.Make
  (struct
     type t = String.t
     let compare = presentation_address_equality_match
   end)

let new_presentation_address_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     PresentationAddressMatch.add PresentationAddressMatch.mem
     PresentationAddressMatch.remove PresentationAddressMatch.empty
     PresentationAddressMatch.elements PresentationAddressMatch.cardinal
     PresentationAddressMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.23 NAME 'uniqueMemberMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.34 *)
let unique_member_equality_match v1 v2 = String.compare v1 v2

module UniqueMemberMatch = Set.Make
  (struct
     type t = String.t
     let compare = unique_member_equality_match
   end)

let new_unique_member_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     UniqueMemberMatch.add UniqueMemberMatch.mem
     UniqueMemberMatch.remove UniqueMemberMatch.empty
     UniqueMemberMatch.elements UniqueMemberMatch.cardinal
     UniqueMemberMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.24 NAME 'protocolInformationMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.42 *)
let protocol_information_equality_match v1 v2 = String.compare v1 v2

module ProtocolInformationMatch = Set.Make
  (struct
     type t = String.t
     let compare = protocol_information_equality_match
   end)

let new_protocol_information_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     ProtocolInformationMatch.add ProtocolInformationMatch.mem
     ProtocolInformationMatch.remove ProtocolInformationMatch.empty
     ProtocolInformationMatch.elements ProtocolInformationMatch.cardinal
     ProtocolInformationMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.27 NAME 'generalizedTimeMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.24 *)
let generalized_time_equality_match v1 v2 = String.compare v1 v2

module GeneralizedTimeMatch = Set.Make
  (struct
     type t = String.t
     let compare = generalized_time_equality_match
   end)

let new_generalized_time_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     GeneralizedTimeMatch.add GeneralizedTimeMatch.mem
     GeneralizedTimeMatch.remove GeneralizedTimeMatch.empty
     GeneralizedTimeMatch.elements GeneralizedTimeMatch.cardinal
     GeneralizedTimeMatch.exists
     ordering substrings syntax :> attribute_t)
     
(* 2.5.13.2 NAME 'caseIgnoreMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_ignore_equality_match v1 v2 = 
  Lcstring.compare 
    (Lcstring.of_string (collapse_whitespace v1))
    (Lcstring.of_string (collapse_whitespace v2))

module CaseIgnoreMatch = Set.Make
  (struct
     type t = String.t
     let compare = case_ignore_equality_match
   end)

let new_case_ignore_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     CaseIgnoreMatch.add CaseIgnoreMatch.mem
     CaseIgnoreMatch.remove CaseIgnoreMatch.empty
     CaseIgnoreMatch.elements CaseIgnoreMatch.cardinal
     CaseIgnoreMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.5 NAME 'caseExactMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_exact_equality_match v1 v2 = 
  String.compare 
    (collapse_whitespace v1)
    (collapse_whitespace v2)

module CaseExactMatch = Set.Make
  (struct
     type t = String.t
     let compare = case_exact_equality_match
   end)

let new_case_exact_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     CaseExactMatch.add CaseExactMatch.mem
     CaseExactMatch.remove CaseExactMatch.empty
     CaseExactMatch.elements CaseExactMatch.cardinal
     CaseExactMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.11 NAME 'caseIgnoreListMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.41 *)
let case_ignore_list_equality_match = case_ignore_equality_match

module CaseIgnoreListMatch = Set.Make
  (struct
     type t = String.t
     let compare = case_ignore_list_equality_match
   end)

let new_case_ignore_list_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     CaseIgnoreListMatch.add CaseIgnoreListMatch.mem
     CaseIgnoreListMatch.remove CaseIgnoreListMatch.empty
     CaseIgnoreListMatch.elements CaseIgnoreListMatch.cardinal
     CaseIgnoreListMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.20 NAME 'telephoneNumberMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.50 *)
let telephone_number_equality_match v1 v2 = 
  String.compare 
    (collapse_whitespace v1)
    (collapse_whitespace v2)

module TelephoneNumberMatch = Set.Make
  (struct
     type t = String.t
     let compare = telephone_number_equality_match
   end)

let new_telephone_number_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     TelephoneNumberMatch.add TelephoneNumberMatch.mem
     TelephoneNumberMatch.remove TelephoneNumberMatch.empty
     TelephoneNumberMatch.elements TelephoneNumberMatch.cardinal
     TelephoneNumberMatch.exists
     ordering substrings syntax :> attribute_t)

(* 1.3.6.1.4.1.1466.109.114.1 NAME 'caseExactIA5Match' SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 *)
let case_exact_ia5_equality_match v1 v2 = 
  String.compare
    (collapse_whitespace v1)
    (collapse_whitespace v2)

module CaseExactIA5Match = Set.Make
  (struct
     type t = String.t
     let compare = case_exact_ia5_equality_match
   end)

let new_case_exact_ia5_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     CaseExactIA5Match.add CaseExactIA5Match.mem
     CaseExactIA5Match.remove CaseExactIA5Match.empty
     CaseExactIA5Match.elements CaseExactIA5Match.cardinal
     CaseExactIA5Match.exists
     ordering substrings syntax :> attribute_t)

(* 1.3.6.1.4.1.1466.109.114.2 NAME 'caseIgnoreIA5Match' SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 *)
let case_ignore_ia5_equality_match v1 v2 = 
  Lcstring.compare 
    (Lcstring.of_string (collapse_whitespace v1))
    (Lcstring.of_string (collapse_whitespace v2))

module CaseIgnoreIA5Match = Set.Make
  (struct
     type t = String.t
     let compare = case_ignore_ia5_equality_match
   end)

let new_case_ignore_ia5_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     CaseIgnoreIA5Match.add CaseIgnoreIA5Match.mem
     CaseIgnoreIA5Match.remove CaseIgnoreIA5Match.empty
     CaseIgnoreIA5Match.elements CaseIgnoreIA5Match.cardinal
     CaseIgnoreIA5Match.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.17 NAME 'octetStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.40 *)
let octet_string_equality_match = String.compare

module OctetStringMatch = Set.Make
  (struct
     type t = String.t
     let compare = octet_string_equality_match
   end)

let new_octet_string_match ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     OctetStringMatch.add OctetStringMatch.mem
     OctetStringMatch.remove OctetStringMatch.empty
     OctetStringMatch.elements OctetStringMatch.cardinal
     OctetStringMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.13 NAME 'booleanMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.7 *)
let boolean_equality_match = String.compare

module BooleanMatch = Set.Make
  (struct
     type t = String.t
     let compare = octet_string_equality_match
   end)

let new_boolean_match ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     BooleanMatch.add BooleanMatch.mem
     BooleanMatch.remove BooleanMatch.empty 
     BooleanMatch.elements BooleanMatch.cardinal
     BooleanMatch.exists
     ordering substrings syntax :> attribute_t)

(* RFC2252:

   Implementors should note that the assertion syntax of these
   matching rules, an INTEGER or OID, is different from the value
   syntax of attributes for which this is the equality matching
   rule. 
   
   What is up with this! *)
(* 2.5.13.29 NAME 'integerFirstComponentMatch' 
   SYNTAX 1.3.6.1.4.1.1466.115.121.1.27 (wrong!) *)
let integer_first_component_match = integer_equality_match

module IntegerFirstComponentMatch = Set.Make
  (struct
     type t = String.t
     let compare = integer_first_component_match
   end)

let new_integer_first_component_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     IntegerFirstComponentMatch.add IntegerFirstComponentMatch.mem
     IntegerFirstComponentMatch.remove IntegerFirstComponentMatch.empty
     IntegerFirstComponentMatch.elements IntegerFirstComponentMatch.cardinal
     IntegerFirstComponentMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.30 NAME 'objectIdentifierFirstComponentMatch' 
   SYNTAX 1.3.6.1.4.1.1466.115.121.1.38 (wrong!) *)
let object_identifier_first_component_match = object_identifier_equality_match

module ObjectIdentifierFirstComponentMatch = Set.Make
  (struct
     type t = String.t
     let compare = object_identifier_first_component_match
   end)

let new_object_identifier_first_component_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     ObjectIdentifierFirstComponentMatch.add ObjectIdentifierFirstComponentMatch.mem
     ObjectIdentifierFirstComponentMatch.remove ObjectIdentifierFirstComponentMatch.empty
     ObjectIdentifierFirstComponentMatch.elements ObjectIdentifierFirstComponentMatch.cardinal
     ObjectIdentifierFirstComponentMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.31 NAME 'directoryStringFirstComponentMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let directory_string_first_component_match = case_ignore_equality_match

module DirectoryStringFirstComponentMatch = Set.Make
  (struct
     type t = String.t
     let compare = directory_string_first_component_match
   end)

let new_directory_string_first_component_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     DirectoryStringFirstComponentMatch.add DirectoryStringFirstComponentMatch.mem
     DirectoryStringFirstComponentMatch.remove DirectoryStringFirstComponentMatch.empty
     DirectoryStringFirstComponentMatch.elements DirectoryStringFirstComponentMatch.cardinal
     DirectoryStringFirstComponentMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.33 NAME 'keywordMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let keyword_equality_match = case_ignore_equality_match

module KeywordMatch = Set.Make
  (struct
     type t = String.t
     let compare = keyword_equality_match
   end)

let new_keyword_equality_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     KeywordMatch.add KeywordMatch.mem
     KeywordMatch.remove KeywordMatch.empty
     KeywordMatch.elements KeywordMatch.cardinal
     KeywordMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.41 NAME 'storedPrefixMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let stored_prefix_match = case_ignore_equality_match

module StoredPrefixMatch = Set.Make
  (struct
     type t = String.t
     let compare = stored_prefix_match
   end)

let new_stored_prefix_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     StoredPrefixMatch.add StoredPrefixMatch.mem
     StoredPrefixMatch.remove StoredPrefixMatch.empty
     StoredPrefixMatch.elements StoredPrefixMatch.cardinal
     StoredPrefixMatch.exists
     ordering substrings syntax :> attribute_t)

(* 2.5.13.32 NAME 'wordMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let word_match = case_ignore_equality_match

module WordMatch = Set.Make
  (struct
     type t = String.t
     let compare = word_match
   end)

let new_word_match_set ?(ordering=None) ?(substrings=None) syntax =
  (new attribute
     WordMatch.add WordMatch.mem
     WordMatch.remove WordMatch.empty
     WordMatch.elements WordMatch.cardinal
     WordMatch.exists
     ordering substrings syntax :> attribute_t)




(* ordering matching rules used in inequality filters *)

(* 2.5.13.28 NAME 'generalizedTimeOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.24 *)
let generalized_time_ordering_match v1 v2 = String.compare v1 v2

(* 2.5.13.3 NAME 'caseIgnoreOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_ignore_ordering_match v1 v2 = 
  Lcstring.compare
    (Lcstring.of_string (collapse_whitespace v1))
    (Lcstring.of_string (collapse_whitespace v2))

(* 2.5.13.6 NAME 'caseExactOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_exact_ordering_match v1 v2 = 
  String.compare
    (collapse_whitespace v1)
    (collapse_whitespace v2)

(* 2.5.13.9 NAME 'numericStringOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.36 *)
let numeric_string_ordering_match v1 v2 =
  String.compare
    (remove_whitespace v1)
    (remove_whitespace v2)

(* 2.5.13.15 NAME 'integerOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.27 *)
let integer_ordering_match = numeric_string_ordering_match

(* 2.5.13.18 NAME 'octetStringOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.40 *)
let octet_string_ordering_match = case_ignore_ordering_match





(* substring matching rules *)

(* 2.5.13.4 NAME 'caseIgnoreSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_ignore_substrings_match subs v = false

(* 2.5.13.7 NAME 'caseExactSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_exact_substrings_match subs v = false

(* 2.5.13.21 NAME 'telephoneNumberSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.50 *)
let telephone_number_substrings_match subs v = false

(* 2.5.13.10 NAME 'numericStringSubstringsMatch' SYNTAX  1.3.6.1.4.1.1466.115.121.1.36 *)
let numeric_string_substrings_match subs v = false

(* 1.3.6.1.4.1.4203.1.2.1 NAME 'caseExactIA5SubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 *)
let case_exact_ia5_substrings_match subs v = false

(* 1.3.6.1.4.1.1466.109.114.3 NAME 'caseIgnoreIA5SubstringsMatch' 
   SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 *)
let case_ignore_ia5_substrings_match subs v = false

(* 2.5.13.12 NAME 'caseIgnoreListSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.58 *)
let case_ignore_list_substrings_match subs v = false





let oid = Oid.of_string
let (equality, equality_bysyntax) = 
  List.fold_left
    (fun (m1, m2) (oid, alias, syntax, checksyntax, constructor) -> 
       (Oidmap.add alias (syntax, checksyntax, constructor)
	  (Oidmap.add oid (syntax, checksyntax, constructor) m1),
	Oidmap.add syntax constructor m2))
    (Oidmap.empty, Oidmap.empty)
    [(oid "2.5.13.13", oid "booleanMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.7", false, new_boolean_match);
     (oid "2.5.13.32", oid "wordMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, new_word_match_set);
     (oid "2.5.13.41", oid "storedPrefixMatch",
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, new_stored_prefix_set);
     (oid "2.5.13.33", oid "keywordMatch",
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, new_keyword_equality_set);
     (oid "2.5.13.31", oid "directoryStringFirstComponentMatch",
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, new_directory_string_first_component_set);
     (oid "2.5.13.30", oid "objectIdentifierFirstComponentMatch",
      oid "1.3.6.1.4.1.1466.115.121.1.38", false, new_object_identifier_first_component_set);
     (oid "2.5.13.29", oid "integerFirstComponentMatch",
      oid "1.3.6.1.4.1.1466.115.121.1.27", false, new_integer_first_component_set);
     (oid "2.5.13.17", oid "octetStringMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.40", false, new_octet_string_match);
     (oid "2.5.13.0", oid "objectidentifiermatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.38", false, new_object_identifier_equality_set);
     (oid "2.5.13.1", oid "distinguishednamematch", 
      oid "1.3.6.1.4.1.1466.115.121.1.12", false, new_distinguished_name_equality_set);
     (oid "2.5.13.8", oid "numericstringmatch",
      oid "1.3.6.1.4.1.1466.115.121.1.36", false, new_numeric_string_equality_set);
     (oid "2.5.13.14", oid "integermatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.27", false, new_integer_equality_set);
     (oid "2.5.13.16", oid "bitstringmatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.6", false, new_bit_string_equality_set);
     (oid "2.5.13.22", oid "presentationaddressmatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.43", false, new_presentation_address_equality_set);
     (oid "2.5.13.23", oid "uniquemembermatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.34", false, new_unique_member_equality_set);
     (oid "2.5.13.24", oid "protocolinformationmatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.42", false, new_protocol_information_equality_set);
     (oid "2.5.13.27", oid "generalizedtimematch", 
      oid "1.3.6.1.4.1.1466.115.121.1.24", false, new_generalized_time_equality_set);
     (oid "2.5.13.2", oid "caseignorematch", 
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, new_case_ignore_equality_set);
     (oid "2.5.13.5", oid "caseExactMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, new_case_exact_equality_set);
     (oid "2.5.13.11", oid "caseignorelistmatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.41", false, new_case_ignore_list_equality_set);
     (oid "2.5.13.20", oid "telephonenumbermatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.50", false, new_telephone_number_equality_set);
     (oid "1.3.6.1.4.1.1466.109.114.1", oid "caseexactia5match", 
      oid "1.3.6.1.4.1.1466.115.121.1.26", false, new_case_exact_ia5_equality_set);
     (oid "1.3.6.1.4.1.1466.109.114.2", oid "caseignoreia5match", 
      oid "1.3.6.1.4.1.1466.115.121.1.26", false, new_case_ignore_ia5_equality_set)]

let (ordering, ordering_bysyntax) =
  List.fold_left
    (fun (m1, m2) (oid, alias, syntax, checksyntax, matchingrule) -> 
       (Oidmap.add alias (syntax, checksyntax, matchingrule) 
	  (Oidmap.add oid (syntax, checksyntax, matchingrule) m1),
	Oidmap.add syntax matchingrule m2))
    (Oidmap.empty, Oidmap.empty)
    [(oid "2.5.13.28", oid "generalizedtimeorderingmatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.24", false, generalized_time_ordering_match);
     (oid "2.5.13.3", oid "caseignoreorderingmatch",
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, case_ignore_ordering_match);
     (oid "2.5.13.18", oid "octetStringOrderingMatch",
      oid "1.3.6.1.4.1.1466.115.121.1.40", false, octet_string_ordering_match);
     (oid "2.5.13.6", oid "caseExactOrderingMatch",
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, case_exact_ordering_match);
     (oid "2.5.13.9", oid "numericStringOrderingMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.36", false, numeric_string_ordering_match);
     (oid "2.5.13.15", oid "integerOrderingMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.27", false, integer_ordering_match)]

let (substring, substring_bysyntax) = 
  List.fold_left
    (fun 
       (m1, m2) 
       (oid, alias, syntax, checksyntax, 
	(value: (Ldap_types.substring_component -> string -> bool))) -> 
	 (Oidmap.add alias (syntax, checksyntax, value) 
	    (Oidmap.add oid (syntax, checksyntax, value) m1),
	  Oidmap.add syntax value m2))
    (Oidmap.empty, Oidmap.empty)
    [(oid "2.5.13.4", oid "caseignoresubstringsmatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, case_ignore_substrings_match);
     (oid "2.5.13.12", oid "caseIgnoreListSubstringsMatch",
      oid "1.3.6.1.4.1.1466.115.121.1.58", false, case_ignore_list_substrings_match);
     (oid "2.5.13.21", oid "telephonenumbersubstringsmatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.50", false, telephone_number_substrings_match);
     (oid "2.5.13.10", oid "numericstringsubstringsmatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.36", false, numeric_string_substrings_match);
     (oid "2.5.13.7", oid "caseExactSubstringsMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.15", false, case_exact_substrings_match);
     (oid "1.3.6.1.4.1.4203.1.2.1", oid "caseExactIA5SubstringsMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.26", false, case_exact_ia5_substrings_match);
     (oid "1.3.6.1.4.1.1466.109.114.3", oid "caseIgnoreIA5SubstringsMatch", 
      oid "1.3.6.1.4.1.1466.115.121.1.26", false, case_ignore_ia5_substrings_match)]
