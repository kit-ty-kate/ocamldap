open Ldap_types
open Ldap_schema

exception Value_exists
exception Value_does_not_exist

class type attribute_t =
object
  method add: ?idempotent:bool -> string -> unit
  method delete: ?idempotent:bool -> string -> unit
  method replace: string list -> unit
  method exists: string -> bool
  method values: string list
end

class ['a] attribute 
  (add: string -> 'a -> 'a) 
  (mem: string -> 'a -> bool)
  (remove: string -> 'a -> 'a)
  (empty: 'a)
  (elements: 'a -> string list)
  (syntax: string -> unit) =
object
  val mutable store = empty
  method add ?(idempotent=false) v =
    syntax v;
    if idempotent then
      store <- add v store
    else if mem v store then
      raise Value_exists
    else
      store <- add v store
  method delete ?(idempotent=false) v =
    if idempotent then
      store <- remove v store
    else if not (mem v store) then
      raise Value_does_not_exist
    else
      store <- remove v store
  method replace vals =
    store <- 
      (List.fold_left
	 (fun s v -> syntax v;add v s)
	 empty
	 vals)
  method exists v = mem v store
  method values = elements store
end

(* equality matching rules *)

(* 2.5.13.0 NAME 'objectIdentifierMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.38 *)
let object_identifier_equality_match v1 v2 = String.compare v1 v2

module ObjectIdentifierMatch = Set.Make
  (struct
     type t = String.t
     let compare = object_identifier_equality_match
   end)

let new_object_identifier_equality_set syntax =
  (new attribute
     ObjectIdentifierMatch.add
     ObjectIdentifierMatch.mem
     ObjectIdentifierMatch.remove
     ObjectIdentifierMatch.empty
     ObjectIdentifierMatch.elements
     syntax :> attribute_t)

(* 2.5.13.1 NAME 'distinguishedNameMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.12 *)
let distinguished_name_equality_match v1 v2 = String.compare v1 v2

module DistinguishedNameMatch = Set.Make
  (struct
     type t = String.t
     let compare = distinguished_name_equality_match
   end)

let new_distinguished_name_equality_set syntax =
  (new attribute
     DistinguishedNameMatch.add
     DistinguishedNameMatch.mem
     DistinguishedNameMatch.remove
     DistinguishedNameMatch.empty
     DistinguishedNameMatch.elements
     syntax :> attribute_t)

(* 2.5.13.8 NAME 'numericStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.36 *)
let numeric_string_equality_match v1 v2 = String.compare v1 v2

module NumericStringMatch = Set.Make
  (struct
     type t = String.t
     let compare = numeric_string_equality_match
   end)

let new_numeric_string_equality_set syntax =
  (new attribute
     NumericStringMatch.add
     NumericStringMatch.mem
     NumericStringMatch.remove
     NumericStringMatch.empty
     NumericStringMatch.elements
     syntax :> attribute_t)

(* 2.5.13.14 NAME 'integerMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.27 *)
let integer_equality_match v1 v2 = String.compare v1 v2

module IntegerMatch = Set.Make
  (struct
     type t = String.t
     let compare = integer_equality_match
   end)

let new_integer_equality_set syntax =
  (new attribute
     IntegerMatch.add
     IntegerMatch.mem
     IntegerMatch.remove
     IntegerMatch.empty
     IntegerMatch.elements
     syntax :> attribute_t)

(* 2.5.13.16 NAME 'bitStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.6 *)
let bit_string_equality_match v1 v2 = String.compare v1 v2

module BitStringMatch = Set.Make
  (struct
     type t = String.t
     let compare = bit_string_equality_match
   end)

let new_bit_string_equality_set syntax =
  (new attribute
     BitStringMatch.add
     BitStringMatch.mem
     BitStringMatch.remove
     BitStringMatch.empty
     BitStringMatch.elements
     syntax :> attribute_t)

(* 2.5.13.22 NAME 'presentationAddressMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.43 *)
let presentation_address_equality_match v1 v2 = String.compare v1 v2

module PresentationAddressMatch = Set.Make
  (struct
     type t = String.t
     let compare = presentation_address_equality_match
   end)

let new_presentation_address_equality_set syntax =
  (new attribute
     PresentationAddressMatch.add
     PresentationAddressMatch.mem
     PresentationAddressMatch.remove
     PresentationAddressMatch.empty
     PresentationAddressMatch.elements
     syntax :> attribute_t)

(* 2.5.13.23 NAME 'uniqueMemberMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.34 *)
let unique_member_equality_match v1 v2 = String.compare v1 v2

module UniqueMemberMatch = Set.Make
  (struct
     type t = String.t
     let compare = unique_member_equality_match
   end)

let new_unique_member_equality_set syntax =
  (new attribute
     UniqueMemberMatch.add
     UniqueMemberMatch.mem
     UniqueMemberMatch.remove
     UniqueMemberMatch.empty
     UniqueMemberMatch.elements
     syntax :> attribute_t)

(* 2.5.13.24 NAME 'protocolInformationMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.42 *)
let protocol_information_equality_match v1 v2 = String.compare v1 v2

module ProtocolInformationMatch = Set.Make
  (struct
     type t = String.t
     let compare = protocol_information_equality_match
   end)

let new_protocol_information_equality_set syntax =
  (new attribute
     ProtocolInformationMatch.add
     ProtocolInformationMatch.mem
     ProtocolInformationMatch.remove
     ProtocolInformationMatch.empty
     ProtocolInformationMatch.elements
     syntax :> attribute_t)

(* 2.5.13.27 NAME 'generalizedTimeMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.24 *)
let generalized_time_equality_match v1 v2 = String.compare v1 v2

module GeneralizedTimeMatch = Set.Make
  (struct
     type t = String.t
     let compare = generalized_time_equality_match
   end)

let new_generalized_time_equality_set syntax =
  (new attribute
     GeneralizedTimeMatch.add
     GeneralizedTimeMatch.mem
     GeneralizedTimeMatch.remove
     GeneralizedTimeMatch.empty
     GeneralizedTimeMatch.elements
     syntax :> attribute_t)

let whsp = Pcre.regexp ~study:true "\\s+"
let leading_or_trailing_whsp = Pcre.regexp ~study:true "(^\\s+|\\s+$)"
let collapse_whitespace v = 
  (Pcre.replace ~rex:leading_or_trailing_whsp ~templ:""
     (Pcre.replace ~rex:whsp ~templ:" " v))
     
(* 2.5.13.2 NAME 'caseIgnoreMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_ignore_equality_match v1 v2 = 
  String.compare 
    (String.lowercase (collapse_whitespace v1))
    (String.lowercase (collapse_whitespace v2))

module CaseIgnoreMatch = Set.Make
  (struct
     type t = String.t
     let compare = case_ignore_equality_match
   end)

let new_case_ignore_equality_set syntax =
  (new attribute
     CaseIgnoreMatch.add
     CaseIgnoreMatch.mem
     CaseIgnoreMatch.remove
     CaseIgnoreMatch.empty
     CaseIgnoreMatch.elements
     syntax :> attribute_t)

(* 2.5.13.11 NAME 'caseIgnoreListMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.41 *)
let case_ignore_list_equality_match = case_ignore_equality_match

module CaseIgnoreListMatch = Set.Make
  (struct
     type t = String.t
     let compare = case_ignore_list_equality_match
   end)

let new_case_ignore_list_equality_set syntax =
  (new attribute
     CaseIgnoreListMatch.add
     CaseIgnoreListMatch.mem
     CaseIgnoreListMatch.remove
     CaseIgnoreListMatch.empty
     CaseIgnoreListMatch.elements
     syntax :> attribute_t)

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

let new_telephone_number_equality_set syntax =
  (new attribute
     TelephoneNumberMatch.add
     TelephoneNumberMatch.mem
     TelephoneNumberMatch.remove
     TelephoneNumberMatch.empty
     TelephoneNumberMatch.elements
     syntax :> attribute_t)

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

let new_case_exact_ia5_equality_set syntax =
  (new attribute
     CaseExactIA5Match.add
     CaseExactIA5Match.mem
     CaseExactIA5Match.remove
     CaseExactIA5Match.empty
     CaseExactIA5Match.elements
     syntax :> attribute_t)

(* 1.3.6.1.4.1.1466.109.114.2 NAME 'caseIgnoreIA5Match' SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 *)
let case_ignore_ia5_equality_match v1 v2 = 
  String.compare 
    (String.lowercase (collapse_whitespace v1))
    (String.lowercase (collapse_whitespace v2))

module CaseIgnoreIA5Match = Set.Make
  (struct
     type t = String.t
     let compare = case_ignore_ia5_equality_match
   end)

let new_case_ignore_ia5_equality_set syntax =
  (new attribute
     CaseIgnoreIA5Match.add
     CaseIgnoreIA5Match.mem
     CaseIgnoreIA5Match.remove
     CaseIgnoreIA5Match.empty
     CaseIgnoreIA5Match.elements
     syntax :> attribute_t)

(* 2.5.13.17 NAME 'octetStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.40 *)
let octet_string_match = String.compare

module OctetStringMatch = Set.Make
  (struct
     type t = String.t
     let compare = octet_string_match
   end)

let new_octet_string_match syntax =
  (new attribute
     OctetStringMatch.add
     OctetStringMatch.mem
     OctetStringMatch.remove
     OctetStringMatch.empty
     OctetStringMatch.elements
     syntax :> attribute_t)

(* ordering matching rules used in inequality filters *)

(* 2.5.13.28 NAME 'generalizedTimeOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.24 *)
let generalized_time_ordering_match v1 v2 = String.compare v1 v2

(* 2.5.13.3 NAME 'caseIgnoreOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_ignore_ordering_match v1 v2 = 
  String.compare
    (String.lowercase (collapse_whitespace v1))
    (String.lowercase (collapse_whitespace v2))

(* substring matching rules, these are different beasts *)

(* 2.5.13.4 NAME 'caseIgnoreSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.58 *)
let case_ignore_substrings_match subs v = false

(* 2.5.13.21 NAME 'telephoneNumberSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.58 *)
let telephone_number_substrings_match subs v = false

(* 2.5.13.10 NAME 'numericStringSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.58 *)
let numeric_string_substrings_match subs v = false

let equality = 
  List.fold_left
    (fun m (oid, syntax, attribute) -> Oidmap.add oid (syntax, attribute) m)
    Oidmap.empty
    [(Oid.of_string "2.5.13.17",
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.40",
      new_octet_string_match);
     (Oid.of_string "octetStringMatch",
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.40",
      new_octet_string_match);
     (Oid.of_string "2.5.13.0", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.38", 
      new_object_identifier_equality_set);
     (Oid.of_string "objectidentifiermatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.38", 
      new_object_identifier_equality_set);
     (Oid.of_string "2.5.13.1", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.12", 
      new_distinguished_name_equality_set);
     (Oid.of_string "distinguishednamematch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.12", 
      new_distinguished_name_equality_set);
     (Oid.of_string "2.5.13.8", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.36", 
      new_numeric_string_equality_set);
     (Oid.of_string "numericstringmatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.36", 
      new_numeric_string_equality_set);
     (Oid.of_string "2.5.13.14", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.27", 
      new_integer_equality_set);
     (Oid.of_string "integermatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.27", 
      new_integer_equality_set);
     (Oid.of_string "2.5.13.16", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.6", 
      new_bit_string_equality_set);
     (Oid.of_string "bitstringmatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.6", 
      new_bit_string_equality_set);
     (Oid.of_string "2.5.13.22", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.43", 
      new_presentation_address_equality_set);
     (Oid.of_string "presentationaddressmatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.43", 
      new_presentation_address_equality_set);
     (Oid.of_string "2.5.13.23", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.34", 
      new_unique_member_equality_set);
     (Oid.of_string "uniquemembermatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.34", 
      new_unique_member_equality_set);
     (Oid.of_string "2.5.13.24", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.42", 
      new_protocol_information_equality_set);
     (Oid.of_string "protocolinformationmatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.42", 
      new_protocol_information_equality_set);
     (Oid.of_string "2.5.13.27", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.24", 
      new_generalized_time_equality_set);
     (Oid.of_string "generalizedtimematch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.24", 
      new_generalized_time_equality_set);
     (Oid.of_string "2.5.13.2", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.15", 
      new_case_ignore_equality_set);
     (Oid.of_string "caseignorematch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.15", 
      new_case_ignore_equality_set);
     (Oid.of_string "2.5.13.11", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.41", 
      new_case_ignore_list_equality_set);
     (Oid.of_string "caseignorelistmatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.41", 
      new_case_ignore_list_equality_set);
     (Oid.of_string "2.5.13.20", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.50", 
      new_telephone_number_equality_set);
     (Oid.of_string "telephonenumbermatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.50", 
      new_telephone_number_equality_set);
     (Oid.of_string "1.3.6.1.4.1.1466.109.114.1", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.26", 
      new_case_exact_ia5_equality_set);
     (Oid.of_string "caseexactia5match", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.26", 
      new_case_exact_ia5_equality_set);
     (Oid.of_string "1.3.6.1.4.1.1466.109.114.2", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.26", 
      new_case_ignore_ia5_equality_set);
     (Oid.of_string "caseignoreia5match", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.26", 
      new_case_ignore_ia5_equality_set)]

let ordering =
  List.fold_left
    (fun m (oid, syntax, attribute) -> Oidmap.add oid (syntax, attribute) m)
    Oidmap.empty
    [(Oid.of_string "2.5.13.28", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.24", 
      generalized_time_ordering_match);
     (Oid.of_string "generalizedtimeorderingmatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.24",
      generalized_time_ordering_match);
     (Oid.of_string "2.5.13.3", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.15", 
      case_ignore_ordering_match);
     (Oid.of_string "caseignoreorderingmatch", 
      Oid.of_string "1.3.6.1.4.1.1466.115.121.1.15", 
      case_ignore_ordering_match)]
    
let substring = 
  List.fold_left
    (fun m (oid, (value: (Ldap_types.substring_component -> string -> bool))) -> 
       Oidmap.add oid value m)
    Oidmap.empty
    [(Oid.of_string "2.5.13.4", case_ignore_substrings_match);
     (Oid.of_string "caseignoresubstringsmatch", case_ignore_substrings_match);
     (Oid.of_string "2.5.13.21", telephone_number_substrings_match);
     (Oid.of_string "telephonenumbersubstringsmatch", telephone_number_substrings_match);
     (Oid.of_string "2.5.13.10", numeric_string_substrings_match);
     (Oid.of_string "numericstringsubstringsmatch", numeric_string_substrings_match)]
