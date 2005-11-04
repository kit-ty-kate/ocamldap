open Ldap_types
open Ldap_schema

(* equality matching rules *)
exception Value_exists
exception Value_does_not_exist
class attribute add mem remove empty elements =
object
  val mutable store = empty
  method add ?(idempotent=true) v =
    if idempotent then
      store <- add v store
    else if mem v store then
      raise Value_exists
    else
      store <- add v store

  method delete ?(idempotent=true) v =
    if idempotent then
      store <- remove v store
    else if not (mem v store) then
      raise Value_does_not_exist
    else
      store <- remove v store

  method replace vals =
    store <- 
      (List.fold_left
	 (fun s v -> add v s)
	 empty
	 vals)

  method exists v = exists v store
  method get_value = elements store
end

(* 2.5.13.0 NAME 'objectIdentifierMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.38 *)
module ObjectIdentifierMatch = Set.make
  (struct
     type t = String.t
     let compare = String.compare v1 v2
   end)

(* 2.5.13.1 NAME 'distinguishedNameMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.12 *)
let distinguished_name_equality_match v1 v2 = String.compare v1 v2
module DistinguishedNameMatch = Set.Make
  (struct
     type t = String.t
     let compare = distinguished_name_equality_match
   end)

(* 2.5.13.8 NAME 'numericStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.36 *)
let numeric_string_equality_match v1 v2 = String.compare v1 v2
module NumericStringMatch = Set.Make
  (struct
     type t = String.t
     let compare = numeric_string_equality_match
   end)

(* 2.5.13.14 NAME 'integerMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.27 *)
let integer_equality_match v1 v2 = String.compare v1 v2
module IntegerMatch = Set.Make
  (struct
     type t = String.t
     let compare = integer_equality_match
   end)

(* 2.5.13.16 NAME 'bitStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.6 *)
let bit_string_equality_match v1 v2 = String.compare v1 v2
module BitStringMatch = Set.Make
  (struct
     type t = String.t
     let compare = bit_string_equality_match
   end)

(* 2.5.13.22 NAME 'presentationAddressMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.43 *)
let presentation_address_equality_match v1 v2 = String.compare v1 v2
module PresentationAddressMatch = Set.Make
  (struct
     type t = String.t
     let compare = presentation_address_equality_match
   end)

(* 2.5.13.23 NAME 'uniqueMemberMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.34 *)
let unique_member_equality_match v1 v2 = String.compare v1 v2
module UniqueMemberMatch = Set.Make
  (struct
     type t = String.t
     let compare = unique_member_equality_match
   end)

(* 2.5.13.24 NAME 'protocolInformationMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.42 *)
let protocol_information_equality_match v1 v2 = String.compare v1 v2
module ProtocolInformationMatch = Set.Make
  (struct
     type t = String.t
     let compare = protocol_information_equality_match
   end)

(* 2.5.13.27 NAME 'generalizedTimeMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.24 *)
let generalized_time_equality_match v1 v2 = String.compare v1 v2
module GeneralizedTimeMatch = Set.Make
  (struct
     type t = String.t
     let compare = generalized_time_equality_match
   end)

(* *)
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

(* 2.5.13.11 NAME 'caseIgnoreListMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.41 *)
let case_ignore_list_equality_match = case_ignore_equality_match
module CaseIgnoreListMatch = Set.Make
  (struct
     type t = String.t
     let compare = case_ignore_list_equality_match
   end)

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

(* ordering matching rules used in inequality filters *)

(* 2.5.13.28 NAME 'generalizedTimeOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.24 *)
let generalized_time_ordering_match v1 v2 = String.compare v1 v2
module GeneralizedTimeOrderingMatch = Set.Make
  (struct
     type t = String.t
     let compare = generalized_time_ordering_match
   end)

(* 2.5.13.3 NAME 'caseIgnoreOrderingMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.15 *)
let case_ignore_ordering_match v1 v2 = 
  String.compare
    (String.lowercase (collapse_whitespace v1))
    (String.lowercase (collapse_whitespace v2))
module CaseIgnoreOrderingMatch = Set.Make
  (struct
     type t = String.t
     let compare = case_ignore_ordering_match
   end)

(* substring matching rules, these are different beasts *)

(* 2.5.13.4 NAME 'caseIgnoreSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.58 *)
let case_ignore_substrings_match subs v = false

(* 2.5.13.21 NAME 'telephoneNumberSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.58 *)
let telephone_number_substrings_match subs v = false

(* 2.5.13.10 NAME 'numericStringSubstringsMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.58 *)
let numeric_string_substrings_match subs v = false

let equality_matching_rules = 
  List.fold_left
    (fun m (key, value) -> Oidmap.add key value m)
    Oidmap.empty
    [(Oid.of_string "2.5.13.0", object_identifier_equality_match);
     (Oid.of_string "objectidentifiermatch", object_identifier_equality_match);
     (Oid.of_string "2.5.13.1", distinguished_name_equality_match);
     (Oid.of_string "distinguishednamematch", distinguished_name_equality_match);
     (Oid.of_string "2.5.13.8", numeric_string_equality_match);
     (Oid.of_string "numericstringmatch", numeric_string_equality_match);
     (Oid.of_string "2.5.13.14", integer_equality_match);
     (Oid.of_string "integermatch", integer_equality_match);
     (Oid.of_string "2.5.13.16", bit_string_equality_match);
     (Oid.of_string "bitstringmatch", bit_string_equality_match);
     (Oid.of_string "2.5.13.22", presentation_address_equality_match);
     (Oid.of_string "presentationaddressmatch", presentation_address_equality_match);
     (Oid.of_string "2.5.13.23", unique_member_equality_match);
     (Oid.of_string "uniquemembermatch", unique_member_equality_match);
     (Oid.of_string "2.5.13.24", protocol_information_equality_match);
     (Oid.of_string "protocolinformationmatch", protocol_information_equality_match);
     (Oid.of_string "2.5.13.27", generalized_time_equality_match);
     (Oid.of_string "generalizedtimematch", generalized_time_equality_match);
     (Oid.of_string "2.5.13.2", case_ignore_equality_match);
     (Oid.of_string "caseignorematch", case_ignore_equality_match);
     (Oid.of_string "2.5.13.11", case_ignore_list_equality_match);
     (Oid.of_string "caseignorelistmatch", case_ignore_list_equality_match);
     (Oid.of_string "2.5.13.20", telephonenumber_equality_match);
     (Oid.of_string "telephonenumbermatch", telephonenumber_equality_match);
     (Oid.of_string "1.3.6.1.4.1.1466.109.114.1", case_exact_ia5_equality_match);
     (Oid.of_string "caseexactia5match", case_exact_ia5_equality_match);
     (Oid.of_string "1.3.6.1.4.1.1466.109.114.2", case_ignore_ia5_equality_match);
     (Oid.of_string "caseignoreia5match", case_ignore_ia5_equality_match)]

let ordering_matching_rules =
  List.fold_left
    (fun m (key, value) -> Oidmap.add key value m)
    Oidmap.empty
    [(Oid.of_string "2.5.13.28", generalized_time_ordering_match);
     (Oid.of_string "generalizedtimeorderingmatch", generalized_time_ordering_match);
     (Oid.of_string "2.5.13.3", case_ignore_ordering_match);
     (Oid.of_string "caseignoreorderingmatch", case_ignore_ordering_match)]

let substring_matching_rules = 
  List.fold_left
    (fun m (key, value) -> Oidmap.add key value m)
    Oidmap.empty
    [(Oid.of_string "2.5.13.4", case_ignore_substrings_match);
     (Oid.of_string "caseignoresubstringsmatch", case_ignore_substrings_match);
     (Oid.of_string "2.5.13.21", telephone_number_substring_match);
     (Oid.of_string "telephonenumbersubstringsmatch", telephone_number_substring_match);
     (Oid.of_string "2.5.13.10", numeric_string_substrings_match);
     (Oid.of_string "numericstringsubstringsmatch", numeric_string_substrings_match)]
