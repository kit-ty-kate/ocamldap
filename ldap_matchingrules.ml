open Ldap_types
open Ldap_schema

(* equality matching rules *)

(* 2.5.13.0 NAME 'objectIdentifierMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.38 *)
let object_identifier_equality_match v1 v2 = String.compare v1 v2

(* 2.5.13.1 NAME 'distinguishedNameMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.12 *)
let distinguished_name_equality_match v1 v2 = String.compare v1 v2

(* 2.5.13.8 NAME 'numericStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.36 *)
let numeric_string_equality_match v1 v2 = String.compare v1 v2

(* 2.5.13.14 NAME 'integerMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.27 *)
let integer_equality_match v1 v2 = String.compare v1 v2

(* 2.5.13.16 NAME 'bitStringMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.6 *)
let bit_string_equality_match v1 v2 = String.compare v1 v2

(* 2.5.13.22 NAME 'presentationAddressMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.43 *)
let presentation_address_equality_match v1 v2 = String.compare v1 v2

(* 2.5.13.23 NAME 'uniqueMemberMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.34 *)
let unique_member_equality_match v1 v2 = String.compare v1 v2

(* 2.5.13.24 NAME 'protocolInformationMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.42 *)
let protocol_information_equality_match v1 v2 = String.compare v1 v2

(* 2.5.13.27 NAME 'generalizedTimeMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.24 *)
let generalized_time_equality_match v1 v2 = String.compare v1 v2

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

(* 2.5.13.11 NAME 'caseIgnoreListMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.41 *)
let case_ignore_list_equality_match = case_ignore_equality_match

(* 2.5.13.20 NAME 'telephoneNumberMatch' SYNTAX 1.3.6.1.4.1.1466.115.121.1.50 *)
let telephone_number_equality_match v1 v2 = 
  String.compare 
    (collapse_whitespace v1)
    (collapse_whitespace v2)

(* 1.3.6.1.4.1.1466.109.114.1 NAME 'caseExactIA5Match' SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 *)
let case_exact_ia5_equality_match v1 v2 = 
  String.compare
    (collapse_whitespace v1)
    (collapse_whitespace v2)

(* 1.3.6.1.4.1.1466.109.114.2 NAME 'caseIgnoreIA5Match' SYNTAX 1.3.6.1.4.1.1466.115.121.1.26 *)
let case_ignore_ia5_equality_match v1 v2 = 
  String.compare 
    (String.lowercase (collapse_whitespace v1))
    (String.lowercase (collapse_whitespace v2))

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

let equality_matching_rules = 
  let t = Hashtbl.create 15 in
    Hashtbl.add t (Oid.of_string "2.5.13.0") object_identifier_equality_match;
    Hashtbl.add t (Oid.of_string "objectidentifiermatch") object_identifier_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.1") distinguished_name_equality_match;
    Hashtbl.add t (Oid.of_string "distinguishednamematch") distinguished_name_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.8") numeric_string_equality_match;
    Hashtbl.add t (Oid.of_string "numericstringmatch") numeric_string_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.14") integer_equality_match;
    Hashtbl.add t (Oid.of_string "integermatch") integer_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.16") bit_string_equality_match;
    Hashtbl.add t (Oid.of_string "bitstringmatch") bit_string_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.22") presentation_address_equality_match;
    Hashtbl.add t (Oid.of_string "presentationaddressmatch") presentation_address_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.23") unique_member_equality_match;
    Hashtbl.add t (Oid.of_string "uniquemembermatch") unique_member_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.24") protocol_information_equality_match;
    Hashtbl.add t (Oid.of_string "protocolinformationmatch") protocol_information_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.27") generalized_time_equality_match;
    Hashtbl.add t (Oid.of_string "generalizedtimematch") generalized_time_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.2") case_ignore_equality_match;
    Hashtbl.add t (Oid.of_string "caseignorematch") case_ignore_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.11") case_ignore_list_equality_match;
    Hashtbl.add t (Oid.of_string "caseignorelistmatch") case_ignore_list_equality_match;
    Hashtbl.add t (Oid.of_string "2.5.13.20") telephonenumber_equality_match;
    Hashtbl.add t (Oid.of_string "1.3.6.1.4.1.1466.109.114.1") case_exact_ia5_equality_match;
    Hashtbl.add t (Oid.of_string "1.3.6.1.4.1.1466.109.114.2") case_ignore_ia5_equality_match;
    t

let ordering_matching_rules =
  let t = Hashtbl.create 5 in
    Hashtbl.add t (Oid.of_string "2.5.13.28") generalized_time_ordering_match;
    Hashtbl.add t (Oid.of_string "2.5.13.3") case_ignore_ordering_match;
    t

let substring_matching_rules = 
  let t = Hashtbl.create 5 in
    Hashtbl.add t (Oid.of_string "2.5.13.4") case_ignore_substrings_match;
    Hashtbl.add t (Oid.of_string "2.5.13.21") telephone_number_substring_match;
    Hashtbl.add t (Oid.of_string "2.5.13.10") numeric_string_substrings_match;
    t
