open Ldap_schema

exception Invalid_syntax of string

(* 1.3.6.1.4.1.1466.115.121.1.3 DESC 'Attribute Type Description' *)
let attribute_type_description_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.5 DESC 'Binary' *)
let binary_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.6 DESC 'Bit String' *)
let bitstring_rex = Pcre.regexp ~study:true "^\'[01]*\'B$"
let bitstring_syntax v =
  if not (Pcre.pmatch ~rex:bitstring_rex v) then
    raise (Invalid_syntax v)

(* 1.3.6.1.4.1.1466.115.121.1.7 DESC 'Boolean' *)
let boolean_syntax v =
  if not (v = "TRUE" || v = "FALSE") then
    raise (Invalid_syntax v)

(* 1.3.6.1.4.1.1466.115.121.1.8 DESC 'Certificate' *)
let certificate_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.9 DESC 'Certificate List' *)
let certificate_list_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.10 DESC 'Certificate Pair' *)
let certificate_pair_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.11 DESC 'Country String' *)
let country_string_rex = Pcre.regexp ~study:true "^[a-zA-Z]{2}$"
let country_string_syntax v =
  if Pcre.pmatch ~rex:country_string_rex v then
    raise (Invalid_syntax v)

(* 1.3.6.1.4.1.1466.115.121.1.12 DESC 'DN' *)
let dn_syntax v =
  try ignore (Ldap_dn.of_string v)
  with exn ->
    raise (Invalid_syntax v)

(* 1.3.6.1.4.1.1466.115.121.1.15 DESC 'Directory String' *)
let directory_string_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.22 DESC 'Facsimile Telephone Number' *)
let facsimile_telephone_number_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.23 DESC 'Fax' *)
let fax_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.24 DESC 'Generalized Time' *)
let generalized_time_rex = Pcre.regexp ~study:true "^[0-9]{4}()"
let generalized_time_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.26 DESC 'IA5 String' *)
let ia5_string_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.27 DESC 'INTEGER' *)
let integer_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.28 DESC 'JPEG' *)
let jpeg_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.30 DESC 'Matching Rule Description' *)
let matching_rule_description_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.31 DESC 'Matching Rule Use Description' *)
let matching_rule_use_description_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.33 DESC 'MHS OR Address' *)
let mhs_or_address_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.34 DESC 'Name And Optional UID' *)
let name_and_optional_uid v = ()

(* 1.3.6.1.4.1.1466.115.121.1.35 DESC 'Name Form Description' *)
let name_form_description_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.36 DESC 'Numeric String' *)
let numeric_string_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.37 DESC 'Object Class Description' *)
let object_class_description_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.38 DESC 'OID' *)
let oid_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.39 DESC 'Other Mailbox' *)
let other_mailbox_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.41 DESC 'Postal Address' *)
let postal_address_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.43 DESC 'Presentation Address' *)
let presentation_address_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.44 DESC 'Printable String' *)
let printable_string_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.50 DESC 'Telephone Number' *)
let telephone_number_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.53 DESC 'UTC Time' *)
let utc_time_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.54 DESC 'LDAP Syntax Description' *)
let ldap_syntax_description v = ()

(* 1.3.6.1.4.1.1466.115.121.1.17 DESC 'DIT Structure Rule Description' *)
let dit_structure_rule_description_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.40 DESC 'Octet String' *)
let octet_string v = ()

(* 1.3.6.1.4.1.1466.115.121.1.13 DESC 'Data Quality Syntax' *)
let data_quality_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.51 DESC 'Teletex Terminal Identifier' *)
let teletex_terminal_identifier v = ()

(* 1.3.6.1.4.1.1466.115.121.1.25 DESC 'Guide' *)
let guide v = ()

(* 1.3.6.1.4.1.1466.115.121.1.52 DESC 'Telex Number' *)
let telex_number v = ()

(* 1.3.6.1.1.1.0.1 DESC 'RFC2307 Boot Parameter Syntax' *)
let rfc2307_boot_paramater_syntax v = ()

(* 1.3.6.1.1.1.0.0 DESC 'RFC2307 NIS Netgroup Triple' *)
let rfc2307_nis_netgroup_triple v = ()

(* 1.3.6.1.4.1.1466.115.121.1.21 DESC 'Enhanced Guide' *)
let enhanced_guide v = ()

(* 1.3.6.1.4.1.1466.115.121.1.1 DESC 'ACI Item' *)
let aci_item v = ()

(* 1.3.6.1.4.1.1466.115.121.1.2 DESC 'Access Point' *)
let access_point v = ()

(* 1.3.6.1.4.1.1466.115.121.1.4 DESC 'Audio' *)
let audio v = ()

(* 1.3.6.1.4.1.1466.115.121.1.14 DESC 'Delivery Method' *)
let delivery_method v = ()

(* 1.3.6.1.4.1.1466.115.121.1.16 DESC 'DIT Content Rule Description' *)
let dit_content_rule_description v = ()

(* 1.3.6.1.4.1.1466.115.121.1.18 DESC 'DL Submit Permission' *)
let dl_submit_permission v = ()

(* 1.3.6.1.4.1.1466.115.121.1.19 DESC 'DSA Quality Syntax' *)
let dsa_quality_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.20 DESC 'DSE Type' *)
let dse_type v = ()

(* 1.3.6.1.4.1.1466.115.121.1.29 DESC 'Master And Shadow Access Points' *)
let master_and_shadow_access_points v = ()

(* 1.3.6.1.4.1.1466.115.121.1.32 DESC 'Mail Preference' *)
let mail_preference v = ()

(* 1.3.6.1.4.1.1466.115.121.1.42 DESC 'Protocol Information' *)
let protocol_information v = ()

(* 1.3.6.1.4.1.1466.115.121.1.45 DESC 'Subtree Specification' *)
let subtree_specification v = ()

(* 1.3.6.1.4.1.1466.115.121.1.46 DESC 'Supplier Information' *)
let supplier_information v = ()

(* 1.3.6.1.4.1.1466.115.121.1.47 DESC 'Supplier Or Consumer' *)
let supplier_or_consumer v = ()

(* 1.3.6.1.4.1.1466.115.121.1.48 DESC 'Supplier And Consumer' *)
let supplier_and_consumer v = ()

(* 1.3.6.1.4.1.1466.115.121.1.49 DESC 'Supported Algorithm' *)
let supported_algorithm v = ()

(* 1.3.6.1.4.1.1466.115.121.1.55 DESC 'Modify Rights' *)
let modify_rights v = ()

(* 1.3.6.1.4.1.1466.115.121.1.56 DESC 'LDAP Schema Definition' *)
let ldap_schema_definition v = ()

(* 1.3.6.1.4.1.1466.115.121.1.57 DESC 'LDAP Schema Description' *)
let ldap_schema_description v = ()

(* 1.3.6.1.4.1.1466.115.121.1.58 DESC 'Substring Assertion' *)
let substring_assertion v = ()

let syntaxes =
  List.fold_left
    (fun m (key, (value: string -> unit)) -> Oidmap.add key value m)
    Oidmap.empty
    [(Oid.of_string "1.3.6.1.4.1.1466.115.121.1.40", octet_string);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.58", substring_assertion);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.57", ldap_schema_description);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.56", ldap_schema_definition);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.55", modify_rights);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.49", supported_algorithm);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.48", supplier_and_consumer);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.47", supplier_or_consumer);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.46", supplier_information);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.45", subtree_specification);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.42", protocol_information);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.32", mail_preference);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.29", master_and_shadow_access_points);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.20", dse_type);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.19", dsa_quality_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.18", dl_submit_permission);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.16", dit_content_rule_description);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.14", delivery_method);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.4", audio);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.2", access_point);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.1", aci_item);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.21", enhanced_guide);
     (Oid.of_string "1.3.6.1.1.1.0.0", rfc2307_nis_netgroup_triple);
     (Oid.of_string "1.3.6.1.1.1.0.1", rfc2307_boot_paramater_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.52", telex_number);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.25", guide);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.51", teletex_terminal_identifier);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.13", data_quality_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.3", attribute_type_description_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.5", binary_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.6", bitstring_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.7", boolean_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.8", certificate_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.9", certificate_list_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.10", certificate_pair_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.11", country_string_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.12", dn_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.15", directory_string_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.22", facsimile_telephone_number_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.23", fax_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.24", generalized_time_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.26", ia5_string_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.27", integer_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.28", jpeg_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.30", matching_rule_description_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.31", matching_rule_use_description_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.33", mhs_or_address_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.34", name_and_optional_uid);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.35", name_form_description_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.36", numeric_string_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.37", object_class_description_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.38", oid_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.39", other_mailbox_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.41", postal_address_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.43", presentation_address_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.44", printable_string_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.50", telephone_number_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.53", utc_time_syntax);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.54", ldap_syntax_description);
     (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.17", dit_structure_rule_description_syntax)]
