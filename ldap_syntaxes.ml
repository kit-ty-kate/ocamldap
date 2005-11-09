open Ldap_schema

(* 1.3.6.1.4.1.1466.115.121.1.3 DESC 'Attribute Type Description' *)
let attribute_type_description_syntax v = ()
  
(* 1.3.6.1.4.1.1466.115.121.1.5 DESC 'Binary' *)
let binary_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.6 DESC 'Bit String' *)
let bitstring_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.7 DESC 'Boolean' *)
let boolean_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.8 DESC 'Certificate' *)
let certificate_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.9 DESC 'Certificate List' *)
let certificate_list_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.10 DESC 'Certificate Pair' *)
let certificate_pair_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.11 DESC 'Country String' *)
let country_string_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.12 DESC 'DN' *)
let dn_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.15 DESC 'Directory String' *)
let directory_string_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.22 DESC 'Facsimile Telephone Number' *)
let facsimile_telephone_number_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.23 DESC 'Fax' *)
let fax_syntax v = ()

(* 1.3.6.1.4.1.1466.115.121.1.24 DESC 'Generalized Time' *)
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

let syntaxes = 
  List.fold_left
    (fun m (key, (value: string -> unit)) -> Oidmap.add key value m)
    Oidmap.empty
    [(Oid.of_string "1.3.6.1.4.1.1466.115.121.1.40", octet_string);
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
