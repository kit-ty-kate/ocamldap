open Ldap_schema
open Ldap_matchingrules
open Ldap_syntaxes

type schema_object =
    [ `Attribute of string
    | `Objectclass of string ]

type schema_error =
    Non_unique_name_for_must_or_may of string
  | Undefined_must_or_may of string
  | Non_unique_name_for_superior_oc of string
  | Undefined_superior_oc of string
  | Cross_linked_oid of schema_object list
  | Unknown_syntax of Oid.t
  | Unknown_matching_rule of Oid.t
  | No_equality_matching_rule_defined

let validate schema =
  (* check that all musts, and all mays are attributes which exist. *)
  let errors =
    Lcmap.fold
      (fun oc {oc_must=musts;oc_may=mays} errors ->
	 let oc = Lcstring.to_string oc in
	 let check_error errors attr =
	   try ignore (attrNameToAttr schema attr);errors
	   with
	       Invalid_attribute _ ->
		 (`Objectclass oc, Undefined_must_or_may attr) :: errors
	     | Non_unique_attribute_alias attr ->
		 (`Objectclass oc, Non_unique_name_for_must_or_may attr) :: errors
	 in
	   (List.rev_append errors
	      (List.rev_append
		 (List.fold_left check_error [] musts)
		 (List.fold_left check_error [] mays))))
      schema.objectclasses
      []
  in
    (* check for cross linked oids *)
  let errors =
    let oids = Hashtbl.create 100 in
    let seen = Hashtbl.create 100 in
      Oidmap.iter
	(fun oid {at_name=n} -> Hashtbl.add oids oid (`Attribute (List.hd n)))
	schema.attributes_byoid;
      Oidmap.iter
	(fun oid {oc_name=n} -> Hashtbl.add oids oid (`Objectclass (List.hd n)))
	schema.objectclasses_byoid;
      Hashtbl.fold
	(fun oid name errors ->
	   if List.length (Hashtbl.find_all oids oid) > 1 then
	     if Hashtbl.mem seen oid then errors
	     else begin
	       Hashtbl.add seen oid ();
	       (name, Cross_linked_oid (Hashtbl.find_all oids oid)) :: errors
	     end
	   else
	     errors)
	oids
	errors
  in
    (* make sure all superior ocs are defined *)
  let errors =
    Lcmap.fold
      (fun oc {oc_sup=sups} errors ->
	 let oc = Lcstring.to_string oc in
	   List.fold_left
	     (fun errors sup ->
		try ignore (ocNameToOc schema sup);errors
		with
		    Invalid_objectclass _ ->
		      (`Objectclass oc, Undefined_superior_oc sup) :: errors
		  | Non_unique_objectclass_alias _ ->
		      (`Objectclass oc, Non_unique_name_for_superior_oc sup) :: errors)
	     errors
	     sups)
      schema.objectclasses
      errors
  in
    (* check that we know all of the syntaxes defined in the schema *)
  let errors =
    Lcmap.fold
      (fun attr {at_syntax=syntax} errors ->
	 let attr = Lcstring.to_string attr in
	   if Oidmap.mem syntax Ldap_syntaxes.syntaxes then errors
	   else (`Attribute attr, Unknown_syntax syntax) :: errors)
      schema.attributes
      errors
  in
    (* check that an equality matching rule is defined either directly,
       indirectly by a parent attribute, or by virtue of the syntax of
       the attribute *)
  let errors =
    Lcmap.fold
      (fun attrname attr errors ->
	 let attrname = Lcstring.to_string attrname in
	   try
	     begin match lookupMatchingRule schema `Equality attr with
		 Some mrule -> errors
	       | None -> (* try it by syntax *)
		   if Oidmap.mem attr.at_syntax Ldap_matchingrules.equality_bysyntax then errors
		   else (`Attribute attrname, No_equality_matching_rule_defined) :: errors
	     end
	   with _ -> (`Attribute attrname, No_equality_matching_rule_defined) :: errors)
      schema.attributes
      errors
  in
    errors
