(********************************************************************************)
(********************************************************************************)
(********************************************************************************)
(* A schema checking entry:
   An entry which validates its validity against the server's
   schema *)

(* for the schema checker, should never be seen by
   the user *)

open Ldap_schema
open Ldap_types
open Ldap_matchingrules

type sc_violation_data = {
  missing_attributes: Oidset.t;
  illegal_attributes: Oidset.t;
  missing_objectclasses: Oidset.t;
  illegal_objectclasses: Oidset.t;
  single_value_violations: Oidset.t
}

exception Cannot_construct_attribute of string * string
exception No_such_attribute of string
exception Objectclass_is_required
exception Schema_violation of sc_violation_data
exception Invalid_matching_rule_syntax of Oid.t * Oid.t
exception Unknown_syntax of Oid.t
exception Unknown_matching_rule of Oid.t

let rec setOfList ?(set=Oidset.empty) list = 
  match list with
      a :: tail -> setOfList ~set:(Oidset.add a set) tail
    | []  -> set

class scldapentry ?(from_entry=new Ldap_ooclient.ldapentry) schema =
object (self)
  val mutable dn = ""
  val mutable data = Oidmap.empty
  val mutable changes = []
  val mutable changetype = `ADD
  val mutable must = Oidset.empty
  val mutable may = Oidset.empty
  val mutable presentOcs = Oidset.empty

  initializer
    self#of_entry from_entry

  method private check data =
    let presentOcs' =
      setOfList 
	(List.rev_map 
	   (ocNameToOid schema)
	   (try (Oidmap.find (Oid.of_string "2.5.4.0") data)#values (* objectclass *)
	    with Not_found -> raise Objectclass_is_required))
    in
    let presentOcslst = Oidset.elements presentOcs' in
    let (present, singleValue) = 
      Oidmap.fold 
	(fun k v (attrs, svattrs) -> 
	   (Oidset.add k attrs,
	    if (oidToAttr schema k).at_single_value && v#cardinal > 1 then
	      Oidset.add k svattrs (* violations of single value assertions *)
	    else svattrs))
	data 
	(Oidset.empty, Oidset.empty)
    in
    let (must', may') =
      List.fold_left
	(fun (must, may) oc ->
	   let mkset l = setOfList (List.rev_map (attrNameToOid schema) l) in
	   let {oc_must=oc_must;oc_may=oc_may} = oidToOc schema oc in
	     (Oidset.union must (mkset oc_must), Oidset.union may (mkset oc_may)))
	(Oidset.empty, Oidset.empty)
	presentOcslst
    in
    let all_allowed = Oidset.union must' may' in
    let missingAttrs = Oidset.diff must' (Oidset.inter must' present) in
    let illegalAttrs = Oidset.diff present (Oidset.inter all_allowed present) in
    let rec lstRequired schema oid =
      oid :: (List.flatten 
		(List.rev_map 
		   (fun sup -> lstRequired schema (ocNameToOid schema sup))
		   (oidToOc schema oid).oc_sup))
    in
    let requiredOcs =
      setOfList 
	(List.flatten
	   (List.rev_map 
	      (lstRequired schema) 
	      presentOcslst))
    in
    let missingOcs = Oidset.diff requiredOcs (Oidset.inter requiredOcs presentOcs') in
    let missingOcslst = Oidset.elements missingOcs in
    let illegalOcs = 
      setOfList
	(List.filter 
	   (fun oid ->
	      let supchain = lstRequired schema oid in
		List.exists
		  (fun missing -> 
		     List.exists 
		       (fun sup -> Oid.compare sup missing = 0) 
		       supchain)
		  missingOcslst)
	   presentOcslst)
    in
      if 
	(not 
	   (Oidset.is_empty 
	      (Oidset.union 
		 (Oidset.union 
		    missingAttrs 
		    (Oidset.union
		       illegalAttrs
		       singleValue))
		 illegalOcs)))
      then
	raise (Schema_violation 
		 {missing_attributes=missingAttrs;
		  illegal_attributes=illegalAttrs;
		  missing_objectclasses=missingOcs;
		  illegal_objectclasses=illegalOcs;
		  single_value_violations=singleValue})
      else begin
	must <- must';
	may <- may';
	presentOcs <- presentOcs'
      end

  method private normalize_ops ops =
    List.rev_map
      (fun (name, vals) -> (attrNameToAttr schema name, vals))
      ops
      
  method private new_attribute ({at_oid=atoid;at_name=name;at_syntax=syn} as attr) =
    let ordering_match = 
      match lookupMatchingRule schema `Ordering attr with
	  Some oid ->
	    (try 
	       let (syntax, check, mrule) = Oidmap.find oid Ldap_matchingrules.ordering in
		 if (not check) || Oid.compare syn syntax = 0 then Some mrule
		 else raise (Invalid_matching_rule_syntax (oid, syn))
	     with Not_found -> raise (Unknown_matching_rule oid))
	| None -> 
	    (try Some (Oidmap.find syn Ldap_matchingrules.ordering_bysyntax)
	     with Not_found -> None)
    in
    let substring_match = 
      match lookupMatchingRule schema `Substring attr with
	  Some oid ->
	    (try
	       let (syntax, check, mrule) = Oidmap.find oid Ldap_matchingrules.substring in
		 if (not check) || Oid.compare syn syntax = 0 then Some mrule
		 else raise (Invalid_matching_rule_syntax (oid, syn))
	     with Not_found -> raise (Unknown_matching_rule oid))
	| None ->
	    (try Some (Oidmap.find syn Ldap_matchingrules.substring_bysyntax)
	     with Not_found -> None)
    in
    let attribute_constructor = 
      match lookupMatchingRule schema `Equality attr with
	  Some oid -> 
	    (try 
	       let (syntax, check, constructor) = Oidmap.find oid Ldap_matchingrules.equality in
		 if (not check) || Oid.compare syn syntax = 0 then constructor
		 else raise (Invalid_matching_rule_syntax (oid, syn))
	     with Not_found -> raise (Unknown_matching_rule oid))	    
	| None ->
	    (try Oidmap.find syn Ldap_matchingrules.equality_bysyntax
	     with Not_found -> (* use a binary compare as the rule of last resort *)
	       Oidmap.find 
		 (Oid.of_string "1.3.6.1.4.1.1466.115.121.1.40")
		 Ldap_matchingrules.equality_bysyntax)
    in
      try 
	attribute_constructor
	  ~ordering:ordering_match
	  ~substrings:substring_match
	  (Oidmap.find syn Ldap_syntaxes.syntaxes)
      with Not_found -> raise (Unknown_syntax syn)

  method private commit_changes data' ops =
    self#check data';
    if not (changetype = `ADD) then changes <- ops @ changes;
    data <- data'

  method private add' data ops =
    List.fold_left
      (fun data -> 
	 function (attr, []) -> data
	   | ({at_oid=oid} as attr, values) ->
	       let attr_object = 
		 try Oidmap.find oid data
		 with Not_found ->
		   self#new_attribute attr
	       in
	       let attr_object' = 
		 List.fold_left 
		   (fun (attr_object: attribute_t) v -> attr_object#add v)
		   attr_object
		   values
	       in
		 Oidmap.add oid attr_object' data)
      data
      (self#normalize_ops ops)

  method add ops = 
    let data' = self#add' data ops in
      self#commit_changes data' (List.rev_map (fun (a, v) -> (`ADD, a, v)) ops)

  method propose_add ops = self#check (self#add' data ops)

  method private delete' data ops = 
    List.fold_left
      (fun data ({at_oid=oid}, (values: string list)) ->
	 let (attr_obj: Ldap_matchingrules.attribute_t) = 
	   try Oidmap.find oid data
	   with Not_found -> 
	     raise (No_such_attribute (oidToAttrName schema oid))
	 in
	   if values = [] then
	     Oidmap.remove oid data
	   else
	     let attr_obj' = 
	       List.fold_left 
		 (fun (attr_obj: attribute_t) v -> attr_obj#delete v)
		 attr_obj
		 values
	     in
	       if attr_obj'#cardinal = 0 then Oidmap.remove oid data
	       else Oidmap.add oid attr_obj' data)
      data
      (self#normalize_ops ops)

  method delete ops = 
    let data' = self#delete' data ops in
      self#commit_changes data' (List.rev_map (fun (a, v) -> (`DELETE, a, v)) ops)

  method propose_delete ops = self#check (self#delete' data ops)

  method private replace' data ops = 		   
    List.fold_left
      (fun data ({at_oid=oid} as attr, values) ->
	 let attr_obj =
	   try Oidmap.find oid data
	   with Not_found ->
	     self#new_attribute attr
	 in
	   if values = [] then Oidmap.remove oid data
	   else Oidmap.add oid (attr_obj#replace values) data)
      data
      (self#normalize_ops ops)

  method replace ops =
    let data' = self#replace' data ops in
      self#commit_changes data' (List.rev_map (fun (a, v) -> (`REPLACE, a, v)) ops)

  method propose_replace ops = self#check (self#replace' data ops)

  method private modify' data ops = 
    List.fold_left
      (fun data ->
	 function (`ADD, attr, vals) -> self#add' data [(attr, vals)]
	   | (`DELETE, attr, vals) -> self#delete' data [(attr, vals)]
	   | (`REPLACE, attr, vals) -> self#replace' data [(attr, vals)])
      data
      ops
	
  method modify ops = 
    let data' = self#modify' data ops in
      self#commit_changes data' ops

  method propose_modify ops = self#check (self#modify' data ops)

  method exists a = 
    try ignore (Oidmap.find (attrNameToOid schema a) data);true
    with Not_found -> false

  method get_value a = (Oidmap.find (attrNameToOid schema a) data)#values
  method changes = changes
  method changetype = changetype
  method set_changetype (c: Ldap_ooclient.changetype) = changetype <- c
  method flush_changes = changes <- []
  method dn = dn
  method set_dn dn' = 
    ignore (Ldap_dn.of_string dn'); (* check for validity *)
    dn <- dn'
  method print = print_endline "This deprecated method has been removed"
  method diff (e: Ldap_ooclient.ldapentry_t) = 
    ([]: (Ldap_types.modify_optype * string * string list) list)

  method of_entry (e: Ldap_ooclient.ldapentry_t) =
    let data' =
      List.fold_left
	(fun data attr -> self#add' data [(attr, e#get_value attr)])
	data
	e#attributes
    in
    let dn' = dn in
      try
	self#set_dn e#dn;
	self#commit_changes data' [];
	changetype <- e#changetype
      with exn -> dn <- dn'

  method private oid_lst_to_name_lst lst = 
    List.rev_map
      (fun oid -> oidToAttrName schema oid)
      lst

  method attributes_byoid =
    Oidmap.fold (fun k v s -> Oidset.add k s) data Oidset.empty
  method attributes = self#oid_lst_to_name_lst (Oidset.elements self#attributes_byoid)

  method objectclasses_byoid = presentOcs
  method objectclasses = self#oid_lst_to_name_lst (Oidset.elements self#objectclasses_byoid)

  method must = self#oid_lst_to_name_lst (Oidset.elements must)      
  method must_byoid = must
  method may = self#oid_lst_to_name_lst (Oidset.elements may)
  method may_byoid = may

  method attributes_not_allowed_byoid = 
    let oids = setOfList (Oidmap.fold (fun k v l -> k :: l) schema.attributes_byoid []) in
      Oidset.diff oids (Oidset.union must may)

  method attributes_not_allowed =
    self#oid_lst_to_name_lst 
      (Oidset.elements self#attributes_not_allowed_byoid)

  method match_filter filter = 
    let lookup_attr attrname = Oidmap.find (attrNameToOid schema attrname) data in
      match filter with
	  `And flist -> List.for_all self#match_filter flist
	| `Or flist -> List.exists self#match_filter flist
	| `Not filter -> not (self#match_filter filter)
	| `EqualityMatch {attributeDesc=attrname;assertionValue=asserted_value} ->
	    (try (lookup_attr attrname)#equality_match asserted_value
	     with Not_found -> false)
	| `Substrings {attrtype=attrname;substrings=subs} ->
	    (try (lookup_attr attrname)#substrings_match subs
	     with Not_found -> false)
	| `GreaterOrEqual {attributeDesc=attrname;assertionValue=asserted_value} ->
	    (try (lookup_attr attrname)#greater_than_or_equal_match asserted_value
	     with Not_found -> false)
	| `LessOrEqual {attributeDesc=attrname;assertionValue=asserted_value} ->
	    (try (lookup_attr attrname)#less_than_or_equal_match asserted_value
	     with Not_found -> false)
	| `Present attrname ->
	    (try ignore (lookup_attr attrname);true
	     with Not_found -> false)
	| `ApproxMatch {attributeDesc=attrname;assertionValue=asserted_value} ->
	    (try (lookup_attr attrname)#approximate_match asserted_value
	     with Not_found -> false)
	| `ExtensibleMatch {matchingRule=mruleoid;ruletype=rtype;
			    matchValue=asserted_value;dnAttributes=dnattrs} ->
	    failwith "extensible match is not implemented"

  method match_filterstring fstring = self#match_filter (Ldap_filter.of_string fstring)
end

exception Single_value of string list

(* schema checking flavor *)
type adaptiveflavor = Expansive (* Favor expansion of the object in order to make it legal. 
				   Attempt to find and add objectclasses
				   which make illegal attributes legal. *)
		      | Reductive (* delete any illegal attributes,
				     do not add objectclasses to
				     make them legal*)

class adaptivescldapentry ?(from_entry=new Ldap_ooclient.ldapentry) schema =
object (self)
  inherit scldapentry schema ~from_entry as super
  val mutable proposed_changes = []
  method adapt_proposed_changes flavor 
    (proposed_changes: (modify_optype * string * string list) list) =
    (* find an objectclass which allows attr *)
    let find_oc schema attr = 
      let find_in_oc oc attr = 
	try
	  (List.exists
	     (fun must -> compareAttrs schema attr must = 0)
	     oc.oc_must) || 
	    (List.exists
	       (fun may -> compareAttrs schema attr may = 0)
	       oc.oc_may) 
	with Invalid_attribute _ -> false
      in
	match
	  Oidmap.fold
	    (fun key valu oc -> if (find_in_oc valu attr) then Some key else oc)
	    schema.objectclasses_byoid
	    None
	with
	    Some oc -> oidToOcName schema oc
	  | None -> raise Not_found
    in
    let check_single_value singleValue =
      if not (Oidset.is_empty singleValue) then 
	raise 
	  (Single_value
	     (Oidset.fold 
		(fun elt l -> (oidToAttrName schema elt) :: l)
		singleValue
		[]))
    in
    let normalize_proposed_changes proposed_changes =
      List.fold_left
	(fun proposed_changes (op', attrname', vals') ->
	   let (matchingOps, rest) =
	     List.partition
	       (fun (op, attrname, _) -> 
		  op = op' && (compareAttrs schema attrname' attrname = 0))
	       proposed_changes
	   in
	     (List.fold_left
		(fun (op, attrname, vals) (_, _, newvals) -> 
		   (op, attrname, List.rev_append newvals vals))
		(op', attrname', vals')
		matchingOps) :: rest)
	[]
	(List.rev_map
	   (function (`REPLACE, attrname, []) -> (`DELETE, attrname, []) | op -> op)
	   proposed_changes)
    in
      (* Expansive adaptation means that we add things to the object
	 to make it fit the schema. This may mean that we simply don't
	 apply our proposed changes, for example, if we proposed the
	 deletion of objectclass: person, but it is required by
	 objectclasses or attributes on the object we should not
	 process that delete operation. *)
    let expansive_adapt (proposed_changes: (modify_optype * string * string list) list) =
      try super#propose_modify proposed_changes;proposed_changes
      with 
	  Schema_violation 
	    {missing_attributes=missing;illegal_attributes=illegal;
	     missing_objectclasses=missingOc;illegal_objectclasses=illegalOc;
	     single_value_violations=singleValue} ->
	      check_single_value singleValue;
	      let (proposed_changes', missing, missingOc, illegal) =
		List.fold_left
		  (fun (mods, missing, missingOc, illegal) op ->
		     match op with
			 (`DELETE, attrname, vals) when 
			   (Oidset.mem (attrNameToOid schema attrname) missing) -> 
			     (mods, Oidset.remove (attrNameToOid schema attrname) missing,
			      missingOc, illegal)
		       | (`DELETE, attrname, vals) when 
			   compareAttrs schema "objectClass" attrname = 0 ->
			   let (vals, missingOc, illegal) = 
			     List.fold_left
			       (fun (vals, missingOc, illegal) (v: string) -> 
				  let {oc_oid=oid;oc_must=must;oc_may=may} = ocNameToOc schema v in
				  let allowed = 
				    setOfList 
				      (List.rev_map
					 (attrNameToOid schema)
					 (List.rev_append must may))
				  in
				    if (Oidset.mem oid missingOc) then
				      (vals, Oidset.remove oid missingOc, illegal)
				    else if not (Oidset.is_empty (Oidset.inter allowed illegal)) then
				      (vals, missingOc, Oidset.diff illegal allowed)
				    else
				      (v :: vals, missingOc, illegal))
			       ([], missingOc, illegal)
			       vals
			   in
			     if vals = [] then 
			       (mods, missing, missingOc, illegal)
			     else 
			       ((`DELETE, "objectClass", vals) :: mods, 
				missing, missingOc, illegal)
		       | _ -> (op :: mods, missing, missingOc, illegal))
		  ([], missing, missingOc, illegal)
		  proposed_changes
	      in
		(List.rev_append
		   (if not (Oidset.is_empty illegal) then
		      [(`ADD, "objectClass",
			(List.rev_map 
			   (find_oc schema) 
			   (List.rev_map 
			      (oidToAttrName schema)
			      (Oidset.elements illegal))))]
		    else [])
		   (if not (Oidset.is_empty missingOc) then
		      (`ADD, "objectClass", 
		       List.rev_map 
			 (oidToOcName schema)
			 (Oidset.elements missingOc)) :: proposed_changes'
		    else proposed_changes'))
    in
    let reductive_adapt proposed_changes = 
      try super#propose_modify proposed_changes;[]
      with
	  Schema_violation
	    {missing_attributes=missing;illegal_attributes=illegal;
	     missing_objectclasses=missingOc;illegal_objectclasses=illegalOc;
	     single_value_violations=singleValue} ->
	      check_single_value singleValue;
	      let (proposed_changes', attrsDeleting) =
		List.fold_left
		  (fun (modifications, attrs) ((op, attrname, values) as modification) ->
		     match op with
			 `ADD | `REPLACE ->
			   if compareAttrs schema attrname "objectclass" = 0 then
			     ((op, attrname, 
			       List.filter 
				 (fun value -> 
				    not (Oidset.mem (ocNameToOid schema value) illegalOc))
				 values) :: modifications,
			      attrs)
			   else if Oidset.mem (attrNameToOid schema attrname) illegal then
			     (modifications, attrs)
			   else 
			     (modification :: modifications, attrs)
		       | `DELETE ->
			   if compareAttrs schema attrname "objectclass" = 0 then
			     ((op, attrname,
			       List.rev_map
				 (oidToOcName schema)
				 (Oidset.elements
				    (Oidset.union 
				       illegalOc
				       (setOfList 
					  (List.rev_map 
					     (ocNameToOid schema) 
					     values))))) :: modifications,
			      attrs)
			   else 
			     (modification :: modifications,
			      if values = [] then 
				Oidset.add (attrNameToOid schema attrname) attrs
			      else attrs))
		  ([], Oidset.empty)
		  (normalize_proposed_changes proposed_changes)
	      in
		List.rev_append
		  (List.rev_map
		     (fun attroid -> (`DELETE, oidToAttrName schema attroid, []))
		     (Oidset.elements (Oidset.diff illegal attrsDeleting)))
		  proposed_changes'

    in
      match flavor with 
	  Expansive -> expansive_adapt proposed_changes
	| Reductive -> reductive_adapt proposed_changes

end;;

(*
  method private drive_updatecon =
  try self#update_condition
  with 
  Invalid_objectclass(s) -> super#delete [("objectClass",[s])];self#drive_updatecon
  | Invalid_attribute(s) -> super#delete [(s,[])];self#drive_updatecon
  | Objectclass_is_required -> super#add [("objectClass", ["top"])]
*)	  

(*
  method private drive_reconsile flavor =
    try self#reconsile_illegal flavor
    with Invalid_attribute(a) -> (* remove attributes for which there is no objectclass *)
      (super#delete [(a, [])];
       self#drive_updatecon;
       self#drive_reconsile flavor)

  method of_entry ?(scflavor=Pessimistic) (e:ldapentry) =
    self#drive_reconsile scflavor

  (* raise an exception if the user attempts to have more than
     one value in a single valued attribute. *)
  method private single_val_check (x:op_lst) consider_present =
    let check op =
      let attr = oidToAttr schema (Oid.of_string (fst op)) in
	(if attr.at_single_value then
	   (match op with
		(attr, v1 :: v2 :: tail) -> false
	      | (attr, v1 :: tail) -> 
		  (if consider_present && (super#exists attr) then
		     false
		   else true)
	      | _ -> true)
	 else true)
    in
      match x with
	  op :: tail -> (if not (check op) then
			   raise (Single_value (fst op))
			 else self#single_val_check tail consider_present)
	|  [] -> ()

  method add x = 
    self#single_val_check x true;super#add x;
    self#drive_updatecon;self#drive_reconsile Optimistic
      
  method delete x = 
    super#delete x;self#drive_updatecon;self#drive_reconsile Pessimistic

  method replace x = 
    self#single_val_check x false;super#replace x;
    self#drive_updatecon;self#drive_reconsile Optimistic

  method modify x = 
    let filter_mod x op = 
      List.rev_map
	(fun (_, a, v) -> (a, v))
	(List.filter 
	   (function (the_op, _, _) when the_op = op -> true | _ -> false) x)
    in
      self#single_val_check (filter_mod x `ADD) true;
      self#single_val_check (filter_mod x `REPLACE) false;
      super#modify x;
      self#drive_updatecon;
      self#drive_reconsile Pessimistic

  method get_value x =
    let values = 
      List.fold_left
	(fun v name -> 
	   try super#get_value name 
	   with Not_found ->
	     if (Setstr.mem (attrToOid schema (Lcstring.of_string name)) missingAttrs) then
	       ["required"]
	     else v)
	[]
	(getAttr (Lcstring.of_string x)).at_name
    in
      match values with
	  [] -> raise Not_found
	| values -> values

  method attributes =
    List.rev_append
      super#attributes
      (List.rev_map
	 (fun a -> oidToAttr schema a) 
	 (Setstr.elements missingAttrs))

  method list_missing = Setstr.elements missingAttrs
  method list_allowed = Setstr.elements all_allowed
  method list_present = Setstr.elements present
  method is_missing x = 
    Setstr.mem (attrToOid schema (Lcstring.of_string x)) missingAttrs
  method is_allowed x = 
    Setstr.mem (attrToOid schema (Lcstring.of_string x)) all_allowed
*)


(*
(********************************************************************************)
(********************************************************************************)
(********************************************************************************)
(* a high level interface for accounts, and services in the directory *)

type generator = {gen_name:string;
		  required:string list;
		  genfun:(ldapentry_t -> string list)};;

type service = {svc_name: string;
		static_attrs: (string * (string list)) list;
		generate_attrs: string list;
		depends: string list};;

type generation_error = Missing_required of string list
			| Generator_error of string

exception No_generator of string;;
exception Generation_failed of generation_error;;
exception No_service of string;;
exception Service_dep_unsatisfiable of string;;
exception Generator_dep_unsatisfiable of string * string;;
exception Cannot_sort_dependancies of (string list);;

let diff_values convert_to_oid convert_from_oid attr attrvals svcvals =
    (attr, (List.rev_map
	      convert_from_oid
	      (Oidset.elements
		 (Oidset.diff
		    svcvals
		    (Oidset.inter svcvals attrvals)))))

(* compute the intersection of values between an attribute and a service,
   you need to pass this function as an argument to apply_set_op_to_values *)
let intersect_values convert_to_oid convert_from_oid attr attrvals svcvals =
  (attr, (List.rev_map
	    convert_from_oid
	    (Oidset.elements
	       (Oidset.inter svcvals attrvals))))

(* this function allows you to apply a set operation to the values of an attribute, and 
   the static values on a service *)
let apply_set_op_to_values schema (attr:string) e svcval opfun =
  let lc = String.lowercase in
  let convert_to_oid = (match lc ((getAttr schema (Lcstring.of_string attr)).at_equality) with
			    "objectidentifiermatch" -> 
			      (fun oc -> ocToOid schema (Lcstring.of_string oc))
			  | "caseexactia5match" -> Oid.of_string
			  | _ -> (fun av -> Oid.of_string (lc av)))
  in
  let convert_from_oid = (match lc ((getAttr schema (Lcstring.of_string attr)).at_equality) with
			      "objectidentifiermatch" -> (fun av -> oidToOc schema av)
			    | "caseexactia5match" -> Oid.to_string
			    | _ -> Oid.to_string)
  in
  let attrvals = setOfList
		   (List.rev_map
		      convert_to_oid
		      (try e#get_value attr with Not_found -> []))
  in
  let svcvals = setOfList (List.rev_map convert_to_oid (snd svcval))
  in
    opfun convert_to_oid convert_from_oid attr attrvals svcvals

class ldapaccount 
  schema 
  (generators:(string, generator) Hashtbl.t)
  (services:(string, service) Hashtbl.t) =
object (self)
  inherit scldapentry schema as super
  val mutable toGenerate = Oidset.empty
  val mutable neededByGenerators = Oidset.empty
  val services = services
  val generators = generators

(* evaluates the set of missing attributes to see if any of
   them can be generated, if so, it adds them to be generated *)
  method private resolve_missing =
    (* computes the set of generateable attributes *)
    let generate_togenerate generators missing togenerate =
      (* generators have dependancies. Some of the dependancies can
	 also be generated. We can generate a dependancy if the following
	 conditions are met. 
	 1. The dependancy is in the generators hash (it has a generation function)
	 2. The dependancy is allowed by the schema (it is either a must or may of
	 an objectclass currently on the object)
	 3. The dependancy is not already present (if it is present already then it
	 has already been satisfied, and there is no need to generate it) *)
      let find_generatable_dep generators generator =
	(List.rev_map
	   (fun e -> attrToOid schema (Lcstring.of_string e))
	   (List.filter
	      (fun g ->
		 if ((Hashtbl.mem generators g) && 
		     (not (Oidset.mem
			     (attrToOid schema (Lcstring.of_string g))
			     (setOfList self#list_present)))) then
		   true
		 else false)
	      (List.filter (* we can only add it if it is allowed by the schema *)
		 (fun attr -> super#is_allowed attr)
		 (Hashtbl.find generators generator).required)))
      in
	(* collect a flat list of all generatable dependancies *)
      let rec find_generatable_deps generators genlst =
	(List.flatten
	   (List.rev_map
	      (find_generatable_dep generators)
	      genlst))
      in
	(* the set we are currently generating, union the set of missing attributes which we
	   can generate. *)
      let generateing = (List.filter
			   (fun gen -> 
			      if (Hashtbl.mem generators (lowercase (oidToAttr schema gen))) then
				true
			      else false)
			   (List.rev_append
			      missing
			      (Oidset.elements togenerate)))
      in
	(* the total set of generatable at any point in time is. The set
	   we are already generating, unioned with any generatable dependancies, unioned
	   with the set of missing attributes (required by the schema) which can be generated. 
	   Note, the last union is done in the generateing expression above. *)
	setOfList
	  (List.rev_append generateing (find_generatable_deps
					  generators
					  (List.rev_map
					     (fun e -> lowercase (oidToAttr schema e))
					     generateing)))
    in
    let generate_missing togen generators =
      setOfList
	(Hashtbl.fold 
	   (fun key valu requiredlst -> 
	      if Oidset.mem (attrToOid schema (Lcstring.of_string valu.gen_name)) togen then
		List.rev_append
		  requiredlst
		  (List.rev_map
		     (fun x -> try
			attrToOid schema (Lcstring.of_string x)
		      with Invalid_attribute a -> 
			raise (Generator_dep_unsatisfiable (key, a)))
		     valu.required)
	      else
		requiredlst)
	   generators [])
    in
      toGenerate <- generate_togenerate generators super#list_missing toGenerate;
      neededByGenerators <- generate_missing toGenerate generators;

  method list_missing = 
    let allmissing = 
      Oidset.union neededByGenerators (setOfList super#list_missing) 
    in
      Oidset.elements
	(Oidset.diff
	   allmissing 
	   (Oidset.inter
	      allmissing
	      (Oidset.union 
		 toGenerate 
		 (setOfList super#list_present))))

  method attributes =
    (List.rev_map (oidToAttr schema)
       (Oidset.elements
	  (Oidset.union toGenerate
	     (setOfList 
		(List.rev_map
		   (fun a -> attrToOid schema (Lcstring.of_string a))
		   super#attributes)))))

  method is_missing x = (not (Oidset.mem
				(attrToOid schema (Lcstring.of_string x)) 
				toGenerate)) 
			|| (super#is_missing x)

  method generate =
    let sort_genlst generators unsatisfied =
      let satisfied alreadysatisfied present deps =
	List.for_all
	  (fun dep -> 
	     (List.mem dep alreadysatisfied) || 
	     (List.mem (attrToOid schema (Lcstring.of_string dep)) (present)))
	  deps
      in
      let rec sort present ordtogen unsatisfied =
	match unsatisfied with
	    [] -> ordtogen
	  | todo ->
	      let (aresat, notyet) =
		(List.partition
		   (fun attr ->
		      (satisfied ordtogen present
			 (Hashtbl.find generators attr).required))
		   todo)
	      in
		match aresat with
		    [] -> raise (Cannot_sort_dependancies notyet)
		  | _ -> sort present (ordtogen @ aresat) notyet
      in
	sort (self#list_present) [] unsatisfied
    in
      match self#list_missing with
	  [] -> 
	    (List.iter
	       (fun attr ->
		  self#add [(attr, (Hashtbl.find generators attr).genfun (self:>ldapentry_t))])
	       (sort_genlst generators
		  (List.rev_map
		     (fun elt -> String.lowercase (oidToAttr schema elt))
		     (Oidset.elements toGenerate))));
	    toGenerate <- Oidset.empty
	| a  -> raise (Generation_failed
			 (Missing_required (List.rev_map (oidToAttr schema) a)))

  method get_value x =
    if (Oidset.mem (attrToOid schema (Lcstring.of_string x)) toGenerate) then
      ["generate"]
    else
      super#get_value x

(* adapt the passed in service to the current state of the entry
   this may result in a service with applies no changes. The entry
   may already have the service. *)
  method adapt_service svc =    
      {svc_name=svc.svc_name;
       static_attrs=(List.filter
			  (fun cons ->
			     match cons with
				 (attr, []) -> false
			       | _          -> true)
			  (List.rev_map
			     (fun cons -> apply_set_op_to_values schema (fst cons) self cons diff_values)
			     svc.static_attrs));
       generate_attrs=(List.filter
			 (fun attr -> 
			    (try (ignore (super#get_value attr));false
			     with Not_found -> true))			
			 svc.generate_attrs);
       depends=svc.depends}

(* add a service to the account, if they already satisfy the service
   then do nothing *)			     
  method add_service svc =
    let service = try Hashtbl.find services (lowercase svc)
    with Not_found -> raise (No_service svc) in
      (try List.iter (self#add_service) service.depends
       with (No_service x) -> raise (Service_dep_unsatisfiable x));
      let adaptedsvc = self#adapt_service service in
	(let do_adds a =
	   let singlevalu = 
	     (List.filter 
		(fun attr -> (getAttr schema
			     (Lcstring.of_string (fst attr))).at_single_value) a)
	   in
	   let multivalued = 
	     (List.filter 
		(fun attr -> not (getAttr schema
				 (Lcstring.of_string (fst attr))).at_single_value) a)
	   in
	     self#add multivalued;
	     self#replace singlevalu
	 in
	   do_adds adaptedsvc.static_attrs);
	(match adaptedsvc.generate_attrs with
	     [] -> ()
	   | a  -> List.iter (self#add_generate) a)

  method delete_service svc =
    let find_deps services service =
      (Hashtbl.fold
	 (fun serv svcstruct deplst ->
	    if (List.exists ((=) service) svcstruct.depends) then
	      serv :: deplst
	    else
	      deplst)
	 services [])
    in
    let service = try Hashtbl.find services (lowercase svc)
    with Not_found -> raise (No_service svc) in
      (List.iter (self#delete_service) (find_deps services svc));
      (List.iter
	 (fun e -> match e with
	      (attr, []) -> ()
	    | a -> (try (ignore (super#get_value (fst a)));super#delete [a]
		    with Not_found -> ()))
	 (List.rev_map
	    (fun cons ->
	       apply_set_op_to_values schema (fst cons) self cons intersect_values)
	    service.static_attrs));
      (List.iter
	 (fun attr -> 
	    (try (match self#get_value attr with
		      ["generate"] -> self#delete_generate attr
		    | _ -> super#delete [(attr, [])])
	     with Not_found -> ()))
	 service.generate_attrs)	     	     

  method service_exists service =
    let service = (try (Hashtbl.find services service) 
		   with Not_found -> raise (No_service service))
    in
      match self#adapt_service service with
	  {svc_name=s;
	   static_attrs=[];
	   generate_attrs=[];
	   depends=d} -> (match d with
			      [] -> true
			    | d  -> List.for_all self#service_exists d)
	| _ -> false

  method services_present =
    Hashtbl.fold
      (fun k v l -> 
	 if self#service_exists v.svc_name then
	   v.svc_name :: l
	 else l)
      services []
      
  method of_entry ?(scflavor=Pessimistic) e = super#of_entry ~scflavor e;self#resolve_missing

  method add_generate x = 
    (if (Hashtbl.mem generators (lowercase x)) then
       toGenerate <- Oidset.add (attrToOid schema (Lcstring.of_string x)) toGenerate
     else raise (No_generator x));
    self#resolve_missing
  method delete_generate x =
    let find_dep attr generators =
      (Hashtbl.fold
	 (fun key valu deplst ->
	    if (List.exists ((=) attr) valu.required) then
	      key :: deplst
	    else
	      deplst)
	 generators [])
    in
      (List.iter (self#delete_generate) (find_dep x generators));
      toGenerate <- 
      Oidset.remove
	(attrToOid schema (Lcstring.of_string x)) toGenerate

  method add x = (* add x, remove all attributes in x from the list of generated attributes *)
    super#add x; 
    (List.iter 
      (fun a -> 
	 toGenerate <- (Oidset.remove
			  (attrToOid schema (Lcstring.of_string (fst a)))
			  toGenerate))
       x);
    self#resolve_missing
  method delete x = super#delete x;self#resolve_missing
  method replace x = (* replace x, removeing it from the list of generated attrs *)
    super#replace x;
    (List.iter
       (fun a -> 
	  toGenerate <- (Oidset.remove
			   (attrToOid schema (Lcstring.of_string (fst a)))
			   toGenerate))
       x);
    self#resolve_missing
end;;
*)
