open Ldap_types

let err2string code = 
  match code with
      `SUCCESS -> "success"
    | `OPERATIONS_ERROR -> "operations error"
    | `PROTOCOL_ERROR -> "protocol error"
    | `TIMELIMIT_EXCEEDED -> "time limit exceeded"
    | `SIZELIMIT_EXCEEDED -> "size limit exceeded"
    | `COMPARE_FALSE -> "compare false"
    | `COMPARE_TRUE -> "compare true"
    | `AUTH_METHOD_NOT_SUPPORTED -> "auth method not supported"
    | `STRONG_AUTH_REQUIRED -> "strong auth required"
    | `REFERRAL -> "referral"
    | `ADMINLIMIT_EXCEEDED -> "admin limit exceeded"
    | `UNAVAILABLE_CRITICAL_EXTENSION -> "unavilable critical extension"
    | `CONFIDENTIALITY_REQUIRED -> "confidentiality required"
    | `SASL_BIND_IN_PROGRESS -> "sasl bind in progress"
    | `NO_SUCH_ATTRIBUTE -> "no such attribute"
    | `UNDEFINED_TYPE -> "undefined type"
    | `INAPPROPRIATE_MATCHING -> "inappropriate matching"
    | `CONSTRAINT_VIOLATION -> "constraint violation"
    | `TYPE_OR_VALUE_EXISTS -> "type or value exists"
    | `INVALID_SYNTAX -> "invalid syntax"
    | `NO_SUCH_OBJECT -> "no such object"
    | `ALIAS_PROBLEM -> "alias problem"
    | `INVALID_DN_SYNTAX -> "invalid dn syntax"
    | `ALIAS_DEREF_PROBLEM -> "alias deref problem"
    | `INAPPROPRIATE_AUTH -> "inappropriate auth"
    | `INVALID_CREDENTIALS -> "invalid credentials"
    | `INSUFFICIENT_ACCESS -> "insufficient access"
    | `BUSY -> "busy"
    | `UNAVAILABLE -> "unavailable"
    | `UNWILLING_TO_PERFORM -> "unwilling to perform"
    | `LOOP_DETECT -> "loop detected"
    | `NAMING_VIOLATION -> "naming violation"
    | `OBJECT_CLASS_VIOLATION -> "object class violation"
    | `NOT_ALLOWED_ON_NONLEAF -> "not allowed on non leaf"
    | `NOT_ALLOWED_ON_RDN -> "not allowed on rdn"
    | `ALREADY_EXISTS -> "already exists"
    | `NO_OBJECT_CLASS_MODS -> "no objectclass mods"
    | `LOCAL_ERROR -> "local error"
    | `SERVER_DOWN -> "server down"
    | `OTHER -> "other"
    | _ -> raise (LDAP_Decoder "invalid error code")

let ldap_strerror error ldaperror = 
  match ldaperror with
      LDAP_Failure (code, error, {ext_matched_dn=mdn;ext_referral=refs}) ->
	("ldap error. code: " ^ (err2string code) ^ 
	   ", error: " ^ error ^ 
	   ", matched dn: " ^ mdn ^ 
	   (match refs with
		Some lst ->
		  List.fold_left
		    (fun s item -> 
		       if s = "" then
			 item
		       else
			 s ^ ", " ^ item)
		    "" lst
	      | None -> ""))
    | _ -> failwith "not an ldap error"

let ldap_perror error ldaperror = 
  prerr_endline (ldap_strerror error ldaperror)
