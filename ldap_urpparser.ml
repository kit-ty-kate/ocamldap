open Ldap_urllexer
open Ldap_types

exception Parse_error of string

let parse_url_begin read_token =
  match read_token () with
      SCHEME ->
	(match read_token () with
	     COLONSLASHSLASH -> ()
	   | _ -> raise (Parse_error "expected ://"))
    | _ -> raise (Parse_error "expected \"ldap\"")

let rec parse_url read_token read url = 
  match read_token () with
      HOST h -> 
	(match read with
	     [] -> (parse_url
		      read_token 
		      ((HOST h) :: read) 
		      {url with url_host=(Some h)})
	   | _ -> raise (Parse_error "unexpected token HOST"))
    | PORT p -> 
	(match read with
	     [HOST _] -> (parse_url
			    read_token
			    ((PORT p) :: read)
			    {url with url_port=(Some p)})
	   | _ -> raise (Parse_error "unexpected token PORT"))
    | SLASH ->
	(match read with
	     [HOST _] | [PORT _;HOST _] | [] ->
	       parse_url read_token (SLASH :: read) url
	   | _ -> raise (Parse_error "unexpected token SLASH"))
    | DN d ->
	(match read with
	     [SLASH;HOST _] | [SLASH;PORT _;HOST _] | [SLASH] ->
	       (parse_url read_token
		  ((DN d) :: read)
		  {url with url_dn=(Some d)})
	   | _ -> raise (Parse_error "unexpected token DN"))
    | QUESTION -> parse_url read_token (QUESTION :: read) url
    | 
