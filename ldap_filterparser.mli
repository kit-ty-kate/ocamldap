type token =
  | WHSP
  | LPAREN
  | RPAREN
  | AND
  | OR
  | NOT
  | EOF
  | ATTREQUAL of (string * string)
  | ATTREQUALSUB of (string * Ldap_types.substring_component)
  | ATTRGTE of (string * string)
  | ATTRLTE of (string * string)
  | ATTRAPPROX of (string * string)
  | ATTRPRESENT of (string)
  | ATTREXTENDEDMATCH of (string * string * string)
  | ATTREXTENDEDDN of (string * string option * string)

val filter_and_eof :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ldap_types.filter
