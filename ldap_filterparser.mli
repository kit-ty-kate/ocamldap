type token =
  | WHSP
  | LPAREN
  | RPAREN
  | AND
  | OR
  | NOT
  | EQUAL
  | APPROX
  | GTE
  | LTE
  | STAR
  | EOF
  | ATTR of (string)
  | EXTENDEDMATCHATTR of (string * string)
  | EXTENDEDDNATTR of (string * string option)
  | VALUE of (string)

val filter :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ldap_types.filter
