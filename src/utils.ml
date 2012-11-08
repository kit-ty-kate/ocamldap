  let star_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\2a")
  let lparen_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\28")
  let rparen_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\29")
  let backslash_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\5c")
  let null_escape_rex = Pcre.regexp ~study:true ("\\" ^ "\\00")
  let unescape s =
    (Pcre.qreplace ~rex:star_escape_rex ~templ:"*"
       (Pcre.qreplace ~rex:lparen_escape_rex ~templ:"("
          (Pcre.qreplace ~rex:rparen_escape_rex ~templ:")"
             (Pcre.qreplace ~rex:null_escape_rex ~templ:"\000"
                (Pcre.qreplace ~rex:backslash_escape_rex ~templ:"\\" s)))))
