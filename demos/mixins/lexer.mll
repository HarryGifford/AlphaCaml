{

  open Parser

}

rule main = parse
| [' ' '\009' '\012' '\r' '\n' ]+
    { main lexbuf }
| "/*"
    { comment (Lexing.lexeme_start lexbuf) lexbuf; main lexbuf }
| ("mixin" | "mix" )
    { MIXIN }
| "end"
    { END }
| "and"
    { AND }
| "let"
    { LET }
| "in"
    { IN }
| "close"
    { CLOSE }
| "delete"
    { DELETE }
| "fake"
    { FAKE }
| "depends"
    { DEPENDS }
| "on"
    { ON }
| "rec"
    { REC }
| "val"
    { VAL }
| "as"
    { AS }
| "+"
    { PLUS }
| "_"
    { WILDCARD }
| "."
    { DOT }
| "="
    { EQUAL }
| "{"
    { LCURLY } 
| "("
    { LPAREN } 
| "["
    { LSQUARE }
| "}"
    { RCURLY }
| ")"
    { RPAREN }
| "]"
    { RSQUARE }
| (['a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*) as str
    { LIDENT str }
| eof
    { EOF }
| _ 
    { failwith (Printf.sprintf "Illegal character at offset %d." (Lexing.lexeme_start lexbuf)) }

and comment start = parse
  "/*"
    { comment (Lexing.lexeme_start lexbuf) lexbuf; comment start lexbuf }
| "*/"
    { () }
| eof
    { failwith (Printf.sprintf "Unterminated comment at offset %d." start) }
| _
    { comment start lexbuf }

