{

  open Parser

}

rule main = parse
| [' ' '\009' '\012' '\r' '\n' ]+
    { main lexbuf }
| "/*"
    { comment (Lexing.lexeme_start lexbuf) lexbuf; main lexbuf }
| "lambda"
    { LAMBDA }
| "Top"
    { TTOP }
| "<:"
    { LEQ }
| "All"
    { ALL }
| "let"
    { LET }
| "in"
    { IN }
| "_"
    { USCORE }
| "."
    { DOT }
| ";"
    { SEMI }
| ","
    { COMMA }
| ":"
    { COLON }
| "="
    { EQ }
| "["
    { LSQUARE } 
| "{"
    { LCURLY } 
| "("
    { LPAREN } 
| "}"
    { RCURLY }
| ")"
    { RPAREN }
| "]"
    { RSQUARE }
| "->"
    { ARROW }
| (['a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*) as str
    { LCID str }
| (['A'-'Z'] ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*) as str
    { UCID str }
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

