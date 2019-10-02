{

  open Error
  open Lexing
  open Parser

  (* Updates the line counter, which is used in some error messages. *)

  let update_loc lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  (* Extracts a chunk out of the source file. *)    

  let chunk ofs1 ofs2 =
    let len = ofs2 - ofs1 in
    let c = Error.get_file() in
    let p = pos_in c in
    seek_in c ofs1;
    let buffer = Bytes.create len in
    really_input c buffer 0 len;
    seek_in c p;
    Bytes.to_string buffer

}

let newline = ('\010' | '\013' | "\013\010")

let blank = [' ' '\009' '\012']

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
| "of"
    { OF }
| "type"
    { TYPE }
| "binds"
    { BINDS }
| "inner"
    { INNER }
| "outer"
    { OUTER }
| "neutral"
    { NEUTRAL }
| "sort"
    { SORT }
| "atom"
    { ATOM }
| "container"
    { CONTAINER }
| "with"
    { WITH }
| "and"
    { AND }
| "module"
    { MODULE }
| "identifier"
    { IDENTIFIER }
| (lowercase identchar *) as id
    { LID (lexeme_start_p lexbuf, lexeme_end_p lexbuf, id) }
| (uppercase identchar *) as id
    { UID (lexeme_start_p lexbuf, lexeme_end_p lexbuf, id) }
| "."
    { DOT }
| "|"
    { BAR }
| "="
    { EQUAL }
| ":"
    { COLON }
| ";"
    { SEMICOLON }
| "*"
    { STAR }
| ","
    { COMMA }
| "{"
    { LBRACE }
| "}"
    { RBRACE }
| "["
    { ocaml (lexeme_start_p lexbuf) lexbuf }
| "<"
    { LANGLE }
| ">"
    { RANGLE }
| "'"
    { QUOTE }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| eof
    { EOF }
| newline
    { update_loc lexbuf; token lexbuf }
| blank +
    { token lexbuf }
| "(*"
    { comment (lexeme_start_p lexbuf) lexbuf; token lexbuf }
| _
    { error2 (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) "Lexical error: unexpected character(s)." }

(* Collect O'Caml code, delimited by brackets. *)

and ocaml openingpos = parse
| '['
    { let _ = ocaml (lexeme_start_p lexbuf) lexbuf in ocaml openingpos lexbuf }
| ']'
    { OCAML (chunk (openingpos.pos_cnum + 1) (lexeme_start lexbuf)) }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; ocaml openingpos lexbuf }
| "'"
    { char lexbuf; ocaml openingpos lexbuf }
| "(*"
    { comment (lexeme_start_p lexbuf) lexbuf; ocaml openingpos lexbuf }
| newline
    { update_loc lexbuf; ocaml openingpos lexbuf }
| eof
    { error1 openingpos "Lexical error: unterminated Objective Caml code." }
| _
    { ocaml openingpos lexbuf }

(* Skip O'Caml comments. Comments can be nested and can contain
   strings or characters, which must be correctly analyzed. (A string
   could contain begin-of-comment or end-of-comment sequences, which
   must be ignored; a character could contain a begin-of-string
   sequence.) *)

and comment openingpos = parse
| "*)"
    { () }
| "(*"
    { comment (lexeme_start_p lexbuf) lexbuf; comment openingpos lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; comment openingpos lexbuf }
| "'"
    { char lexbuf; comment openingpos lexbuf }
| newline
    { update_loc lexbuf; comment openingpos lexbuf }
| eof
    { error1 openingpos "Lexical error: unterminated comment." }
| _
    { comment openingpos lexbuf }

(* Skip O'Caml strings. *)

and string openingpos = parse
| '"' 
   { () }
| '\\' _
   (* Upon finding a backslash, skip the character that follows.
      Pretty crude, but should work. *)
   { string openingpos lexbuf }
| newline
   { update_loc lexbuf; string openingpos lexbuf }
| eof 
   { error1 openingpos "Lexical error: unterminated string." }
| _
   { string openingpos lexbuf }

(* Skip O'Caml characters. A lone quote character is legal inside
   a comment, so if we don't recognize the matching closing quote,
   we simply abandon. *)

and char = parse
| '\\'? newline "'"
   { update_loc lexbuf }
| [^ '\\' '\''] "'"
| '\\' _ "'"
| '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
| ""
   { () } 
