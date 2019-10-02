(* This module helps report errors and maintains some information
   about the source file that is being read. *)

val filename: string option ref

val verbose: bool ref

val error1: Lexing.position -> string -> 'a

val error2: Lexing.position -> Lexing.position -> string -> 'a

val signal2: Lexing.position -> Lexing.position -> string -> unit

val file: in_channel option ref

val get_file: unit -> in_channel

