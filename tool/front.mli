(* This module parses the command line, opens the input file,
   and parses it. *)

(* The file's base name. *)

val basename: string

(* The prologue found in the file. *)

val prologue: string

(* The declarations found in the file. *)

val declarations: Syntax.declaration list

