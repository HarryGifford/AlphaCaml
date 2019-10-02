(* A pretty-printer. *)

open Fsub.Raw

val printty: type_expr -> unit

val printtm: term_expr -> unit

val prtop: toplevel -> unit

