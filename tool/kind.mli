(* This module performs kind checking, sanity checking,
   and infers information for use by the generator. *)

open Strings
open Syntax

(* The set of atom sorts that are being declared. *)

val sorts: StringSet.t

(* The subset of pattern types that are used in an abstraction. *)

val abstractions: StringSet.t

(* The set of atom sorts that each expression type directly or
   indirectly refers to. *)

val getlive: string -> StringSet.t

(* The set of atom sorts that each pattern type is declared
   to bind. *)

val getbinds: string -> StringSet.t

(* The set of atom sorts that each pattern type directly or indirectly
   refers to in a binding position. A subset of [binds]. Coincides
   with [binds] for pattern types that are used in an abstraction. *)

val getboundlive: string -> StringSet.t

(* The set of atom sorts that each pattern type directly or indirectly
   refers to in inner scope. A subset of [binds]. Coincides with
   [binds] for pattern types that are used in an abstraction. *)

val getinnerlive: string -> StringSet.t

(* The set of atom sorts that each pattern type directly or
   indirectly refers to in outer scope. *)

val getouterlive: string -> StringSet.t

(* Knowledge about external containers. *)

module Container : sig

  val print: container -> string
  val suffix: container -> string
  val map: container -> string
  val fold: container -> string
  val fold2: container -> string

end

(* The identifier module that should be used. *)

val identifier_module: string

(* The set of all type parameters that appear in type definitions. *)

val allparams: StringSet.t

