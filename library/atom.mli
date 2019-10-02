open Signatures

(* An implementation of identifiers as strings. [basename] chops off
   all trailing digits, while [combine] appends a representation of
   the integer salt grain to the basename. *)

module String : Identifier with type t = string

(* An implementation of atoms, parameterized by an implementation of
   identifiers. One independent reason for making it a functor is to
   allow keeping several separate sorts of atoms, each with its own
   type and internal state. *)

module Make (Identifier : Identifier) : Atom with type identifier = Identifier.t
                                              and type 'a identifier_map = 'a Identifier.Map.t

