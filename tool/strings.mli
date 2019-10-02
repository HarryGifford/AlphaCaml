(* This module defines sets of strings, maps over strings,
   and auxiliary functions over these data structures. *)

module StringSet : Set.S with type elt = string

module StringMap : Map.S with type key = string

module StringAux : sig

  (* Apply a function to all elements of a set. *)

  val map: (string -> string) -> StringSet.t -> StringSet.t

  (* Turn a set of strings into a map over strings,
     and vice-versa. *)

  val set_to_map: StringSet.t -> 'a -> 'a StringMap.t
  val domain: 'a StringMap.t -> StringSet.t

  (* Print a set of strings as a comma-separated list
     of strings. *)

  val print: StringSet.t -> string

  (* Modify an entry in a (mutable) map over strings. *)

  val modify: 'a StringMap.t ref -> string -> ('a -> 'a) -> unit

  (* Merge a set of strings into an entry in a (mutable) map over
     strings. A hook is called if the entry changes. *)

  val merge: StringSet.t StringMap.t ref -> string -> StringSet.t -> (string -> unit) -> unit

  (* [new_list_manager channel empty opening separator closing]
     returns two functions [element] and [finished] that should be
     called prior to printing each list element and when done printing
     the list. The functions take care of printing the appropriate
     bits of syntax on the output channel. *)

  val new_list_manager: out_channel -> string -> string -> string -> string -> (unit -> unit) * (unit -> unit)

  (* [show_list empty opening separator closing] turns a list of
     strings into a string. If the list is empty, [empty] is returned;
     otherwise, [opening] and [closing] are used to delimit a
     [separator]-separated list of elements. *)

  val show_list: string -> string -> string -> string -> string list -> string

end
