(* This module defines sets and maps over strings. *)

module StringSet : Set.S with type elt = string

module StringMap : sig

  include Map.S with type key = string

  val singleton: string -> 'a -> 'a t

  val foldv: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val foldv2: ('b -> 'a -> 'a -> 'b) -> 'b -> 'a t -> 'a t -> 'b

  val domain: 'a t -> StringSet.t

  val union: 'a t -> 'a t -> 'a t

  val diff: 'a t -> StringSet.t -> 'a t

  val elements: 'a t -> (string * 'a) list

end

