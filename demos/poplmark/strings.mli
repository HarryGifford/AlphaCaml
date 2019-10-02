(* This module defines maps over strings. *)

module StringMap : sig

  include Map.S with type key = string

  val singleton: string -> 'a -> 'a t

  val foldv: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val foldv2: ('b -> 'a -> 'a -> 'b) -> 'b -> 'a t -> 'a t -> 'b

end

