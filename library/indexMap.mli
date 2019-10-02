(* This functor turns an implementation of sets and maps over integers
   into an implementation of sets and maps over an indexed type. An
   indexed type is a type whose values can be mapped to integers in an
   injective manner (that is, no two elements have the same index). *)

module Make (I : sig

  type t

  val index: t -> int

end) (M : GMap.S with type key = int) : sig

  module Set : sig

    (* [t] is the type of sets. *)

    type t
    type element = I.t

    (* [empty] is the empty set. *)

    val empty: t

    (* [singleton a] is the singleton set [{a}]. *)

    val singleton: element -> t

    (* [add x s] is ([{x}]\tcup[s]). *)

    val add: element -> t -> t

    (* [remove x s] is ([s]\tminus[{x}]). *)

    val remove: element -> t -> t

    (* [union s1 s2] is ([s1]\tcup[s2]). *)

    val union: t -> t -> t

    (* [inter s1 s2] is ([s1]\tcap[s2]). *)

    val inter: t -> t -> t

    (* [diff s1 s2] is ([s1]\tminus[s2]). *)

    val diff: t -> t -> t

    (* [mem a s] is [true] if and only if [a] is a member of [s]. *)

    val mem: element -> t -> bool

    (* [is_empty s] is [true] if and only if [s] is the empty set. *)

    val is_empty: t -> bool

    (* [disjoint s1 s2] is [true] if and only if the sets [s1] and
       [s2] are disjoint, that is, if and only if their intersection
       is empty. *)

    val disjoint: t -> t -> bool

    (* [equal s1 s2] is [true] if and only if [s1] and [s2] are
       extensionally equal, that is, if and only if they have the same
       members. *)

    val equal: t -> t -> bool

    (* [subset s1 s2] is [true] if and only if [s1] is a subset of
       [s2], that is, if and only if every member of [s1] is also a
       member of [s2]. *)

    val subset: (t -> t -> bool)

    (* The call [iter f s] has the effect of applying the function
       [f] in turn to every member of [s]. *)

    val iter: (element -> unit) -> t -> unit

    (* The call [fold f s accu] has the effect of applying the
       function [f] in turn to every member of [s] and to an
       accumulator whose value, threaded through the calls, is
       initially [accu]. Its result is the final value of the
       accumulator. *)

    val fold: (element -> 'a -> 'a) -> t -> 'a -> 'a

    (* [iterator s] returns a stateful iterator over the set [s]. That
       is, if $s = \{ x_1, x_2, \ldots, x_n \}$, where $x_1 < x_2 <
       \ldots < x_n$, then [iterator s] is a function which, when
       invoked for the $k^{\text{th}}$ time, returns [Some ]$x_k$, if
       $k\leq n$, and [None] otherwise. *)

    val iterator: t -> (unit -> element option)

    (* [cardinal s] is the cardinal of the set [s]. *)

    val cardinal: t -> int

    (* [choose s] returns an arbitrarily chosen element of [s], if [s]
       is nonempty, and raises [Not_found] otherwise. *)

    val choose: t -> element

    (* [compare] is an ordering over sets. *)

    val compare: t -> t -> int

    (* [print] is a default printer for sets. It is parameterized with
       a printer for elements. It displays sets as comma-separated
       sequences of elements. *)

    val print: (Buffer.t -> element -> unit) -> (Buffer.t -> t -> unit)

  end

  module Map : sig

    (* [key] is the type of elements. *)

    type key =
	I.t

    (* ['a t] is the type of maps of elements to data of type ['a]. *)

    type 'a t

    (* [empty] is the empty map. *)

    val empty: 'a t

    (* [singleton a d] is the singleton map that maps element [a] to
       datum [d]. *)

    val singleton: key -> 'a -> 'a t

    (* [add a d m] is the map that maps element [a] to datum [d] and
       elsewhere behaves like [m]. *)

    val add: key -> 'a -> 'a t -> 'a t

    (* [strict_add a x m] raises [Strict a] if [a] is in the domain of
       [m] and otherwise returns [add a x m]. *)

    exception Strict of key

    val strict_add: key -> 'a -> 'a t -> 'a t

    (* [add_or_lookup a d m] adds a binding of [a] to [d] if no binding
       for [a] exists in [m]. It returns the existing datum associated
       with [a] if a binding for [a] already exists. *)

    type 'a add_or_lookup =
      | Added of 'a t
      | LookedUp of 'a

    val add_or_lookup: key -> 'a -> 'a t -> 'a add_or_lookup

    (* [remove a m] is the map that has no binding at [a] and
       elsewhere behaves like [m]. *)

    val remove: key -> 'a t -> 'a t

    (* [union m1 m2] is the map that behaves like [m2] where [m2]
       is defined and elsewhere behaves like [m1]. In other words,
       the bindings in [m2] take precedence over those in [m1]. *)

    val union: 'a t -> 'a t -> 'a t

    (* [lookup a m] returns the datum associated with element [a] in
       the map [m], if defined, and raises the exception [Not_found]
       otherwise. [lookup] is also known as [find]. *)

    val lookup: key -> 'a t -> 'a
    val find: key -> 'a t -> 'a

    (* [mem k m] tells whether the key [k] appears in the domain of the
       map [m]. *)

    val mem: key -> 'a t -> bool

    (* [is_empty m] is [true] if and only if [m] is the empty map. *)

    val is_empty: 'a t -> bool

    (* [map f m] is the map obtained by composing the function [f]
       with the map [m], that is, the map that maps an element [a] to
       [(f d)] when [m] maps [a] to [d]. *)

    val map: ('a -> 'b) -> 'a t -> 'b t

    (* [endo_map] is similar to [map], but attempts to physically share
       its result with its input. This saves memory when [f] is the
       identity function. *)

    val endo_map: ('a -> 'a) -> 'a t -> 'a t

    (* [mapi f m] is the map that maps an element [a] to [(f a d)] when
       [m] maps [a] to [d]. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t

    (* [iter f m] applies [f] in turn to each binding in the map [m]. *)

    val iter : (key -> 'a -> unit) -> 'a t -> unit

    (* [fold f m accu] applies [f] in turn to each binding in the map
       [m], threading an accumulator through the sequence of calls. *)

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (* [iterator m] returns a stateful iterator over the map [m]. That
       is, if $m = \{ x_1 \mapsto d_1, x_2 \mapsto d_2, \ldots, x_n
       \mapsto d_n \}$, where $x_1 < x_2 < \ldots < x_n$, then [iterator
       s] is a function which, when invoked for the $k^{\text{th}}$
       time, returns [Some ]$(x_k, d_k)$, if $k\leq n$, and [None]
       otherwise. *)

    val iterator: 'a t -> (unit -> (key * 'a) option)

    (* [cardinal m] returns [m]'s cardinal, that is, the number of keys
       it binds, or, in other words, the cardinal of its domain. *)

    val cardinal: 'a t -> int

    (* [choose m] returns an arbitrarily chosen binding in [m], if [m]
       is nonempty, and raises [Not_found] otherwise. *)

    val choose: 'a t -> key * 'a

    (* [domain m] is the domain of the map [m]. *)

    val domain: 'a t -> Set.t

    (* [print] is a default printer for maps. It is parameterized with
       a printer for keys and a printer for data. It displays maps as
       newline-terminated sequences of bindings. It displays bindings
       as a pair of a key and a datum, separated with an arrow. *)

    val print: (Buffer.t -> key -> unit) -> (Buffer.t -> 'a -> unit) -> (Buffer.t -> 'a t -> unit)

  end

end

