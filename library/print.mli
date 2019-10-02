(* This module offers printing facilities for data structures equipped
   with iterators. It is used in order to offer default printing
   functions for sets and maps over atoms. *)

type punctuation =
    Buffer.t -> unit

type 'a printer =
    Buffer.t -> 'a -> unit

type 'a iterator =
   unit -> 'a option

val comma: punctuation

val preciterator: punctuation -> 'a printer ->  'a iterator printer
val sepiterator: punctuation -> 'a printer ->  'a iterator printer

