(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/fresh/library/patricia.mli,v 1.1 2005/04/18 16:05:22 fpottier Exp $ *)

(* This is an implementation of Patricia trees, following Chris Okasaki's paper at the 1998 ML Workshop in Baltimore.
   Both big-endian and little-endian trees are provided. Both sets and maps are implemented on top of Patricia
   trees. *)

module Little : GMap.S with type key = int

module Big : GMap.S with type key = int

