(* An evaluator and typechecker. *)

open Fsub

(* Type substitutions. *)

type type_subst =
    type_expr Type_var.AtomMap.t

val typeSubst: type_subst -> type_expr -> type_expr

(* Term substitutions. *)

type term_subst =
    term_expr Term_var.AtomMap.t

val termSubst: type_subst -> term_subst -> term_expr -> term_expr

(* Evaluation. *)

exception NoRuleApplies

val eval: term_expr -> term_expr

(* Contexts map type variables to their bounds. *)

type ctx =
    type_expr Type_var.AtomMap.t

val noctx: ctx

(* Environments map term variables to their types. *)

type env =
    type_expr Term_var.AtomMap.t

val noenv: env

(* Typing. *)

val typeof: env -> ctx -> term_expr -> type_expr

