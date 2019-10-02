(* This file was generated from lambda.mla. Do not edit! *)

(* Expose the module signatures defined in alphaLib. *)

open AlphaLib.Signatures

(* This module defines identifiers, that is,
   external representations for atoms. *)

module Identifier : Identifier with type t = AlphaLib.Atom.String.t

(* This exception is raised by the functions that open two
   abstractions at once when the two abstractions have different
   structure, so that their bound atoms cannot be forced to be
   identical. *)

exception Open2

(* This module defines atoms of sort var. *)

module Var : Atom with type identifier = Identifier.t

(* This module reflects the type definitions found in the source
   file, but in a raw form, that is, in a form where atoms are
   represented by identifiers and abstractions are transparent.
   Raw forms are usually produced by a parser and consumed by a
   pretty-printer. Functions that convert to and from raw forms
   are provided. *)

module Raw : sig

type var =
  Identifier.t

 and expression = 
  | EVar of var
  | ELambda of lambda
  | EApp of expression * expression
  | EPair of expression * expression
  | EInj of ( int ) * expression
  | ECase of expression * branch list
  | ELetRec of letrec

 and lambda = 
  var * expression

 and branch = 
  clause

 and clause = 
  pattern * expression

 and letrec = 
  binding list * expression

 and binding = 
  pattern * expression

 and pattern = 
  | PWildcard
  | PVar of var
  | PPair of pattern * pattern
  | PInj of ( int ) * pattern
  | PAnd of pattern * pattern
  | POr of pattern * pattern

end

(* This module reflects the type definitions found in the source file,
   in a flat internal form, that is, in a form where atoms are opaque,
   but abstractions are transparent. Functions that convert back and
   forth between internal and flat internal forms are provided. The
   conversion from internal form to flat internal form guarantees that
   all names are unique -- which is why flat internal form can be
   attractive. *)

module Flat : sig

type var =
  Var.Atom.t

 and expression = 
  | EVar of var
  | ELambda of lambda
  | EApp of expression * expression
  | EPair of expression * expression
  | EInj of ( int ) * expression
  | ECase of expression * branch list
  | ELetRec of letrec

 and lambda = 
  var * expression

 and branch = 
  clause

 and clause = 
  pattern * expression

 and letrec = 
  binding list * expression

 and binding = 
  pattern * expression

 and pattern = 
  | PWildcard
  | PVar of var
  | PPair of pattern * pattern
  | PInj of ( int ) * pattern
  | PAnd of pattern * pattern
  | POr of pattern * pattern

end

(* The following type definitions reflect those found in the source
   file, this time in an internal form, that is, in a form where both
   atoms and abstractions are opaque (abstract) data structures.
   Functions that convert between the opaque and transparent versions
   of each abstraction are provided. This approach provides safety --
   the contents of an abstraction cannot be inspected without
   appropriate freshening of its bound atoms -- and better efficiency
   -- sets and maps over atoms are usually less costly than sets and
   maps over identifiers. *)

type var =
  Var.Atom.t

 and expression = 
  | EVar of var
  | ELambda of opaque_lambda
  | EApp of expression * expression
  | EPair of expression * expression
  | EInj of ( int ) * expression
  | ECase of expression * branch list
  | ELetRec of opaque_letrec

 and lambda = 
  var * expression

 and opaque_lambda

 and branch = 
  opaque_clause

 and clause = 
  pattern * expression

 and opaque_clause

 and letrec = 
  binding list * expression

 and opaque_letrec

 and binding = 
  pattern * expression

 and pattern = 
  | PWildcard
  | PVar of var
  | PPair of pattern * pattern
  | PInj of ( int ) * pattern
  | PAnd of pattern * pattern
  | POr of pattern * pattern

(* The following functions operate over the expression type "expression". *)

(*
 * Function:   import_expression
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *             or a [Var.UnboundIdentifier] exception
 *)

val import_expression : var Identifier.Map.t -> Raw.expression -> expression

(*
 * Function:   subst_expression
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_expression : Var.Subst.t -> expression -> expression

(*
 * Function:   export_expression
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_expression : Var.AtomIdMap.t -> expression -> Raw.expression

(*
 * Function:   flatten_expression
 * Summary:    converts internal forms to flat internal forms
 * Parameters: an expression in internal form
 * Results:    an expression in flat internal form
 *               (obtained by opening up every abstraction in the input expression)
 *)

val flatten_expression : expression -> Flat.expression

(*
 * Function:   unflatten_expression
 * Summary:    converts flat internal forms to internal forms
 * Parameters: an expression in flat internal form
 * Results:    an expression in internal form
 *               (obtained by creating abstractions where needed)
 *)

val unflatten_expression : Flat.expression -> expression

(*
 * Function:   free_expression
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort var that occur free in the input expression
 *)

val free_expression : expression -> Var.AtomSet.t

(*
 * Function:   equal_expression
 * Summary:    tests whether two data structures are related modulo alpha-conversion
 * Parameters: two data structures
 * Results:    a Boolean outcome
 *)

val equal_expression : expression -> expression -> bool

(* The following functions operate over the pattern type "lambda". *)

(*
 * Function:   subst_lambda
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_lambda : Var.Subst.t -> lambda -> lambda

(*
 * Function:   bound_lambda
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_lambda : lambda -> Var.AtomSet.t

(*
 * Function:   bound_free_lambda
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *)

val bound_free_lambda : lambda -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   equal_lambda
 * Summary:    tests whether two data structures are related modulo alpha-conversion
 * Parameters: two data structures
 * Results:    a Boolean outcome
 *)

val equal_lambda : lambda -> lambda -> bool

(*
 * Function:   create_lambda
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_lambda : lambda -> opaque_lambda

(*
 * Function:   open_lambda
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_lambda : opaque_lambda -> lambda

(*
 * Function:   open2_lambda
 * Summary:    opens two opaque abstractions at once, ensuring that their structure and bound atoms match
 * Parameters: two abstract data structures
 *               (two opaque abstractions)
 * Results:    two patterns in internal form, with identical structure and bound atoms
 *               (transparent versions of the abstractions, where all bound atoms are freshly renamed)
 *             or an [Open2] exception
 *)

val open2_lambda : opaque_lambda -> opaque_lambda -> lambda * lambda

(* The following functions operate over the expression type "branch". *)

(*
 * Function:   import_branch
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *             or a [Var.UnboundIdentifier] exception
 *)

val import_branch : var Identifier.Map.t -> Raw.branch -> branch

(*
 * Function:   subst_branch
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_branch : Var.Subst.t -> branch -> branch

(*
 * Function:   export_branch
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_branch : Var.AtomIdMap.t -> branch -> Raw.branch

(*
 * Function:   flatten_branch
 * Summary:    converts internal forms to flat internal forms
 * Parameters: an expression in internal form
 * Results:    an expression in flat internal form
 *               (obtained by opening up every abstraction in the input expression)
 *)

val flatten_branch : branch -> Flat.branch

(*
 * Function:   unflatten_branch
 * Summary:    converts flat internal forms to internal forms
 * Parameters: an expression in flat internal form
 * Results:    an expression in internal form
 *               (obtained by creating abstractions where needed)
 *)

val unflatten_branch : Flat.branch -> branch

(*
 * Function:   free_branch
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort var that occur free in the input expression
 *)

val free_branch : branch -> Var.AtomSet.t

(*
 * Function:   equal_branch
 * Summary:    tests whether two data structures are related modulo alpha-conversion
 * Parameters: two data structures
 * Results:    a Boolean outcome
 *)

val equal_branch : branch -> branch -> bool

(* The following functions operate over the pattern type "clause". *)

(*
 * Function:   subst_clause
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_clause : Var.Subst.t -> clause -> clause

(*
 * Function:   bound_clause
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_clause : clause -> Var.AtomSet.t

(*
 * Function:   bound_free_clause
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *)

val bound_free_clause : clause -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   equal_clause
 * Summary:    tests whether two data structures are related modulo alpha-conversion
 * Parameters: two data structures
 * Results:    a Boolean outcome
 *)

val equal_clause : clause -> clause -> bool

(*
 * Function:   create_clause
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_clause : clause -> opaque_clause

(*
 * Function:   open_clause
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_clause : opaque_clause -> clause

(*
 * Function:   open2_clause
 * Summary:    opens two opaque abstractions at once, ensuring that their structure and bound atoms match
 * Parameters: two abstract data structures
 *               (two opaque abstractions)
 * Results:    two patterns in internal form, with identical structure and bound atoms
 *               (transparent versions of the abstractions, where all bound atoms are freshly renamed)
 *             or an [Open2] exception
 *)

val open2_clause : opaque_clause -> opaque_clause -> clause * clause

(* The following functions operate over the pattern type "letrec". *)

(*
 * Function:   subst_letrec
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_letrec : Var.Subst.t -> letrec -> letrec

(*
 * Function:   bound_letrec
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_letrec : letrec -> Var.AtomSet.t

(*
 * Function:   bound_free_letrec
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *)

val bound_free_letrec : letrec -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   equal_letrec
 * Summary:    tests whether two data structures are related modulo alpha-conversion
 * Parameters: two data structures
 * Results:    a Boolean outcome
 *)

val equal_letrec : letrec -> letrec -> bool

(*
 * Function:   create_letrec
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_letrec : letrec -> opaque_letrec

(*
 * Function:   open_letrec
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_letrec : opaque_letrec -> letrec

(*
 * Function:   open2_letrec
 * Summary:    opens two opaque abstractions at once, ensuring that their structure and bound atoms match
 * Parameters: two abstract data structures
 *               (two opaque abstractions)
 * Results:    two patterns in internal form, with identical structure and bound atoms
 *               (transparent versions of the abstractions, where all bound atoms are freshly renamed)
 *             or an [Open2] exception
 *)

val open2_letrec : opaque_letrec -> opaque_letrec -> letrec * letrec

(* The following functions operate over the pattern type "binding". *)

(*
 * Function:   subst_binding
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_binding : Var.Subst.t -> binding -> binding

(*
 * Function:   bound_binding
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_binding : binding -> Var.AtomSet.t

(*
 * Function:   bound_free_binding
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *)

val bound_free_binding : binding -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   equal_binding
 * Summary:    tests whether two data structures are related modulo alpha-conversion
 * Parameters: two data structures
 * Results:    a Boolean outcome
 *)

val equal_binding : binding -> binding -> bool

(* The following functions operate over the pattern type "pattern". *)

(*
 * Function:   subst_pattern
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_pattern : Var.Subst.t -> pattern -> pattern

(*
 * Function:   bound_pattern
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_pattern : pattern -> Var.AtomSet.t

(*
 * Function:   bound_free_pattern
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_free_pattern : pattern -> Var.AtomSet.t

(*
 * Function:   equal_pattern
 * Summary:    tests whether two data structures are related modulo alpha-conversion
 * Parameters: two data structures
 * Results:    a Boolean outcome
 *)

val equal_pattern : pattern -> pattern -> bool

(* The following class contains code that ``transforms'' a data structure.
   The methods provided in this class implement an identity transformation,
   that is, they traverse the data structure and produce a semantically
   equivalent copy of it. The intended use of this class is for the client
   to create a subclass and override one or several methods so as to obtain
   nontrivial behavior. *)

class map : object

  (*
   * Method:     expression
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method expression : expression -> expression

  (*
   * Method:     evar
   * Summary:    transforms under the data constructor "EVar"
   * Parameters: a tuple of the parameters to "EVar"
   * Results:    the result of applying "EVar" to the transformed parameters
   *)

  method evar : var -> expression

  (*
   * Method:     elambda
   * Summary:    transforms under the data constructor "ELambda"
   * Parameters: a tuple of the parameters to "ELambda"
   * Results:    the result of applying "ELambda" to the transformed parameters
   *)

  method elambda : opaque_lambda -> expression

  (*
   * Method:     eapp
   * Summary:    transforms under the data constructor "EApp"
   * Parameters: a tuple of the parameters to "EApp"
   * Results:    the result of applying "EApp" to the transformed parameters
   *)

  method eapp : expression * expression -> expression

  (*
   * Method:     epair
   * Summary:    transforms under the data constructor "EPair"
   * Parameters: a tuple of the parameters to "EPair"
   * Results:    the result of applying "EPair" to the transformed parameters
   *)

  method epair : expression * expression -> expression

  (*
   * Method:     einj
   * Summary:    transforms under the data constructor "EInj"
   * Parameters: a tuple of the parameters to "EInj"
   * Results:    the result of applying "EInj" to the transformed parameters
   *)

  method einj : ( int ) * expression -> expression

  (*
   * Method:     ecase
   * Summary:    transforms under the data constructor "ECase"
   * Parameters: a tuple of the parameters to "ECase"
   * Results:    the result of applying "ECase" to the transformed parameters
   *)

  method ecase : expression * branch list -> expression

  (*
   * Method:     eletrec
   * Summary:    transforms under the data constructor "ELetRec"
   * Parameters: a tuple of the parameters to "ELetRec"
   * Results:    the result of applying "ELetRec" to the transformed parameters
   *)

  method eletrec : opaque_letrec -> expression

  (*
   * Method:     lambda
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method lambda : lambda -> lambda

  (*
   * Method:     branch
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method branch : branch -> branch

  (*
   * Method:     clause
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method clause : clause -> clause

  (*
   * Method:     letrec
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method letrec : letrec -> letrec

  (*
   * Method:     binding
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method binding : binding -> binding

  (*
   * Method:     pattern
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method pattern : pattern -> pattern

  (*
   * Method:     pwildcard
   * Summary:    transforms under the data constructor "PWildcard"
   * Parameters: a tuple of the parameters to "PWildcard"
   * Results:    the result of applying "PWildcard" to the transformed parameters
   *)

  method pwildcard : pattern

  (*
   * Method:     pvar
   * Summary:    transforms under the data constructor "PVar"
   * Parameters: a tuple of the parameters to "PVar"
   * Results:    the result of applying "PVar" to the transformed parameters
   *)

  method pvar : var -> pattern

  (*
   * Method:     ppair
   * Summary:    transforms under the data constructor "PPair"
   * Parameters: a tuple of the parameters to "PPair"
   * Results:    the result of applying "PPair" to the transformed parameters
   *)

  method ppair : pattern * pattern -> pattern

  (*
   * Method:     pinj
   * Summary:    transforms under the data constructor "PInj"
   * Parameters: a tuple of the parameters to "PInj"
   * Results:    the result of applying "PInj" to the transformed parameters
   *)

  method pinj : ( int ) * pattern -> pattern

  (*
   * Method:     pand
   * Summary:    transforms under the data constructor "PAnd"
   * Parameters: a tuple of the parameters to "PAnd"
   * Results:    the result of applying "PAnd" to the transformed parameters
   *)

  method pand : pattern * pattern -> pattern

  (*
   * Method:     por
   * Summary:    transforms under the data constructor "POr"
   * Parameters: a tuple of the parameters to "POr"
   * Results:    the result of applying "POr" to the transformed parameters
   *)

  method por : pattern * pattern -> pattern

end

(* The following class contains code that iterates over a data structure
   while updating an accumulator. The methods provided in this class
   implement an identity over the accumulator, that is, they traverse the
   data structure and always return the initial accumulator. The intended
   use of this class is for the client to create a subclass and override
   one or several methods so as to obtain nontrivial behavior. *)

class [ 'accumulator ] fold : object

  (*
   * Method:     expression
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method expression : 'accumulator -> expression -> 'accumulator

  (*
   * Method:     evar
   * Summary:    iterates under the data constructor "EVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method evar : 'accumulator -> var -> 'accumulator

  (*
   * Method:     elambda
   * Summary:    iterates under the data constructor "ELambda"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ELambda"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method elambda : 'accumulator -> opaque_lambda -> 'accumulator

  (*
   * Method:     eapp
   * Summary:    iterates under the data constructor "EApp"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EApp"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method eapp : 'accumulator -> expression * expression -> 'accumulator

  (*
   * Method:     epair
   * Summary:    iterates under the data constructor "EPair"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EPair"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method epair : 'accumulator -> expression * expression -> 'accumulator

  (*
   * Method:     einj
   * Summary:    iterates under the data constructor "EInj"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EInj"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method einj : 'accumulator -> ( int ) * expression -> 'accumulator

  (*
   * Method:     ecase
   * Summary:    iterates under the data constructor "ECase"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ECase"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ecase : 'accumulator -> expression * branch list -> 'accumulator

  (*
   * Method:     eletrec
   * Summary:    iterates under the data constructor "ELetRec"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ELetRec"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method eletrec : 'accumulator -> opaque_letrec -> 'accumulator

  (*
   * Method:     lambda
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method lambda : 'accumulator -> lambda -> 'accumulator

  (*
   * Method:     branch
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method branch : 'accumulator -> branch -> 'accumulator

  (*
   * Method:     clause
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method clause : 'accumulator -> clause -> 'accumulator

  (*
   * Method:     letrec
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method letrec : 'accumulator -> letrec -> 'accumulator

  (*
   * Method:     binding
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method binding : 'accumulator -> binding -> 'accumulator

  (*
   * Method:     pattern
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method pattern : 'accumulator -> pattern -> 'accumulator

  (*
   * Method:     pwildcard
   * Summary:    iterates under the data constructor "PWildcard"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PWildcard"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method pwildcard : 'accumulator -> 'accumulator

  (*
   * Method:     pvar
   * Summary:    iterates under the data constructor "PVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method pvar : 'accumulator -> var -> 'accumulator

  (*
   * Method:     ppair
   * Summary:    iterates under the data constructor "PPair"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PPair"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ppair : 'accumulator -> pattern * pattern -> 'accumulator

  (*
   * Method:     pinj
   * Summary:    iterates under the data constructor "PInj"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PInj"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method pinj : 'accumulator -> ( int ) * pattern -> 'accumulator

  (*
   * Method:     pand
   * Summary:    iterates under the data constructor "PAnd"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PAnd"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method pand : 'accumulator -> pattern * pattern -> 'accumulator

  (*
   * Method:     por
   * Summary:    iterates under the data constructor "POr"
   * Parameters: an accumulator
   *             a tuple of the parameters to "POr"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method por : 'accumulator -> pattern * pattern -> 'accumulator

end
