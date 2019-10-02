(* This file was generated from lambda.mla. Do not edit! *)

module Identifier = AlphaLib.Atom.String

exception Open2

let change_invalid_to_bool f x1 x2 =
  try
    f () x1 x2;
    true
  with Invalid_argument _ ->
    false

let change_invalid_to_open2 f x1 x2 =
  try
    f x1 x2
  with Invalid_argument _ ->
    raise Open2

module Var = AlphaLib.Atom.Make(Identifier)

module Raw = struct

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

module Flat = struct

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

 and opaque_lambda = {
    mutable lambda_delayed: Var.Subst.t;
    mutable lambda: lambda
  }

 and branch = 
  opaque_clause

 and clause = 
  pattern * expression

 and opaque_clause = {
    mutable clause_delayed: Var.Subst.t;
    mutable clause: clause
  }

 and letrec = 
  binding list * expression

 and opaque_letrec = {
    mutable letrec_delayed: Var.Subst.t;
    mutable letrec: letrec
  }

 and binding = 
  pattern * expression

 and pattern = 
  | PWildcard
  | PVar of var
  | PPair of pattern * pattern
  | PInj of ( int ) * pattern
  | PAnd of pattern * pattern
  | POr of pattern * pattern

let option_map f = function
  | None ->
      None
  | Some x ->
      Some (f x)

let option_fold f accu = function
  | None ->
      accu
  | Some x ->
      f accu x

let option_fold2 f accu o1 o2 =
  match o1, o2 with
  | None, None ->
      accu
  | Some x1, Some x2 ->
      f accu x1 x2
  | None, Some _
  | Some _, None ->
      raise (Invalid_argument "option_fold2")

let rec idmap_override freshb bv env =
  Identifier.Map.fold (fun id () env ->
    Identifier.Map.add id (freshb id) env
  ) bv env

and import_expression : var Identifier.Map.t -> Raw.expression -> expression = fun (var_env) -> function
  | Raw.EVar (_var0) ->
      EVar (Var.find _var0 var_env)
  | Raw.ELambda (_lambda0) ->
      let (var_bvars) = bvi_lambda _lambda0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let _lambda0 = import_lambda (var_ienv) _lambda0 in
      ELambda (create_lambda _lambda0)
  | Raw.EApp (_expression1, _expression0) ->
      EApp ((import_expression (var_env)) _expression1, (import_expression (var_env)) _expression0)
  | Raw.EPair (_expression1, _expression0) ->
      EPair ((import_expression (var_env)) _expression1, (import_expression (var_env)) _expression0)
  | Raw.EInj (_x1, _expression0) ->
      EInj (_x1, (import_expression (var_env)) _expression0)
  | Raw.ECase (_expression1, _branchs0) ->
      ECase ((import_expression (var_env)) _expression1, List.map (import_branch (var_env)) _branchs0)
  | Raw.ELetRec (_letrec0) ->
      let (var_bvars) = bvi_letrec _letrec0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let _letrec0 = import_letrec (var_ienv) _letrec0 in
      ELetRec (create_letrec _letrec0)

and subst_expression : Var.Subst.t -> expression -> expression = fun (var_env) -> function
  | EVar (_var0) ->
      EVar (Var.Subst.lookup _var0 var_env)
  | ELambda (_lambda0) ->
      ELambda (apply_lambda (var_env) _lambda0)
  | EApp (_expression1, _expression0) ->
      EApp ((subst_expression (var_env)) _expression1, (subst_expression (var_env)) _expression0)
  | EPair (_expression1, _expression0) ->
      EPair ((subst_expression (var_env)) _expression1, (subst_expression (var_env)) _expression0)
  | EInj (_x1, _expression0) ->
      EInj (_x1, (subst_expression (var_env)) _expression0)
  | ECase (_expression1, _branchs0) ->
      ECase ((subst_expression (var_env)) _expression1, List.map (subst_branch (var_env)) _branchs0)
  | ELetRec (_letrec0) ->
      ELetRec (apply_letrec (var_env) _letrec0)

and export_expression : Var.AtomIdMap.t -> expression -> Raw.expression = fun (var_m) -> function
  | EVar (_var0) ->
      Raw.EVar (Var.AtomIdMap.lookup _var0 var_m)
  | ELambda (_lambda0) ->
      let lambda = open_lambda _lambda0 in
      let (var_bvars) = bound_lambda lambda in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELambda (export_lambda (var_im) lambda)
  | EApp (_expression1, _expression0) ->
      Raw.EApp ((export_expression (var_m)) _expression1, (export_expression (var_m)) _expression0)
  | EPair (_expression1, _expression0) ->
      Raw.EPair ((export_expression (var_m)) _expression1, (export_expression (var_m)) _expression0)
  | EInj (_x1, _expression0) ->
      Raw.EInj (_x1, (export_expression (var_m)) _expression0)
  | ECase (_expression1, _branchs0) ->
      Raw.ECase ((export_expression (var_m)) _expression1, List.map (export_branch (var_m)) _branchs0)
  | ELetRec (_letrec0) ->
      let letrec = open_letrec _letrec0 in
      let (var_bvars) = bound_letrec letrec in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELetRec (export_letrec (var_im) letrec)

and flatten_expression : expression -> Flat.expression = function
  | EVar (_var0) ->
      Flat.EVar (_var0)
  | ELambda (_lambda0) ->
      let lambda = open_lambda _lambda0 in
      Flat.ELambda (flatten_lambda lambda)
  | EApp (_expression1, _expression0) ->
      Flat.EApp (flatten_expression _expression1, flatten_expression _expression0)
  | EPair (_expression1, _expression0) ->
      Flat.EPair (flatten_expression _expression1, flatten_expression _expression0)
  | EInj (_x1, _expression0) ->
      Flat.EInj (_x1, flatten_expression _expression0)
  | ECase (_expression1, _branchs0) ->
      Flat.ECase (flatten_expression _expression1, List.map flatten_branch _branchs0)
  | ELetRec (_letrec0) ->
      let letrec = open_letrec _letrec0 in
      Flat.ELetRec (flatten_letrec letrec)

and unflatten_expression : Flat.expression -> expression = function
  | Flat.EVar (_var0) ->
      EVar (_var0)
  | Flat.ELambda (_lambda0) ->
      let lambda = unflatten_lambda _lambda0 in
      ELambda (create_lambda lambda)
  | Flat.EApp (_expression1, _expression0) ->
      EApp (unflatten_expression _expression1, unflatten_expression _expression0)
  | Flat.EPair (_expression1, _expression0) ->
      EPair (unflatten_expression _expression1, unflatten_expression _expression0)
  | Flat.EInj (_x1, _expression0) ->
      EInj (_x1, unflatten_expression _expression0)
  | Flat.ECase (_expression1, _branchs0) ->
      ECase (unflatten_expression _expression1, List.map unflatten_branch _branchs0)
  | Flat.ELetRec (_letrec0) ->
      let letrec = unflatten_letrec _letrec0 in
      ELetRec (create_letrec letrec)

and free_expression : expression -> Var.AtomSet.t = 
  function expression -> free_accu_expression (Var.AtomSet.empty) expression

and equal_expression : expression -> expression -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_expression x1 x2

and aeq_expression : unit -> expression -> expression -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | EVar (_var0), EVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_expression");
      ()
  | ELambda (_lambda0), ELambda (_lambda1) ->
      let _lambda0, _lambda1 = open2i_lambda _lambda0 _lambda1 in
      aeq_lambda () _lambda0 _lambda1;
      ()
  | EApp (_expression1, _expression0), EApp (_expression3, _expression2) ->
      aeq_expression () _expression1 _expression3;
      aeq_expression () _expression0 _expression2;
      ()
  | EPair (_expression1, _expression0), EPair (_expression3, _expression2) ->
      aeq_expression () _expression1 _expression3;
      aeq_expression () _expression0 _expression2;
      ()
  | EInj (_x1, _expression0), EInj (_x3, _expression2) ->
      aeq_expression () _expression0 _expression2;
      ()
  | ECase (_expression1, _branchs0), ECase (_expression3, _branchs2) ->
      aeq_expression () _expression1 _expression3;
      List.fold_left2 aeq_branch () _branchs0 _branchs2;
      ()
  | ELetRec (_letrec0), ELetRec (_letrec1) ->
      let _letrec0, _letrec1 = open2i_letrec _letrec0 _letrec1 in
      aeq_letrec () _letrec0 _letrec1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_expression")

and free_accu_expression = fun (var_fvars) -> function
  | EVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)
  | ELambda (_lambda0) ->
      let lambda = open_lambda _lambda0 in
      let (var_bvars, var_ifvars) = bound_free_accu_lambda (Var.AtomSet.empty, Var.AtomSet.empty) lambda in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | EApp (_expression1, _expression0) ->
      let (var_fvars) = free_accu_expression (var_fvars) _expression1 in 
      let (var_fvars) = free_accu_expression (var_fvars) _expression0 in 
      (var_fvars)
  | EPair (_expression1, _expression0) ->
      let (var_fvars) = free_accu_expression (var_fvars) _expression1 in 
      let (var_fvars) = free_accu_expression (var_fvars) _expression0 in 
      (var_fvars)
  | EInj (_x1, _expression0) ->
      let (var_fvars) = free_accu_expression (var_fvars) _expression0 in 
      (var_fvars)
  | ECase (_expression1, _branchs0) ->
      let (var_fvars) = free_accu_expression (var_fvars) _expression1 in 
      let (var_fvars) = List.fold_left free_accu_branch (var_fvars) _branchs0 in 
      (var_fvars)
  | ELetRec (_letrec0) ->
      let letrec = open_letrec _letrec0 in
      let (var_bvars, var_ifvars) = bound_free_accu_letrec (Var.AtomSet.empty, Var.AtomSet.empty) letrec in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and subst_lambda : Var.Subst.t -> lambda -> lambda = fun (var_ienv) -> function
  (_var1, _expression0) ->
    (Var.Subst.lookup _var1 var_ienv, (subst_expression (var_ienv)) _expression0)

and bound_lambda : lambda -> Var.AtomSet.t = 
  function lambda -> bound_accu_lambda (Var.AtomSet.empty) lambda

and bound_free_lambda : lambda -> Var.AtomSet.t * Var.AtomSet.t = 
  function lambda -> bound_free_accu_lambda (Var.AtomSet.empty, Var.AtomSet.empty) lambda

and equal_lambda : lambda -> lambda -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lambda x1 x2

and import_lambda = fun (var_ienv) -> function
  (_var1, _expression0) ->
    (Var.find _var1 var_ienv, (import_expression (var_ienv)) _expression0)

and bvi_accu_lambda = fun (var_bvars) -> function
  (_var1, _expression0) ->
      let var_bvars = Identifier.Map.add _var1 () var_bvars in
      (var_bvars)

and bvi_lambda = 
  function lambda -> bvi_accu_lambda (Identifier.Map.empty) lambda

and bound_accu_lambda = fun (var_bvars) -> function
  (_var1, _expression0) ->
      let var_bvars = Var.AtomSet.add _var1 var_bvars in
      (var_bvars)

and export_lambda : Var.AtomIdMap.t -> lambda -> Raw.lambda = fun (var_im) -> function
  (_var1, _expression0) ->
    (Var.AtomIdMap.lookup _var1 var_im, (export_expression (var_im)) _expression0)

and flatten_lambda : lambda -> Flat.lambda = function
  (_var1, _expression0) ->
    (_var1, flatten_expression _expression0)

and unflatten_lambda : Flat.lambda -> lambda = function
  (_var1, _expression0) ->
    (_var1, unflatten_expression _expression0)

and bound_free_accu_lambda = fun (var_bvars, var_ifvars) -> function
  (_var1, _expression0) ->
      let var_bvars = Var.AtomSet.add _var1 var_bvars in
      let (var_ifvars) = free_accu_expression (var_ifvars) _expression0 in
      (var_bvars, var_ifvars)

and aeq_lambda : unit -> lambda -> lambda -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var1, _expression0), (_var3, _expression2) ->
      if not (Var.Atom.equal _var1 _var3) then raise (Invalid_argument "aeq_lambda");
      aeq_expression () _expression0 _expression2;
      ()

and freshen2_lambda : Var.Subst.t * Var.Subst.t -> lambda -> lambda -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var1, _expression0), (_var3, _expression2) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var1 var_env1 _var3 var_env2 in
      (var_env1, var_env2)

and create_lambda : lambda -> opaque_lambda = 
  function body -> {
    lambda_delayed = (Var.Subst.id);
    lambda = body
  }

and open_lambda : opaque_lambda -> lambda = function abstraction ->
  let (var_delayed) = abstraction.lambda_delayed in
  let body = abstraction.lambda in
  let (var_bvars) = bound_lambda body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_lambda (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.lambda_delayed <- (Var.Subst.id);
    abstraction.lambda <- body
  end;
  body

and open2_lambda : opaque_lambda -> opaque_lambda -> lambda * lambda = fun x1 x2 -> 
  change_invalid_to_open2 open2i_lambda x1 x2

and open2i_lambda : opaque_lambda -> opaque_lambda -> lambda * lambda = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.lambda_delayed in
  let body1 = abstraction1.lambda in
  let (var_delayed2) = abstraction2.lambda_delayed in
  let body2 = abstraction2.lambda in
  let (var_env1, var_env2) = freshen2_lambda (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_lambda (var_env1) body1 in
  let body2 = subst_lambda (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.lambda_delayed <- (Var.Subst.id);
    abstraction1.lambda <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.lambda_delayed <- (Var.Subst.id);
    abstraction2.lambda <- body2
  end;
  body1, body2

and apply_lambda = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.lambda_delayed in {
      abstraction with lambda_delayed = (Var.Subst.compose var_env var_delayed)
    }

and import_branch : var Identifier.Map.t -> Raw.branch -> branch = fun (var_env) -> function
  (_clause0) ->
      let (var_bvars) = bvi_clause _clause0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let _clause0 = import_clause (var_ienv) _clause0 in
    (create_clause _clause0)

and subst_branch : Var.Subst.t -> branch -> branch = fun (var_env) -> function
  (_clause0) ->
    (apply_clause (var_env) _clause0)

and export_branch : Var.AtomIdMap.t -> branch -> Raw.branch = fun (var_m) -> function
  (_clause0) ->
      let clause = open_clause _clause0 in
      let (var_bvars) = bound_clause clause in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_clause (var_im) clause)

and flatten_branch : branch -> Flat.branch = function
  (_clause0) ->
      let clause = open_clause _clause0 in
    (flatten_clause clause)

and unflatten_branch : Flat.branch -> branch = function
  (_clause0) ->
      let clause = unflatten_clause _clause0 in
    (create_clause clause)

and free_branch : branch -> Var.AtomSet.t = 
  function branch -> free_accu_branch (Var.AtomSet.empty) branch

and equal_branch : branch -> branch -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_branch x1 x2

and aeq_branch : unit -> branch -> branch -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_clause0), (_clause1) ->
      let _clause0, _clause1 = open2i_clause _clause0 _clause1 in
      aeq_clause () _clause0 _clause1;
      ()

and free_accu_branch = fun (var_fvars) -> function
  (_clause0) ->
      let clause = open_clause _clause0 in
      let (var_bvars, var_ifvars) = bound_free_accu_clause (Var.AtomSet.empty, Var.AtomSet.empty) clause in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and subst_clause : Var.Subst.t -> clause -> clause = fun (var_ienv) -> function
  (_pattern1, _expression0) ->
    ((subst_pattern (var_ienv)) _pattern1, (subst_expression (var_ienv)) _expression0)

and bound_clause : clause -> Var.AtomSet.t = 
  function clause -> bound_accu_clause (Var.AtomSet.empty) clause

and bound_free_clause : clause -> Var.AtomSet.t * Var.AtomSet.t = 
  function clause -> bound_free_accu_clause (Var.AtomSet.empty, Var.AtomSet.empty) clause

and equal_clause : clause -> clause -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_clause x1 x2

and import_clause = fun (var_ienv) -> function
  (_pattern1, _expression0) ->
    ((import_pattern (var_ienv)) _pattern1, (import_expression (var_ienv)) _expression0)

and bvi_accu_clause = fun (var_bvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and bvi_clause = 
  function clause -> bvi_accu_clause (Identifier.Map.empty) clause

and bound_accu_clause = fun (var_bvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and export_clause : Var.AtomIdMap.t -> clause -> Raw.clause = fun (var_im) -> function
  (_pattern1, _expression0) ->
    ((export_pattern (var_im)) _pattern1, (export_expression (var_im)) _expression0)

and flatten_clause : clause -> Flat.clause = function
  (_pattern1, _expression0) ->
    (flatten_pattern _pattern1, flatten_expression _expression0)

and unflatten_clause : Flat.clause -> clause = function
  (_pattern1, _expression0) ->
    (unflatten_pattern _pattern1, unflatten_expression _expression0)

and bound_free_accu_clause = fun (var_bvars, var_ifvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern1 in
      let (var_ifvars) = free_accu_expression (var_ifvars) _expression0 in
      (var_bvars, var_ifvars)

and aeq_clause : unit -> clause -> clause -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _expression0), (_pattern3, _expression2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_expression () _expression0 _expression2;
      ()

and freshen2_clause : Var.Subst.t * Var.Subst.t -> clause -> clause -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _expression0), (_pattern3, _expression2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern1 _pattern3 in
      (var_env1, var_env2)

and create_clause : clause -> opaque_clause = 
  function body -> {
    clause_delayed = (Var.Subst.id);
    clause = body
  }

and open_clause : opaque_clause -> clause = function abstraction ->
  let (var_delayed) = abstraction.clause_delayed in
  let body = abstraction.clause in
  let (var_bvars) = bound_clause body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_clause (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.clause_delayed <- (Var.Subst.id);
    abstraction.clause <- body
  end;
  body

and open2_clause : opaque_clause -> opaque_clause -> clause * clause = fun x1 x2 -> 
  change_invalid_to_open2 open2i_clause x1 x2

and open2i_clause : opaque_clause -> opaque_clause -> clause * clause = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.clause_delayed in
  let body1 = abstraction1.clause in
  let (var_delayed2) = abstraction2.clause_delayed in
  let body2 = abstraction2.clause in
  let (var_env1, var_env2) = freshen2_clause (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_clause (var_env1) body1 in
  let body2 = subst_clause (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.clause_delayed <- (Var.Subst.id);
    abstraction1.clause <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.clause_delayed <- (Var.Subst.id);
    abstraction2.clause <- body2
  end;
  body1, body2

and apply_clause = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.clause_delayed in {
      abstraction with clause_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_letrec : Var.Subst.t -> letrec -> letrec = fun (var_ienv) -> function
  (_bindings1, _expression0) ->
    (List.map (subst_binding (var_ienv)) _bindings1, (subst_expression (var_ienv)) _expression0)

and bound_letrec : letrec -> Var.AtomSet.t = 
  function letrec -> bound_accu_letrec (Var.AtomSet.empty) letrec

and bound_free_letrec : letrec -> Var.AtomSet.t * Var.AtomSet.t = 
  function letrec -> bound_free_accu_letrec (Var.AtomSet.empty, Var.AtomSet.empty) letrec

and equal_letrec : letrec -> letrec -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_letrec x1 x2

and import_letrec = fun (var_ienv) -> function
  (_bindings1, _expression0) ->
    (List.map (import_binding (var_ienv)) _bindings1, (import_expression (var_ienv)) _expression0)

and bvi_accu_letrec = fun (var_bvars) -> function
  (_bindings1, _expression0) ->
      let (var_bvars) = List.fold_left bvi_accu_binding (var_bvars) _bindings1 in
      (var_bvars)

and bvi_letrec = 
  function letrec -> bvi_accu_letrec (Identifier.Map.empty) letrec

and bound_accu_letrec = fun (var_bvars) -> function
  (_bindings1, _expression0) ->
      let (var_bvars) = List.fold_left bound_accu_binding (var_bvars) _bindings1 in
      (var_bvars)

and export_letrec : Var.AtomIdMap.t -> letrec -> Raw.letrec = fun (var_im) -> function
  (_bindings1, _expression0) ->
    (List.map (export_binding (var_im)) _bindings1, (export_expression (var_im)) _expression0)

and flatten_letrec : letrec -> Flat.letrec = function
  (_bindings1, _expression0) ->
    (List.map flatten_binding _bindings1, flatten_expression _expression0)

and unflatten_letrec : Flat.letrec -> letrec = function
  (_bindings1, _expression0) ->
    (List.map unflatten_binding _bindings1, unflatten_expression _expression0)

and bound_free_accu_letrec = fun (var_bvars, var_ifvars) -> function
  (_bindings1, _expression0) ->
      let (var_bvars, var_ifvars) = List.fold_left bound_free_accu_binding (var_bvars, var_ifvars) _bindings1 in
      let (var_ifvars) = free_accu_expression (var_ifvars) _expression0 in
      (var_bvars, var_ifvars)

and aeq_letrec : unit -> letrec -> letrec -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_bindings1, _expression0), (_bindings3, _expression2) ->
      List.fold_left2 aeq_binding () _bindings1 _bindings3;
      aeq_expression () _expression0 _expression2;
      ()

and freshen2_letrec : Var.Subst.t * Var.Subst.t -> letrec -> letrec -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_bindings1, _expression0), (_bindings3, _expression2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_binding (var_env1, var_env2) _bindings1 _bindings3 in
      (var_env1, var_env2)

and create_letrec : letrec -> opaque_letrec = 
  function body -> {
    letrec_delayed = (Var.Subst.id);
    letrec = body
  }

and open_letrec : opaque_letrec -> letrec = function abstraction ->
  let (var_delayed) = abstraction.letrec_delayed in
  let body = abstraction.letrec in
  let (var_bvars) = bound_letrec body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_letrec (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.letrec_delayed <- (Var.Subst.id);
    abstraction.letrec <- body
  end;
  body

and open2_letrec : opaque_letrec -> opaque_letrec -> letrec * letrec = fun x1 x2 -> 
  change_invalid_to_open2 open2i_letrec x1 x2

and open2i_letrec : opaque_letrec -> opaque_letrec -> letrec * letrec = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.letrec_delayed in
  let body1 = abstraction1.letrec in
  let (var_delayed2) = abstraction2.letrec_delayed in
  let body2 = abstraction2.letrec in
  let (var_env1, var_env2) = freshen2_letrec (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_letrec (var_env1) body1 in
  let body2 = subst_letrec (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.letrec_delayed <- (Var.Subst.id);
    abstraction1.letrec <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.letrec_delayed <- (Var.Subst.id);
    abstraction2.letrec <- body2
  end;
  body1, body2

and apply_letrec = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.letrec_delayed in {
      abstraction with letrec_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_binding : Var.Subst.t -> binding -> binding = fun (var_ienv) -> function
  (_pattern1, _expression0) ->
    ((subst_pattern (var_ienv)) _pattern1, (subst_expression (var_ienv)) _expression0)

and bound_binding : binding -> Var.AtomSet.t = 
  function binding -> bound_accu_binding (Var.AtomSet.empty) binding

and bound_free_binding : binding -> Var.AtomSet.t * Var.AtomSet.t = 
  function binding -> bound_free_accu_binding (Var.AtomSet.empty, Var.AtomSet.empty) binding

and equal_binding : binding -> binding -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_binding x1 x2

and import_binding = fun (var_ienv) -> function
  (_pattern1, _expression0) ->
    ((import_pattern (var_ienv)) _pattern1, (import_expression (var_ienv)) _expression0)

and bvi_accu_binding = fun (var_bvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and bvi_binding = 
  function binding -> bvi_accu_binding (Identifier.Map.empty) binding

and bound_accu_binding = fun (var_bvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and export_binding : Var.AtomIdMap.t -> binding -> Raw.binding = fun (var_im) -> function
  (_pattern1, _expression0) ->
    ((export_pattern (var_im)) _pattern1, (export_expression (var_im)) _expression0)

and flatten_binding : binding -> Flat.binding = function
  (_pattern1, _expression0) ->
    (flatten_pattern _pattern1, flatten_expression _expression0)

and unflatten_binding : Flat.binding -> binding = function
  (_pattern1, _expression0) ->
    (unflatten_pattern _pattern1, unflatten_expression _expression0)

and bound_free_accu_binding = fun (var_bvars, var_ifvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern1 in
      let (var_ifvars) = free_accu_expression (var_ifvars) _expression0 in
      (var_bvars, var_ifvars)

and aeq_binding : unit -> binding -> binding -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _expression0), (_pattern3, _expression2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_expression () _expression0 _expression2;
      ()

and freshen2_binding : Var.Subst.t * Var.Subst.t -> binding -> binding -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _expression0), (_pattern3, _expression2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern1 _pattern3 in
      (var_env1, var_env2)

and subst_pattern : Var.Subst.t -> pattern -> pattern = fun (var_ienv) -> function
  | PWildcard ->
      PWildcard
  | PVar (_var0) ->
      PVar (Var.Subst.lookup _var0 var_ienv)
  | PPair (_pattern1, _pattern0) ->
      PPair ((subst_pattern (var_ienv)) _pattern1, (subst_pattern (var_ienv)) _pattern0)
  | PInj (_x1, _pattern0) ->
      PInj (_x1, (subst_pattern (var_ienv)) _pattern0)
  | PAnd (_pattern1, _pattern0) ->
      PAnd ((subst_pattern (var_ienv)) _pattern1, (subst_pattern (var_ienv)) _pattern0)
  | POr (_pattern1, _pattern0) ->
      POr ((subst_pattern (var_ienv)) _pattern1, (subst_pattern (var_ienv)) _pattern0)

and bound_pattern : pattern -> Var.AtomSet.t = 
  function pattern -> bound_accu_pattern (Var.AtomSet.empty) pattern

and bound_free_pattern : pattern -> Var.AtomSet.t = 
  function pattern -> bound_free_accu_pattern (Var.AtomSet.empty) pattern

and equal_pattern : pattern -> pattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_pattern x1 x2

and import_pattern = fun (var_ienv) -> function
  | Raw.PWildcard ->
      PWildcard
  | Raw.PVar (_var0) ->
      PVar (Var.find _var0 var_ienv)
  | Raw.PPair (_pattern1, _pattern0) ->
      PPair ((import_pattern (var_ienv)) _pattern1, (import_pattern (var_ienv)) _pattern0)
  | Raw.PInj (_x1, _pattern0) ->
      PInj (_x1, (import_pattern (var_ienv)) _pattern0)
  | Raw.PAnd (_pattern1, _pattern0) ->
      PAnd ((import_pattern (var_ienv)) _pattern1, (import_pattern (var_ienv)) _pattern0)
  | Raw.POr (_pattern1, _pattern0) ->
      POr ((import_pattern (var_ienv)) _pattern1, (import_pattern (var_ienv)) _pattern0)

and bvi_accu_pattern = fun (var_bvars) -> function
  | Raw.PWildcard ->
      (var_bvars)
  | Raw.PVar (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)
  | Raw.PPair (_pattern1, _pattern0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | Raw.PInj (_x1, _pattern0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | Raw.PAnd (_pattern1, _pattern0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | Raw.POr (_pattern1, _pattern0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)

and bvi_pattern = 
  function pattern -> bvi_accu_pattern (Identifier.Map.empty) pattern

and bound_accu_pattern = fun (var_bvars) -> function
  | PWildcard ->
      (var_bvars)
  | PVar (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)
  | PPair (_pattern1, _pattern0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | PInj (_x1, _pattern0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | PAnd (_pattern1, _pattern0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | POr (_pattern1, _pattern0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)

and export_pattern : Var.AtomIdMap.t -> pattern -> Raw.pattern = fun (var_im) -> function
  | PWildcard ->
      Raw.PWildcard
  | PVar (_var0) ->
      Raw.PVar (Var.AtomIdMap.lookup _var0 var_im)
  | PPair (_pattern1, _pattern0) ->
      Raw.PPair ((export_pattern (var_im)) _pattern1, (export_pattern (var_im)) _pattern0)
  | PInj (_x1, _pattern0) ->
      Raw.PInj (_x1, (export_pattern (var_im)) _pattern0)
  | PAnd (_pattern1, _pattern0) ->
      Raw.PAnd ((export_pattern (var_im)) _pattern1, (export_pattern (var_im)) _pattern0)
  | POr (_pattern1, _pattern0) ->
      Raw.POr ((export_pattern (var_im)) _pattern1, (export_pattern (var_im)) _pattern0)

and flatten_pattern : pattern -> Flat.pattern = function
  | PWildcard ->
      Flat.PWildcard
  | PVar (_var0) ->
      Flat.PVar (_var0)
  | PPair (_pattern1, _pattern0) ->
      Flat.PPair (flatten_pattern _pattern1, flatten_pattern _pattern0)
  | PInj (_x1, _pattern0) ->
      Flat.PInj (_x1, flatten_pattern _pattern0)
  | PAnd (_pattern1, _pattern0) ->
      Flat.PAnd (flatten_pattern _pattern1, flatten_pattern _pattern0)
  | POr (_pattern1, _pattern0) ->
      Flat.POr (flatten_pattern _pattern1, flatten_pattern _pattern0)

and unflatten_pattern : Flat.pattern -> pattern = function
  | Flat.PWildcard ->
      PWildcard
  | Flat.PVar (_var0) ->
      PVar (_var0)
  | Flat.PPair (_pattern1, _pattern0) ->
      PPair (unflatten_pattern _pattern1, unflatten_pattern _pattern0)
  | Flat.PInj (_x1, _pattern0) ->
      PInj (_x1, unflatten_pattern _pattern0)
  | Flat.PAnd (_pattern1, _pattern0) ->
      PAnd (unflatten_pattern _pattern1, unflatten_pattern _pattern0)
  | Flat.POr (_pattern1, _pattern0) ->
      POr (unflatten_pattern _pattern1, unflatten_pattern _pattern0)

and bound_free_accu_pattern = fun (var_bvars) -> function
  | PWildcard ->
      (var_bvars)
  | PVar (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)
  | PPair (_pattern1, _pattern0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | PInj (_x1, _pattern0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | PAnd (_pattern1, _pattern0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)
  | POr (_pattern1, _pattern0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)

and aeq_pattern : unit -> pattern -> pattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PWildcard, PWildcard ->
      ()
  | PVar (_var0), PVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_pattern");
      ()
  | PPair (_pattern1, _pattern0), PPair (_pattern3, _pattern2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_pattern () _pattern0 _pattern2;
      ()
  | PInj (_x1, _pattern0), PInj (_x3, _pattern2) ->
      aeq_pattern () _pattern0 _pattern2;
      ()
  | PAnd (_pattern1, _pattern0), PAnd (_pattern3, _pattern2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_pattern () _pattern0 _pattern2;
      ()
  | POr (_pattern1, _pattern0), POr (_pattern3, _pattern2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_pattern () _pattern0 _pattern2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_pattern")

and freshen2_pattern : Var.Subst.t * Var.Subst.t -> pattern -> pattern -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PWildcard, PWildcard ->
      (var_env1, var_env2)
  | PVar (_var0), PVar (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)
  | PPair (_pattern1, _pattern0), PPair (_pattern3, _pattern2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern1 _pattern3 in
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern0 _pattern2 in
      (var_env1, var_env2)
  | PInj (_x1, _pattern0), PInj (_x3, _pattern2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern0 _pattern2 in
      (var_env1, var_env2)
  | PAnd (_pattern1, _pattern0), PAnd (_pattern3, _pattern2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern1 _pattern3 in
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern0 _pattern2 in
      (var_env1, var_env2)
  | POr (_pattern1, _pattern0), POr (_pattern3, _pattern2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern1 _pattern3 in
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern0 _pattern2 in
      (var_env1, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_pattern")

class map = object(self)

  method expression : expression -> expression = function
  | EVar (_var0) ->
      self#evar (_var0)
  | ELambda (_lambda0) ->
      self#elambda (_lambda0)
  | EApp (_expression1, _expression0) ->
      self#eapp (_expression1, _expression0)
  | EPair (_expression1, _expression0) ->
      self#epair (_expression1, _expression0)
  | EInj (_x1, _expression0) ->
      self#einj (_x1, _expression0)
  | ECase (_expression1, _branchs0) ->
      self#ecase (_expression1, _branchs0)
  | ELetRec (_letrec0) ->
      self#eletrec (_letrec0)

  method evar : var -> expression = 
  function (_var0) -> 
      EVar (_var0)

  method elambda : opaque_lambda -> expression = 
  function (_lambda0) -> 
      ELambda (create_lambda (self#lambda (open_lambda _lambda0)))

  method eapp : expression * expression -> expression = 
  function (_expression1, _expression0) -> 
      EApp ((self#expression) _expression1, (self#expression) _expression0)

  method epair : expression * expression -> expression = 
  function (_expression1, _expression0) -> 
      EPair ((self#expression) _expression1, (self#expression) _expression0)

  method einj : ( int ) * expression -> expression = 
  function (_x1, _expression0) -> 
      EInj (_x1, (self#expression) _expression0)

  method ecase : expression * branch list -> expression = 
  function (_expression1, _branchs0) -> 
      ECase ((self#expression) _expression1, List.map (self#branch) _branchs0)

  method eletrec : opaque_letrec -> expression = 
  function (_letrec0) -> 
      ELetRec (create_letrec (self#letrec (open_letrec _letrec0)))

  method lambda : lambda -> lambda = function
  (_var1, _expression0) ->
    (_var1, (self#expression) _expression0)

  method branch : branch -> branch = function
  (_clause0) ->
    (create_clause (self#clause (open_clause _clause0)))

  method clause : clause -> clause = function
  (_pattern1, _expression0) ->
    ((self#pattern) _pattern1, (self#expression) _expression0)

  method letrec : letrec -> letrec = function
  (_bindings1, _expression0) ->
    (List.map (self#binding) _bindings1, (self#expression) _expression0)

  method binding : binding -> binding = function
  (_pattern1, _expression0) ->
    ((self#pattern) _pattern1, (self#expression) _expression0)

  method pattern : pattern -> pattern = function
  | PWildcard ->
      self#pwildcard
  | PVar (_var0) ->
      self#pvar (_var0)
  | PPair (_pattern1, _pattern0) ->
      self#ppair (_pattern1, _pattern0)
  | PInj (_x1, _pattern0) ->
      self#pinj (_x1, _pattern0)
  | PAnd (_pattern1, _pattern0) ->
      self#pand (_pattern1, _pattern0)
  | POr (_pattern1, _pattern0) ->
      self#por (_pattern1, _pattern0)

  method pwildcard : pattern = PWildcard

  method pvar : var -> pattern = 
  function (_var0) -> 
      PVar (_var0)

  method ppair : pattern * pattern -> pattern = 
  function (_pattern1, _pattern0) -> 
      PPair ((self#pattern) _pattern1, (self#pattern) _pattern0)

  method pinj : ( int ) * pattern -> pattern = 
  function (_x1, _pattern0) -> 
      PInj (_x1, (self#pattern) _pattern0)

  method pand : pattern * pattern -> pattern = 
  function (_pattern1, _pattern0) -> 
      PAnd ((self#pattern) _pattern1, (self#pattern) _pattern0)

  method por : pattern * pattern -> pattern = 
  function (_pattern1, _pattern0) -> 
      POr ((self#pattern) _pattern1, (self#pattern) _pattern0)

end

class [ 'accumulator ] fold = object(self)

  method expression : 'accumulator -> expression -> 'accumulator = fun accu -> function
  | EVar (_var0) ->
      self#evar accu (_var0)
  | ELambda (_lambda0) ->
      self#elambda accu (_lambda0)
  | EApp (_expression1, _expression0) ->
      self#eapp accu (_expression1, _expression0)
  | EPair (_expression1, _expression0) ->
      self#epair accu (_expression1, _expression0)
  | EInj (_x1, _expression0) ->
      self#einj accu (_x1, _expression0)
  | ECase (_expression1, _branchs0) ->
      self#ecase accu (_expression1, _branchs0)
  | ELetRec (_letrec0) ->
      self#eletrec accu (_letrec0)

  method evar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method elambda : 'accumulator -> opaque_lambda -> 'accumulator = fun accu -> 
  function (_lambda0) -> 
      let accu = self#lambda accu (open_lambda _lambda0) in
      accu

  method eapp : 'accumulator -> expression * expression -> 'accumulator = fun accu -> 
  function (_expression1, _expression0) -> 
      let accu = (self#expression) accu _expression1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method epair : 'accumulator -> expression * expression -> 'accumulator = fun accu -> 
  function (_expression1, _expression0) -> 
      let accu = (self#expression) accu _expression1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method einj : 'accumulator -> ( int ) * expression -> 'accumulator = fun accu -> 
  function (_x1, _expression0) -> 
      let accu = (self#expression) accu _expression0 in
      accu

  method ecase : 'accumulator -> expression * branch list -> 'accumulator = fun accu -> 
  function (_expression1, _branchs0) -> 
      let accu = (self#expression) accu _expression1 in
      let accu = List.fold_left (self#branch) accu _branchs0 in
      accu

  method eletrec : 'accumulator -> opaque_letrec -> 'accumulator = fun accu -> 
  function (_letrec0) -> 
      let accu = self#letrec accu (open_letrec _letrec0) in
      accu

  method lambda : 'accumulator -> lambda -> 'accumulator = fun accu -> function
  (_var1, _expression0) ->
      let accu = (self#expression) accu _expression0 in
      accu

  method branch : 'accumulator -> branch -> 'accumulator = fun accu -> function
  (_clause0) ->
      let accu = self#clause accu (open_clause _clause0) in
      accu

  method clause : 'accumulator -> clause -> 'accumulator = fun accu -> function
  (_pattern1, _expression0) ->
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method letrec : 'accumulator -> letrec -> 'accumulator = fun accu -> function
  (_bindings1, _expression0) ->
      let accu = List.fold_left (self#binding) accu _bindings1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method binding : 'accumulator -> binding -> 'accumulator = fun accu -> function
  (_pattern1, _expression0) ->
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method pattern : 'accumulator -> pattern -> 'accumulator = fun accu -> function
  | PWildcard ->
      self#pwildcard accu
  | PVar (_var0) ->
      self#pvar accu (_var0)
  | PPair (_pattern1, _pattern0) ->
      self#ppair accu (_pattern1, _pattern0)
  | PInj (_x1, _pattern0) ->
      self#pinj accu (_x1, _pattern0)
  | PAnd (_pattern1, _pattern0) ->
      self#pand accu (_pattern1, _pattern0)
  | POr (_pattern1, _pattern0) ->
      self#por accu (_pattern1, _pattern0)

  method pwildcard : 'accumulator -> 'accumulator = fun accu ->       accu

  method pvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method ppair : 'accumulator -> pattern * pattern -> 'accumulator = fun accu -> 
  function (_pattern1, _pattern0) -> 
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#pattern) accu _pattern0 in
      accu

  method pinj : 'accumulator -> ( int ) * pattern -> 'accumulator = fun accu -> 
  function (_x1, _pattern0) -> 
      let accu = (self#pattern) accu _pattern0 in
      accu

  method pand : 'accumulator -> pattern * pattern -> 'accumulator = fun accu -> 
  function (_pattern1, _pattern0) -> 
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#pattern) accu _pattern0 in
      accu

  method por : 'accumulator -> pattern * pattern -> 'accumulator = fun accu -> 
  function (_pattern1, _pattern0) -> 
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#pattern) accu _pattern0 in
      accu

end
