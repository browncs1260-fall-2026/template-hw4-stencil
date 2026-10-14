open Printf
open Ast
open Util

(** a [value]  is the runtime value of an expression *)
type value = Num of int | Bool of bool | Pair of (value * value)

type environment = value Symtab.symtab

let top_env : environment =
  Symtab.empty
  |> Symtab.add "true" (Bool true)
  |> Symtab.add "false" (Bool false)

(** [display_value v] returns a string representation of the runtime value [v] *)
let rec display_value = function
  | Num x ->
      sprintf "%d" x
  | Bool b ->
      if b then "true" else "false"
  | Pair (v1, v2) ->
      sprintf "(pair %s %s)" (display_value v1) (display_value v2)

(** [interp_unary_primitive prim arg] tries to evaluate the primitive operation
   named by [prim] on the argument [arg]. If the operation is ill-typed, or if
   [prim] does not refer to a valid primitive operation, it returns None.*)
let interp_unary_primitive prim arg =
  match (prim, arg) with
  | Add1, Num x ->
      Some (Num (x + 1))
  | Sub1, Num x ->
      Some (Num (x - 1))
  | IsZero, Num 0 ->
      Some (Bool true)
  | IsZero, _ ->
      Some (Bool false)
  | IsNum, Num _ ->
      Some (Bool true)
  | IsNum, _ ->
      Some (Bool false)
  | Not, Bool false ->
      Some (Bool true)
  | Not, _ ->
      Some (Bool false)
  | IsPair, Pair _ ->
      Some (Bool true)
  | IsPair, _ ->
      Some (Bool false)
  | Left, Pair (v, _) ->
      Some v
  | Right, Pair (_, v) ->
      Some v
  | _ ->
      None

(** [interp_binary_primitive prim arg1 arg2] tries to evaluate the primitive
   operation named by [prim] on the arguments [arg] and [arg2]. If the operation
   is ill-typed, or if [prim] does not refer to a valid primitive operation, it
   returns None.*)
let interp_binary_primitive prim arg1 arg2 =
  match (prim, arg1, arg2) with
  | Plus, Num x1, Num x2 ->
      Some (Num (x1 + x2))
  | Minus, Num x1, Num x2 ->
      Some (Num (x1 - x2))
  | Eq, Num x1, Num x2 ->
      Some (Bool (x1 = x2))
  | Lt, Num x1, Num x2 ->
      Some (Bool (x1 < x2))
  | Pair, v1, v2 ->
      Some (Pair (v1, v2))
  | _ ->
      None

(** [interp_trinary_primitive prim arg1 arg2 arg3] tries to evaluate the primitive
   operation named by [prim] on the arguments [arg], [arg2], and [arg3]. If the
   operation is ill-typed, or if [prim] does not refer to a valid primitive
   operation, it returns None.*)
let interp_trinary_primitive prim arg1 arg2 arg3 =
  match (prim, arg1, arg2, arg3) with _ -> None

(** [interp_expr e] tries to evaluate the expression [e], producing a
   value. If [e] isn't a valid expression, it raises an error. *)
let rec interp_expr (env : environment) : expr -> value = function
  | Num x ->
      Num x
  | Var var as e -> (
    match Symtab.find_opt var env with
    | Some value ->
        value
    | None ->
        raise (Error.Stuck e) )
  | Let (bindings, body) ->
      let new_env =
        List.fold_left
          (fun new_env (var, exp) ->
            Symtab.add var (interp_expr env exp) new_env )
          env bindings
      in
      interp_expr new_env body
  | If (test_exp, then_exp, else_exp) ->
      if interp_expr env test_exp <> Bool false then interp_expr env then_exp
      else interp_expr env else_exp
  | Do exps ->
      exps |> List.rev_map (interp_expr env) |> List.hd
  | Prim1 (f, arg) as e -> (
    match interp_unary_primitive f (interp_expr env arg) with
    | Some v ->
        v
    | None ->
        raise (Error.Stuck e) )
  | Prim2 (f, arg1, arg2) as e -> (
    match
      interp_binary_primitive f (interp_expr env arg1) (interp_expr env arg2)
    with
    | Some v ->
        v
    | None ->
        raise (Error.Stuck e) )
  | Prim3 (f, arg1, arg2, arg3) as e -> (
    match
      interp_trinary_primitive f (interp_expr env arg1) (interp_expr env arg2)
        (interp_expr env arg3)
    with
    | Some v ->
        v
    | None ->
        raise (Error.Stuck e) )
  | True ->
      Bool true
  | False ->
      Bool false
  | e ->
      raise (Error.Stuck e)

(** [interp e] evaluates the s_expression list using [interp_expr], then formats
   the result as a string. *)
let interp = function
  | e :: _ ->
      Lisp_syntax.expr_of_s_exp e
      |> interp_expr top_env |> display_value |> printf "%s"
  | _ ->
      failwith "Empty source file"
