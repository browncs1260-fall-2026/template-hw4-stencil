open S_exp

type prim1 =
  | Add1
  | Sub1
  | IsZero
  | IsNum
  | Not
  | IsPair
  | Left
  | Right
  | IsList
  | IsVector
  | VectorLength

type prim2 = Plus | Minus | Eq | Lt | Pair | Vector | VectorGet

type prim3 = VectorSet

type expr =
  | Num of int
  | Var of string
  | True
  | False
  | Nil
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Prim3 of prim3 * expr * expr * expr
  | If of expr * expr * expr
  | Let of (string * expr) list * expr
  | Do of expr list

(***** AST to S_exp *****)
let rec s_exp_of_expr = function
  | True ->
      Sym "true"
  | False ->
      Sym "false"
  | Nil ->
      Lst []
  | Num n ->
      Num n
  | Var var ->
      Sym var
  | Prim1 (Add1, arg) ->
      Lst [Sym "add1"; s_exp_of_expr arg]
  | Prim1 (Sub1, arg) ->
      Lst [Sym "sub1"; s_exp_of_expr arg]
  | Prim1 (IsZero, arg) ->
      Lst [Sym "zero?"; s_exp_of_expr arg]
  | Prim1 (IsNum, arg) ->
      Lst [Sym "num?"; s_exp_of_expr arg]
  | Prim1 (Not, arg) ->
      Lst [Sym "not"; s_exp_of_expr arg]
  | Prim1 (IsPair, arg) ->
      Lst [Sym "pair?"; s_exp_of_expr arg]
  | Prim1 (Left, arg) ->
      Lst [Sym "left"; s_exp_of_expr arg]
  | Prim1 (Right, arg) ->
      Lst [Sym "right"; s_exp_of_expr arg]
  | Prim1 (IsList, arg) ->
      Lst [Sym "list?"; s_exp_of_expr arg]
  | Prim1 (IsVector, arg) ->
      Lst [Sym "vector?"; s_exp_of_expr arg]
  | Prim1 (VectorLength, arg) ->
      Lst [Sym "vector-length"; s_exp_of_expr arg]
  | Prim2 (Plus, arg1, arg2) ->
      Lst [Sym "+"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Minus, arg1, arg2) ->
      Lst [Sym "-"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Lt, arg1, arg2) ->
      Lst [Sym "<"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Eq, arg1, arg2) ->
      Lst [Sym "="; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Pair, arg1, arg2) ->
      Lst [Sym "pair"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (Vector, arg1, arg2) ->
      Lst [Sym "vector"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim2 (VectorGet, arg1, arg2) ->
      Lst [Sym "vector-get"; s_exp_of_expr arg1; s_exp_of_expr arg2]
  | Prim3 (VectorSet, arg1, arg2, arg3) ->
      Lst
        [ Sym "vector-set"
        ; s_exp_of_expr arg1
        ; s_exp_of_expr arg2
        ; s_exp_of_expr arg3 ]
  | If (e1, e2, e3) ->
      Lst [Sym "if"; s_exp_of_expr e1; s_exp_of_expr e2; s_exp_of_expr e3]
  | Let (exps, eval_exp) ->
      let bindings =
        Lst
          (List.map
             (fun (e : string * expr) ->
               match e with v, expr -> Lst [Sym v; s_exp_of_expr expr] )
             exps )
      in
      Lst [Sym "let"; bindings; s_exp_of_expr eval_exp]
  | Do exps ->
      Lst (Sym "do" :: List.map s_exp_of_expr exps)

let string_of_expr s = s |> s_exp_of_expr |> string_of_s_exp
