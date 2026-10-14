open S_exp
open Ast

exception ParseError of s_exp

let prim1_of_string = function
  | "add1" ->
      Some Add1
  | "sub1" ->
      Some Sub1
  | "zero?" ->
      Some IsZero
  | "num?" ->
      Some IsNum
  | "not" ->
      Some Not
  | "pair?" ->
      Some IsPair
  | "left" ->
      Some Left
  | "right" ->
      Some Right
  | "list?" ->
      Some IsList
  | "vector?" ->
      Some IsVector
  | "vector-length" ->
      Some VectorLength
  | _ ->
      None

let prim2_of_string = function
  | "+" ->
      Some Plus
  | "-" ->
      Some Minus
  | "=" ->
      Some Eq
  | "<" ->
      Some Lt
  | "pair" ->
      Some Pair
  | "vector" ->
      Some Vector
  | "vector-get" ->
      Some VectorGet
  | _ ->
      None

let prim3_of_string = function "vector-set" -> Some VectorSet | _ -> None

let rec expr_of_s_exp : s_exp -> expr = function
  | Num n ->
      Num n
  | Sym "true" ->
      True
  | Sym "false" ->
      False
  | Lst [] ->
      Nil
  | Sym var ->
      Var var
  | Lst [Sym "if"; test_s; then_s; else_s] ->
      If (expr_of_s_exp test_s, expr_of_s_exp then_s, expr_of_s_exp else_s)
  | Lst [Sym "let"; Lst bindings; body] ->
      Let
        ( List.map
            (fun (binding : s_exp) ->
              match binding with
              | Lst [Sym var; exp] ->
                  (var, expr_of_s_exp exp)
              | _ ->
                  raise (Invalid_argument "bad binding format") )
            bindings
        , expr_of_s_exp body )
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      Do (List.map expr_of_s_exp exps)
  | Lst [Sym prim; arg] when Option.is_some (prim1_of_string prim) ->
      Prim1 (Option.get (prim1_of_string prim), expr_of_s_exp arg)
  | Lst [Sym prim; arg1; arg2] when Option.is_some (prim2_of_string prim) ->
      Prim2
        ( Option.get (prim2_of_string prim)
        , expr_of_s_exp arg1
        , expr_of_s_exp arg2 )
  | Lst [Sym prim; arg1; arg2; arg3] when Option.is_some (prim3_of_string prim)
    ->
      Prim3
        ( Option.get (prim3_of_string prim)
        , expr_of_s_exp arg1
        , expr_of_s_exp arg2
        , expr_of_s_exp arg3 )
  | e ->
      raise (ParseError e)
