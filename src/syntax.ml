(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp

type program =
    Exp of exp

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

let freevar_ty _ =
  assert false (* Exercise 4.3.1 *)

let string_of_ty _ =
  assert false (* Exercise 4.3.1 *)

let pp_ty ty =
  print_string (string_of_ty ty)
