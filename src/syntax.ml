(* ML interpreter / type reconstruction *)
open MySet
(* Define a custom exception type for errors *)
exception Error of string
let err s = raise (Error s) 

(* Define types for identifiers and binary operations *)
type id = string
type binOp = Plus | Mult | Lt | And | Or

(* Define the abstract syntax tree for expressions *)
type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of (id * exp) list * exp 
  | FunExp of id * exp   
  | AppExp of exp * exp 
  | DFunExp of id * exp 
  | LetRecExp of id * id * exp * exp 

(* Define the abstract syntax tree for programs *)
type program =
    Exp of exp
  | Decl of (id * exp) list 
  | RecDecl of id * id * exp 
  | QuitDecl  

(* let f x1 x2 ...= expr => let f = fun x1 x2 ...-> expr => let f = fun x1 -> fun x2 ...-> expr *)
let rec argstoFun args e = match args with
   [] -> e
  | arg :: rest -> FunExp (arg, argstoFun rest e)

(* Define types for type variables, types, and type schemas *)
type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty
type tysc = TyScheme of tyvar list * ty 

(* Create a type schema from a type *)
let tysc_of_ty ty = TyScheme ([], ty) 

(* Get the free type variables in a type *)
let rec freevar_ty = function   
    TyInt | TyBool -> MySet.empty
  | TyVar tv -> MySet.singleton tv
  | TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
  | _ -> err ("Not Implemented!")

(* Get the free type variables in a type schema *)
let rec freevar_tysc = function 
  TyScheme (vars, ty) -> 
    let allvars = freevar_ty ty in
     MySet.diff allvars (MySet.from_list vars)

(* Convert a type to a string *)
let rec string_of_ty  = function 
| TyInt -> "int"
| TyBool -> "bool"
| TyVar tv -> (* (0,1,2,...,25,26,...,51,...) => (a,b,c,...,z,a1,...,z1,...)*)
     let m = tv / 26 in
     let n = tv mod 26 in
     if m = 0 then  "'" ^ Char.escaped (char_of_int (n + 97))
     else "'" ^ Char.escaped (char_of_int (n+97)) ^ string_of_int m
| TyFun (ty1, ty2) ->
  (match ty1 with
     TyFun _ -> "(" ^ (string_of_ty ty1) ^ ") -> " ^ (string_of_ty ty2)
   | _ -> (string_of_ty ty1) ^ " -> " ^ (string_of_ty ty2))
| _ -> "Not Implemented!"

(* Print a type to the console *)
let rec pp_ty ty = print_string (string_of_ty ty) 

(* Convert a binary operation to a variable *)
let var_of_binop  = function 
  Plus -> Var "+"
| Mult -> Var "*"
| Lt -> Var "<"
| And -> Var "&&"
| Or -> Var "||"

(* Convert a variable to a binary operation *)
let id_of_binop = function 
  Var "+" -> "+"
| Var "*" -> "*"
| Var "<" -> "<"
| Var "&&" -> "&&"
| Var "||" -> "||"
| _ -> "Not Implemented!"

(* Generate a fresh type variable *)

let fresh_tyvar = 
  let counter = ref 0 in (* 次に返すべき tyvar 型の値を参照で持ってく *)
  let body () =
    let v = !counter in
      counter := v + 1; v (* 呼び出されたら参照をインクリメントして，古い counter の参照先の値を返す *)
  in body