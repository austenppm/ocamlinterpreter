open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t 
and dnval = exval

exception Error of string

let err s = raise (Error s)

let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Gt -> ">"


let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Div, IntV i1, IntV i2 -> IntV (i1 / i2)
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Gt, IntV i1, IntV i2 -> BoolV (i1 > i2)
  | (Plus | Minus | Mult | Div), _, _ -> err ("Both arguments must be integer: " ^ string_of_binop op)
  | _ -> err "Invalid arguments"


let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LogicOp (op, exp1, exp2) ->
        (match op with
        And ->
        let arg1 = eval_exp env exp1 in
        (match arg1 with
        BoolV (false) -> BoolV (false)
        | BoolV (true) -> let arg2 = eval_exp env exp2 in
        (match arg2 with
        BoolV (true) -> BoolV (true)
        | BoolV (false) -> BoolV (false)
        | _ -> err ("Both arguments must be boolean: &&"))
        | _ -> err ("Both arguments must be boolean: &&"))
        | Or ->
        let arg1 = eval_exp env exp1 in
        (match arg1 with
        BoolV (true) -> BoolV (true)
        | BoolV (false) -> let arg2 = eval_exp env exp2 in
        (match arg2 with
        BoolV (true) -> BoolV (true)
        | BoolV (false) -> BoolV (false)
        | _ -> err ("Both arguments must be boolean: ||"))
        | _ -> err ("Both arguments must be boolean: ||")))
   | LetExp (id, exp1, exp2) ->
     let value = eval_exp env exp1 in
     eval_exp (Environment.extend id value env) exp2
   | LetAndExp (decls, body) ->
  let ids = List.map fst decls in
  let has_duplicates =
    List.length ids <> List.length (List.sort_uniq compare ids)
  in
  if has_duplicates then
    err "Duplicate variable declaration in let ... and ..."
  else
    let values = List.map (fun (id, exp) -> (id, eval_exp env exp)) decls in
    let env' = List.fold_left (fun env (id, value) -> Environment.extend id value env) env values in
    eval_exp env' body
  | FunExp (id, exp) -> ProcV (id, exp, env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
          ProcV (id, body, env') -> 
              let newenv = Environment.extend id arg env' in
                eval_exp newenv body
        | _ -> 
          err ("Non-function value is applied"))
| NeutralOp (op, e) ->
    let v = eval_exp env e in
    (match op with
     | "(+)" -> ProcV ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y")), env)
     | "(-)" -> ProcV ("x", FunExp ("y", BinOp (Minus, Var "x", Var "y")), env)
     | "(*)" -> ProcV ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y")), env)
     | "(/)" -> ProcV ("x", FunExp ("y", BinOp (Div, Var "x", Var "y")), env)
     | _ -> failwith "unknown operator")

let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) ->
      let v = eval_exp env e in (id, Environment.extend id v env, v)
  | LetDecls decls ->
      let ids = List.map fst decls in
      let has_duplicates =
        List.length ids <> List.length (List.sort_uniq compare ids)
      in
      if has_duplicates then
        err "Duplicate variable declaration in let ... and ..."
      else
        let values = List.map (fun (id, exp) -> (id, eval_exp env exp)) decls in
        let env' = List.fold_left (fun env (id, value) -> Environment.extend id value env) env values in
        ("-", env', IntV 0)
  | LetAndDecls decls ->
      let ids = List.map fst decls in
      let has_duplicates =
        List.length ids <> List.length (List.sort_uniq compare ids)
      in
      if has_duplicates then
        err "Duplicate variable declaration in let ... and ..."
      else
        let values = List.map (fun (id, exp) -> (id, eval_exp env exp)) decls in
        let env' = List.fold_left (fun env (id, value) -> Environment.extend id value env) env values in
        ("-", env', IntV 0)
