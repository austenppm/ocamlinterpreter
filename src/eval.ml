open Syntax
(* Define the syntax for the evaluated values *)
type exval =
    IntV of int                  (* Integer values *)
  | BoolV of bool                (* Boolean values *)
  | ProcV of id * exp * dnval Environment.t ref  (* Function values, with an environment *)
  | DProcV of id * exp          (* Dynamically scoped function values *)
and dnval = exval               (* A recursive type to represent delayed (possibly recursive) values *)

exception Error of string       (* Exception type for runtime errors *)

let err s = raise (Error s)     (* Helper function to raise a runtime error *)

(* Functions for pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i   (* Convert integer to string *)
  | BoolV b -> string_of_bool b (* Convert boolean to string *)
  | ProcV (_,_,_) -> "<fun>"    (* Function values are printed as "<fun>" *)
  | DProcV(_,_) -> "<dfun>"     (* Dynamically scoped function values are printed as "<dfun>" *)

let pp_val v = print_string (string_of_exval v)  (* Print an evaluated value *)

let boundError () = err ("Same variable is bound several times")  (* Error for multiple bindings of the same variable *)
let get_id l = List.map (fun (id, _) -> id ) l   (* Get the list of identifiers from a list of pairs *)
let isBoundSeveralTimes l =                     (* Check if a variable is bound multiple times *)
  let idlist = get_id l in
  let rec bound idlist =
  match idlist with
  [] -> false
  | x :: [] -> false
  | x :: y :: rest -> 
    if x=y then true 
    else bound (x :: rest) || bound (y :: rest) in bound idlist

(* Apply a primitive operation *)
let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)  (* Addition *)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)  (* Multiplication *)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)   (* Less than *)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | Or, BoolV i1, BoolV i2 -> BoolV (i1 || i2)  (* Logical or *)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")
  | And, BoolV i1, BoolV i2 -> BoolV (i1 && i2) (* Logical and *)
  | And, _, _ -> err ("Both arguments must be boolean: &&")

(* Evaluate an expression *)
let rec eval_exp env = function
    Var x ->  (* Variable *)
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i  (* Integer literal *)
  | BLit b -> BoolV b  (* Boolean literal *)
  | BinOp (op, exp1, exp2) ->  (* Binary operation *)
    let arg1 = eval_exp env exp1 in
    if (op = And && arg1 = BoolV false) then BoolV false  (* Short-circuit evaluation for logical and *)
    else if (op = Or && arg1 = BoolV true) then BoolV true  (* Short-circuit evaluation for logical or *)
    else let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->  (* Conditional expression *)
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (ls,restexp) ->  (* Let expression *)
    if isBoundSeveralTimes ls then boundError();
    let id_vals = List.map (fun (id, e) -> (id, eval_exp env e)) ls in
     let newenv = List.fold_left (fun e (id, v) -> Environment.extend id v e) env id_vals in
     eval_exp newenv restexp
  | FunExp (id, exp) -> ProcV (id, exp, ref env)  (* Function expression *)
  | DFunExp (id,exp) -> DProcV (id,exp)  (* Dynamically scoped function expression *)
  | AppExp (exp1, exp2) ->  (* Function application *)
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
          ProcV (id, body, env') -> 
              let newenv = Environment.extend id arg env'.contents in
                eval_exp newenv body
        | DProcV (id, body) -> let newenv = Environment.extend id arg env in 
                eval_exp newenv body
        | _ -> err ("Non-function value is applied"))
  | LetRecExp (id, para, exp1, exp2) ->  (* Letrec expression *)
    let dummyenv = ref Environment.empty in 
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in
        dummyenv := newenv;
        eval_exp newenv exp2
  

(* Evaluate a declaration *)
let eval_decl env = function 
    Exp e -> let v = eval_exp env e in (["-", v], env)  (* Expression declaration *)
  | Decl e_ls ->  (* Multiple declarations *)
       if isBoundSeveralTimes e_ls then boundError();
       let v_ls = List.map (fun (id, e) -> (id, eval_exp env e)) e_ls in
       let newenv = List.fold_left (fun e (id, v) -> Environment.extend id v e) env v_ls in
         (v_ls, newenv)
  | RecDecl (id, para, e) ->  (* Recursive declaration *)
    let dummyenv = ref Environment.empty in
    let v = (ProcV (para, e, dummyenv)) in
    let newenv = Environment.extend id v env in 
    dummyenv := newenv;
    (["-", v], newenv)
  | QuitDecl -> exit 0 (*quit interactive session*)
