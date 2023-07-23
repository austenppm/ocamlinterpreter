(* Import Syntax module *)
open Syntax

(* Define a custom exception for error handling *)
exception Error of string

(* Function to raise an error with a custom message *)
let err s = raise (Error s)

(* Define types for the type environment and the substitution *)
type tyenv = tysc Environment.t 
type subst = (tyvar * ty) list 

(* Function to find free variables in a type environment *)
let rec freevar_tyenv tyenv = 
  (* Folds the environment from the right, unioning the set of free variables of each type schema with the accumulated set *)
  Environment.fold_right (fun tysc set -> MySet.union set (freevar_tysc tysc)) tyenv MySet.empty

(* Function to convert a substitution to a list of type equations *)
let eqs_of_subst s = List.map (fun (tv,ty) -> (TyVar tv, ty)) s

(* Function to apply a substitution to a type *)
let rec subst_type subst typ =   
  (* If the substitution is empty, return the type. Otherwise, apply the substitution recursively *)
  match subst with
    [] -> typ
    | (tv,ty) :: rest ->
        match typ with 
          (* If the type is a type variable that matches the current substitution, recurse with the remaining substitutions and the substituted type *)
          TyVar tv' -> if tv' = tv then subst_type rest ty 
                       else subst_type rest typ
          (* If the type is a function type, apply the substitution to the domain and range types *)
          | TyFun (ty1, ty2) -> TyFun (subst_type subst ty1, subst_type subst ty2)
          (* If the type is a basic type (int or bool), return it as is *)
          | TyInt -> TyInt
          | TyBool -> TyBool
          (* If the type is not handled, raise an error *)
          | _ -> err ("Not Implemented!")

(* Function to compute the closure of a type in a type environment under a substitution *)
let closure ty tyenv subst = 
  (* Compute the free variables of the type environment after applying the substitution *)
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv = MySet.bigunion
      (MySet.map
          (fun id -> freevar_ty (subst_type subst (TyVar id)))
          fv_tyenv') in
  (* Compute the free variables of the type that are not in the type environment *)
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  (* Return a type schema for the type with the computed free variables *)
  TyScheme (MySet.to_list ids, ty)

(* Function to apply a substitution to a list of type equations *)
let rec subst_eqs s eqs = List.map (fun (ty1, ty2) -> (subst_type s ty1, subst_type s ty2)) eqs

(* Function to perform an occur-check: determine whether a type variable occurs in a type *)
let rec occur_check tv = function 
  (* If the type is the same type variable, return true *)
  TyVar tv' -> tv = tv'
  (* If the type is a function type, check if the type variable occurs in the domain or range type *)
  | TyFun (ty1, ty2) -> (occur_check tv ty1) || (occur_check tv ty2)
  (* If the type is a basic type (int or bool), return false *)
  | TyInt -> false
  | TyBool -> false
  (* If the type is not handled, raise an error *)
  | _ -> err ("Not Implemented!")

(* Function to unify a list of type equations *)
let rec unify = function 
  (* If there are no equations, return an empty substitution *)
  [] -> []
  (* Otherwise, handle the first equation and recurse on the rest *)
  | (ty1, ty2) :: rest -> 
    match ty1, ty2 with
      (* If the types are identical basic types, unify the rest of the equations *)
      TyInt, TyInt | TyBool, TyBool -> unify rest
      (* If the types are identical function types, add the equations for the domain and range types to the rest and unify them *)
      | TyFun (ty11, ty12), TyFun (ty21, ty22) -> unify ((ty11, ty21) :: (ty12, ty22) :: rest)
      (* If the types are identical type variables, unify the rest of the equations *)
      | TyVar tv1, TyVar tv2 ->
        if tv1 = tv2 then unify rest
        else 
          (* If the types are different type variables, add the substitution to the rest and unify them *)
          let s = [(tv1, ty2)] in s @ (unify (subst_eqs s rest))
      (* If one type is a type variable and the other is not *)
      | TyVar tv, ty | ty, TyVar tv ->
        (* If the type variable occurs in the type, raise a type error *)
        if occur_check tv ty then err ("Type Error: Type " ^ string_of_ty ty1 ^ " occured in　" ^ string_of_ty ty2 ^ "!")
        else 
          (* Otherwise, add the substitution to the rest and unify them *)
          let s = [(tv, ty)] in s @ (unify (subst_eqs s rest))
      (* If the types are not identical and cannot be unified, raise a type error *)
      | _, _ -> err ("Unification failed because of type error!")
(* Helper functions for list manipulation *)
let get_left (id,_) = id
let get_right (_,e) = e
let get_two l = List.map (fun (id,(_,ty)) -> (id,ty)) l
let get_s l = List.map ( fun (_,(s,_)) -> s) l
let rec append l1 l2 = match l1 with
  [] -> l2
  | x::rest -> x :: append rest l2;;

(* Function to compute the type and constraints of a binary operation *)
let ty_prim op ty1 ty2 = match op with 
  Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)  (* Plus operation expects two integers and results in an integer *)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)  (* Multiplication operation expects two integers and results in an integer *)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)  (* Less-than operation expects two integers and results in a boolean *)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)  (* And operation expects two booleans and results in a boolean *)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)  (* Or operation expects two booleans and results in a boolean *)

(* Function to perform type inference on an expression *)
let rec ty_exp (tyenv : tyenv) = function
  (* If the expression is a variable *)
  Var x ->
    (* Try to find the variable in the type environment *)
    (try 
      (* If the variable is found, get its type schema *)
      let TyScheme (vars, ty) = Environment.lookup x tyenv in
      (* Generate a substitution for the type variables in the type schema *)
      let s = List.map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
      (* Return the substitution and the substituted type *)
      ([], subst_type s ty)
    (* If the variable is not found in the type environment, raise a variable not bound error *)
    with Environment.Not_bound -> err ("variable not bound: " ^ x))
  (* If the expression is an integer literal, return the integer type *)
  | ILit _ -> ([],TyInt)
  (* If the expression is a boolean literal, return the boolean type *)
  | BLit _ -> ([],TyBool)
  (* If the expression is a binary operation *)
  | BinOp (op, exp1, exp2) -> 
    (* Try to perform type inference on the subexpressions *)
    (try
      (* Perform type inference on the first subexpression *)
      let (s1, ty1) = ty_exp tyenv exp1 in
      (* Perform type inference on the second subexpression *)
      let (s2, ty2) = ty_exp tyenv exp2 in
      (* Get the type and constraints of the binary operation *)
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      (* Combine the constraints of the subexpressions and the binary operation *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in 
      (* Unify the constraints to get a substitution *)
      let s3 = unify eqs in 
      (* Return the substitution and the substituted type of the binary operation *)
      (s3, subst_type s3 ty) 
    (* If type inference fails, raise an error *)
    with
      _ ->  let op_str = id_of_binop (var_of_binop op) in 
            if op = Plus || op = Mult || op = Lt then 
            err ("Both arguments must be integer: " ^ op_str ) 
            else  err ("Both arguments must be boolean: " ^ op_str ) )  
  (* If the expression is an if-then-else expression *)
  | IfExp (exp1, exp2, exp3) -> 
    (* Try to perform type inference on the subexpressions *)
    (try
      (* Perform type inference on the condition expression *)
      let (s1, ty1) = ty_exp tyenv exp1 in
      (* Perform type inference on the then expression *)
      let (s2, ty2) = ty_exp tyenv exp2 in
      (* Perform type inference on the else expression *)
      let (s3, ty3) = ty_exp tyenv exp3 in
      (* Combine the constraints of the subexpressions and add constraints that the condition type is boolean and the then and else types are the same *)
      let eqs =  (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1, TyBool)] @ [(ty2, ty3)] in
      (* Unify the constraints to get a substitution *)
      let s4 = unify eqs in 
      (* Return the substitution and the substituted type of the then/else expression *)
      (s4, subst_type s4 ty2) 
    (* If type inference fails, raise an error *)
    with _ -> err ("Type Error in if expression: if") )
  (* If the expression is a let expression *)
  | LetExp (e_ls, restexp) -> 
    (* Perform type inference on each binding in the let expression *)
    let id_s_ty = List.map (fun (id,e) -> (id, ty_exp tyenv e)) e_ls in
    (* Get the substitution for each binding *)
    let e_s = get_s id_s_ty in 
    (* Convert each substitution to a list of type equations *)
    let e_eqs = List.map eqs_of_subst e_s in 
    (* Combine the lists of type equations into a single list *)
    let e_eqs_app = List.fold_left append [] e_eqs in 
    (* Extend the type environment with each binding, using the closure of the inferred type under the substitution *)
    let newtyenv = List.fold_left 
        (fun tyenv' (id', (s', ty')) -> Environment.extend id' (closure ty' tyenv' s') tyenv') tyenv id_s_ty in 
    (* Perform type inference on the body of the let expression under the extended type environment *)
    let (rest_s, rest_ty) = ty_exp newtyenv restexp in
    (* Combine the type equations from the bindings and the body *)
    let eqs = e_eqs_app @ eqs_of_subst rest_s in
    (* Unify the type equations to get a substitution *)
    let s = unify eqs in 
    (* Return the substitution and the substituted type of the body *)
    (s,subst_type s rest_ty)
  (* If the expression is a let-rec expression *)
  | LetRecExp (id, para, exp1, exp2) -> 
    (* Create fresh type variables for the parameter and the body of the function *)
    let ty_para = TyVar (fresh_tyvar ()) in
    let ty_exp1 = TyVar (fresh_tyvar ()) in
    (* Create a function type from the parameter type to the body type *)
    let ty_id = TyFun (ty_para, ty_exp1) in
    (* Extend the type environment with the function under a type schema *)
    let newtyenv = Environment.extend id (tysc_of_ty ty_id) tyenv in
    (* Extend the type environment with the parameter under a type schema *)
    let newtyenv2 = Environment.extend para (tysc_of_ty ty_para) newtyenv in
    (* Perform type inference on the body of the function under the extended type environment *)
    let (s_e1, ty_e1) = ty_exp newtyenv2 exp1 in
    (* Unify the inferred type of the body with the expected type *)
    let s1 = unify (eqs_of_subst s_e1 @ [(ty_e1, ty_exp1)]) in
    (* Substitute the unified type into the function type *)
    let newty = subst_type s1 ty_id in
    (* Extend the type environment with the function under a type schema *)
    let newtyenv3 = Environment.extend id (closure newty tyenv s_e1) tyenv in
    (* Perform type inference on the rest of the expression under the extended type environment *)
    let (s_e2, ty_e2) = ty_exp newtyenv3 exp2 in
    (* Combine the type equations from the body and the rest of the expression, and add a type equation for the expected and inferred types of the body *)
    let eqs = (eqs_of_subst s_e2) @ (eqs_of_subst s_e1) @ [(ty_e1,ty_exp1)] in
    (* Unify the type equations to get a substitution *)
    let s = unify eqs in 
    (* Return the substitution and the substituted type of the rest of the expression *)
    (s, subst_type s ty_e2)
  (* If the expression is a function expression *)
  | FunExp (id, exp) ->
    (* Create a fresh type variable for the domain of the function *)
    let domty = TyVar (fresh_tyvar ()) in
    (* Perform type inference on the body of the function under an extended type environment *)
    let s, ranty = 
      ty_exp (Environment.extend id (tysc_of_ty domty) tyenv) exp in
    (* Return the substitution and a function type from the substituted domain type to the range type *)
    (s, TyFun (subst_type s domty, ranty))
  (* If the expression is a function application *)
  | AppExp (exp1, exp2) -> 
    (* Perform type inference on the function and the argument *)
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    (* Create a fresh type variable for the result of the function *)
    let domty = TyVar (fresh_tyvar ()) in
    (* Create a type equation for the function type and add it to the type equations from the function and the argument *)
    let eqs = (ty1, TyFun(ty2, domty)) :: (eqs_of_subst s1) @ (eqs_of_subst s2) in 
    (* Unify the type equations to get a substitution *)
    let s3 = unify eqs in 
    (* Return the substitution and the substituted result type *)
    (s3, subst_type s3 domty) 
  (* If the expression is not handled, raise an error *)
  | _ -> err ("Not Implemented!")

(* Function to perform type inference on a declaration *)
let ty_decl tyenv = function
  (* If the declaration is an expression *)
  Exp e -> 
    (* Perform type inference on the expression and return a singleton list with the type and the original type environment *)
    (["-", get_right (ty_exp tyenv e)], tyenv)
  (* If the declaration is a list of bindings *)
  | Decl e_ls ->
    (* Perform type inference on each binding *)
    let id_s_ty = List.map (fun (id,e) -> (id, ty_exp tyenv e)) e_ls in
    (* Create a list of pairs of each id and its inferred type *)
    let id_ty = List.map (fun (id,e) -> (id, get_right (ty_exp tyenv e))) e_ls in
    (* Extend the type environment with each binding, using the closure of the inferred type under the substitution *)
    let newtyenv = List.fold_left 
        (fun tyenv' (id, (s,ty)) -> Environment.extend id (closure ty tyenv' s) tyenv') tyenv id_s_ty in
    (* Return the list of id-type pairs and the extended type environment *)
    (id_ty, newtyenv)
  (* If the declaration is a recursive function *)
  | RecDecl (id, para, e) -> 
    (* Perform type inference on the function as a let-rec expression *)
    let (s, ty) = ty_exp tyenv (LetRecExp (id,para,e,Var id)) in
    (* Extend the type environment with the function under a type schema *)
    let newtyenv = Environment.extend id (closure ty tyenv s) tyenv in 
    (* Return a singleton list with the type and the extended type environment *)
    (["-", ty], newtyenv)
  (* If the declaration is a quit declaration, exit the program *)
  | QuitDecl -> exit 0 
