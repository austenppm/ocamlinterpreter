%{
open Syntax
%}

(* Tokens with no attribute values *)
%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR  
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ LETAND 
%token RARROW FUN 
%token DFUN 
%token REC 
%token QUIT

(* Tokens with attribute values *)
%token <int> INTV
%token <Syntax.id> ID

(* Define the start symbol of the grammar and its type *)
%start toplevel 
%type <Syntax.program> toplevel
%%

(* The top-level of a program can be an expression, a let declaration, a recursive function declaration, or a quit declaration *)
toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET e_ls=LetAndExpr SEMISEMI {Decl e_ls} 
  | LET REC x1=ID EQ FUN x2=ID RARROW e=Expr SEMISEMI { RecDecl (x1, x2, e)}
  | QUIT SEMISEMI {QuitDecl}

(* Grammar rules for different types of expressions *)
Expr :
    e=IfExpr { e }
  | e=ORExpr { e }  
  | e=LetExpr { e } 
  | e=FunExpr { e } 
  | e=DFunExpr { e } 
  | e=LetRecExpr { e } 

(* Let expression could have multiple bindings, separated by 'and', and optionally followed by 'in' and another expression *)
LetExpr :
  LET e_ls=LetAndExpr IN e2=Expr { LetExp (e_ls, e2) } 
| LET e_ls=LetAndExpr { LetExp (e_ls, Var "") } 

(* Multiple bindings in a let expression, separated by 'and' *)
LetAndExpr :
   x=ID EQ e=Expr { [(x,e)] }
  | x=ID EQ e1=Expr LETAND e2=LetAndExpr { (x,e1) :: e2 } 
  | x=ID args=MultiArgs EQ e=Expr { [(x, argstoFun args e )] } 
  | x=ID args=MultiArgs EQ e=Expr LETAND e2=LetAndExpr { (x, argstoFun args e ) :: e2 } 

(* Multiple arguments in a function *)
MultiArgs :
  x=ID { [x] }
| x=ID e=MultiArgs { x :: e }

(* Function expressions *)
FunExpr : 
   FUN e=FunMultiAgrsExpr { e } 
  
DFunExpr :
   DFUN x=ID RARROW e=Expr { DFunExp (x,e)} 

FunMultiAgrsExpr : 
   x=ID RARROW e=Expr { FunExp (x,e) }
  | x=ID e=FunMultiAgrsExpr { FunExp (x,e) }

(* Letrec expression for recursive functions *)
LetRecExpr :
   LET REC x1=ID EQ FUN x2=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x1,x2,e1,e2)} 

(* Binary expressions for logical or and and *)
ORExpr :  
 l=ANDExpr OR r=ORExpr { BinOp (Or, l, r) }
| e=ANDExpr { e }

ANDExpr :  
 l=LTExpr AND r=ANDExpr { BinOp (And, l, r) }
| e=LTExpr { e }

(* Binary expressions for less than *)
LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

(* Binary expressions for addition *)
PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

(* Binary expressions for multiplication *)
MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) } 
  | e=AppExpr { e } 
  
(* Function application expressions *)
AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

(* Infix expressions for binary operators *)
InfixExpr :
    LPAREN PLUS RPAREN { FunExp ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y")))}
  | LPAREN MULT RPAREN { FunExp ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y")))}
  | LPAREN LT RPAREN { FunExp ("x", FunExp ("y", BinOp (Lt, Var "x", Var "y")))}
  | LPAREN OR RPAREN { FunExp ("x", FunExp ("y", BinOp (Or, Var "x", Var "y"))) }
  | LPAREN AND RPAREN { FunExp ("x", FunExp ("y", BinOp (And, Var "x", Var "y"))) }
  
    
(* Atomic expressions *)
AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
  | e=InfixExpr { e }

(* If expressions *)
IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
