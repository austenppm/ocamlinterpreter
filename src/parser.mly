%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT GT
%token IF THEN ELSE TRUE FALSE
%token AND OR
%token LET IN EQ LETAND
%token RARROW FUN 

%token <string> NEUTOP
%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET decls=LetDecls SEMISEMI { LetDecls decls } 
  
Expr :
    e=IfExpr  { e }
  | e=LetExpr { e } 
  | e=LetAndExpr { e }
  | e=OrExpr  { e }
  | e=FunExpr { e }
  | e=MExpr   { e }
  | LPAREN op=NEUTOP RPAREN e=Expr { NeutralOp (op, e) }

LetExpr :
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
  | LET decls=LetDecls IN e2=Expr { LetAndExp (decls, e2) }

LetDecls :
     x=ID EQ e=Expr { [(x, e)] }
   | x=ID EQ e=Expr LET decls=LetDecls { (x, e) :: decls }
   | x=ID EQ e=Expr LETAND decls=LetDecls { (x, e) :: decls }

LetAndExpr :
     LET x=ID EQ e1=Expr LETAND decls=LetDecls IN e2=Expr { LetAndExp ((x, e1) :: decls, e2) }
     
OrExpr :
    l=AndExpr OR r=OrExpr  { LogicOp (Or, l, r) }
  | e=AndExpr { e }

AndExpr :
    l=CompExpr AND r=AndExpr { LogicOp (And, l, r) }
  | e=CompExpr { e }

CompExpr :
    l=AddExpr LT r=AddExpr { BinOp (Lt, l, r) }
  | l=AddExpr GT r=AddExpr { BinOp (Gt, l, r) }
  | e=AddExpr { e }

AddExpr :
    l=AddExpr PLUS r=MultExpr { BinOp (Plus, l, r) }
  | e=MultExpr { e }

MultExpr :
    l=MultExpr MULT r=Atom { BinOp (Mult, l, r) }
  | e=Atom { e }

Atom :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

MExpr :
    e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
  | e=AppExpr { e } 

AppExpr :
    e1=AppExpr e2=Atom { AppExp (e1, e2) }
  | e=Atom { e }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }
