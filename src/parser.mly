%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT GT
%token IF THEN ELSE TRUE FALSE
%token AND OR

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }

Expr :
    e=IfExpr  { e }
  | e=OrExpr  { e }

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
  
PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
