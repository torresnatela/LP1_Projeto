%%

%name PlcParser

%pos int

%term VAR |
    | IF | ELSE
    | END | FALSE | TRUE
    | FN | FUN | HD
    | ISE | MATCH | NIL | WITH
    | PRINT | REC | THEN | TL
    | PLUS | MINUS | MULT | DIV | EQ
    | LPAR | RPAR | LCOL | RCOL | LKEY | RKEY
    | TWOP | WOP
    | LESS | LEQ
    | AND | NOT | DIF
    | SEMIC | DOT
    | ARR | BAR | UNDER | DAR
    | NAME of string | INT | BOOL 
    | EOF

%noterm Prog of expr Decl | Decl | Expr of AtomExpr AppExpr MatchExpr | AtomExpr of Const | AppExpr of AtomExpr AppExpr 


%right SEMIC TWOP RPAR RCOL RKEY
%left EQ PLUX MINUS MULTI DIV AND DIF LESS LEQ LPAR LCOL LKEY
%nonassoc IF HD ISE TL PRINT NOT 

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr) | Decl (Decl) 
    | Prog (Let(NAME, Expr, Prog))

Decl : 
    | VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    | FUN NAME ARGS EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    | FUN REC NAME ARGS WOP TYPE EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))

Expr : AtomExpr (AtomExpr) | AppExpr(AppExpr) | MatchExpr (MatchExpr)
    | IF Expr THEN Expr ELSE Expr
    | MATCH Expr WITH MatchExpr
    | NOT Expr (Prim("!", Expr1))
    | MINUS Expr (Prim("-", Expr1))
    | HD Expr 
    | TL Expr
    | ISE Expr
    | PRINT Expr
    | Expr AND Expr (Prim("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim("-", Expr1, Expr2))
    | Expr MULT Expr (Prim("*", Expr1, Expr2))
    | Expr DIV Expr (Prim("/", Expr1, Expr2))
    | Expr EQ Expr (Prim("=", Expr1, Expr2))
    | Expr DIF Expr (Prim("!=", Expr1, Expr2))
    | Expr LESS Expr (Prim("<", Expr1, Expr2))
    | Expr LEQ Expr (Prim("<=", Expr1, Expr2))
    | Expr TWOP Expr (Prim("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim(";", Expr1, Expr2))
    | Expr LCOL Nat RCOL (Prim(Prim("[", Expr1), Nat1, "]"))

AtomExpr : Const (Const) 
    | NAME (Var(NAME))
    | LKEY Prog RKEY (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (Comps)
    | FN ARGS => Expr END 

AppExpr : AtomExpr(AtomExpr) AtomExpr(AtomExpr) | AppExpr(AppExpr) AtomExpr(AtomExpr)