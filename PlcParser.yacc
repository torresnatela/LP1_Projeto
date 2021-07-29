%%

%name PlcParser

%pos int

%term VAR |
    | IF | ELSE
    | BOOL | END | FALSE | TRUE
    | FN | FUN | HD
    | ISE | MATCH | NIL | WITH
    | PRINT | REC | THEN | TL
    | PLUS | MINUS | MULT | DIV | EQ
    | LPAR | RPAR | LCOL | RCOL | LKEY | RKEY
    | TWOP
    | LESS | LEQ
    | AND | NOT | DIF
    | SEMIC | DOT
    | NAME of String | INT of int | BOOL of Bool
    | EOF

%noterm Prog of expr | Expr of expr | AtomExpr of expr | Const of expr |

%right SEMIC
%left EQ PLUX MINUS MULTI DIV

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))

Expr : AtomExpr (AtomExpr)
    | Expr PLUS Expr (Prim("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim("-", Expr1, Expr2))
    | Expr MULT Expr (Prim("-", Expr1, Expr2))
    | Expr DIV Expr (Prim("-", Expr1, Expr2))
    | Expr EQ Expr (Prim("=", Expr1, Expr2))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LPAR Expr RPAR (Expr)

Const : CINT (ConstI(CINT))



