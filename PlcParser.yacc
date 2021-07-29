%%

%name PlcParser

%pos int

%term VAR
    | PLUS | MINUS | MULT | DIV | EQ
    | LPAR | RPAR
    | SEMIC
    | NAME of String | CINT of int
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



