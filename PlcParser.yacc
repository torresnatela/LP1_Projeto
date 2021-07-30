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
    | SEMIC | VIRG
    | ARR | BAR | UNDER | DAR
    | NAME of string | INT | BOOL 
    | EOF

%noterm Prog of expr Decl 
        | Decl 
        | Expr of AtomExpr AppExpr MatchExpr 
        | AtomExpr of Const 
        | AppExpr of AtomExpr AppExpr
        | Const of expr
        | MatchExpr of (expr option * expr) list
        | CondExpr of expr option
        | Args of (plcType * string) list
        | Params of (plcType * string) list
        | TypedVar of plcType * string
        | Type of plcType
        | AtomicType of plcType
        | Types of plcType list
        | Nat of int
        | Name of string



%right SEMIC TWOP RPAR RCOL RKEY
%left EQ PLUS MINUS MULTI DIV AND DIF LESS LEQ LPAR LCOL LKEY
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
    | FUN REC NAME ARGS WOP Type EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))

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

Const : TRUE (ConB(TRUE))
| FALSE (ConB(FALSE))
| Nat (ConI(Nat))
| LPAR RPAR (List([]))
| LPAR Type LKEY RKEY RCOL (ESeq(Type))

Comps : Expr VIRG Expr (Expr1::Expr2::[])
| Expr VIRG Comps (Expr::Comps)

MatchExpr : END ([]) (*Lista Vazia*)
    | BAR CondExpr ARR Expr MatchExpr ([(CondExpr, Expr)] @ MatchExpr)

CondExpr : Expr (SOME(Expr))
| UNDER (NONE)

Args : LPAR RPAR ([]) 
    | LPAR Params DPAR (Params)

Params : TypedVar ([TypedVar])
    | TypedVar VIRG Params ([TypedVar]@Params)

TypedVar : Type NAME ((Type, NAME))

Type : AtomicType (AtomicType)
    | LPAR Types RPAR (ListT Types) (*list Type*)
    | LCOL Type RCOL (SeqT Type) (*sequence Type*)
    | Type TPRODUZ Type (FunT(Type1, Type2)) (*function Type*)

AtomicType : NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)

Types: Type VIRGULA Type ([Type1, Type2]) 
    | Type VIRGULA Types ([Type1] @ Types) 
