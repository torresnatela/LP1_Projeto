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
    | NAT of int
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
        | NAT of int
        | Name of string



%right SEMIC TWOP RPAR RCOL RKEY
%left EQ PLUS MINUS MULTI DIV AND DIF LESS LEQ LPAR LCOL LKEY
%nonassoc IF HD ISE TL PRINT NOT 

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr) | 
    | Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    | FUN NAME Args EQ Expr SEMIC Prog (Let(Name, makeAnon(Args, Expr), Prog))
    | FUN REC NAME Args WOP Type EQ Expr SEMIC Prog (makeFun(Name, Args, Type, Expr, Prog)) 

Expr : AtomExpr (AtomExpr) 
    | AppExpr(AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NOT Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print"", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULT Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr DIF Expr (Prim2("!=", Expr1, Expr2))
    | Expr LESS Expr (Prim2("<", Expr1, Expr2))
    | Expr LEQ Expr (Prim2("<=", Expr1, Expr2))
    | Expr TWOP Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LCOL NAT RCOL (Item(NAT,Expr))

AtomExpr : Const (Const) 
    | NAME (Var(NAME))
    | LKEY Prog RKEY (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List Comps)
    | FN Args DARR Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : TRUE (ConB(TRUE))
| FALSE (ConB(FALSE))
| NAT (ConI(NAT))
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
    | Type ARR Type (FunT(Type1, Type2)) (*function Type*)

AtomicType : NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)

Types: Type VIRG Type ([Type1, Type2]) 
    | Type VIRG Types ([Type1] @ Types) 
