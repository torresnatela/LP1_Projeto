(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) =
    case s of
        "var" => VAR (lpos, rpos)
        "if" => IF (lpos, rpos)
        "else" => ELSE (lpos, rpos)
        "Bool" => BOOL (lpos, rpos)
        "end" => END (lpos, rpos)
        "false" => FALSE(lpos, rpos)
        "fn" => FN(lpos, rpos)
        "fun" => FUN(lpos,rpos) 
        "hd" => HD(lpos,rpos)
        "Int" => INT(lpos,rpos)  
        "ise" => ISE(lpos,rpos)
        "match" => MATCH(lpos,rpos)  
        "Nil" => NIL(lpos, rpos)
        "print" => PRINT(lpos,rpos)
        "rec" => REC(lpos,rpos)  
        "then" => THEN(lpos, rpos)
        "tl" => TL(lpos, rpos)
        "true" => TRUE(lpos, rpos)
        "with" => WITH(lpos,rpos) 
        | _   => NAME (s, lpos, rpos)


val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0


fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end


fun eof () = Tokens.EOF(0,0)

fun strToInt s =
    case Int.fromString s of
        SOME i => i
      | NONE => raise Fail ("Could not convert string to int")


fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;

%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (CINT(strToInt(yytext), yypos, yypos));
{identifier} => (keyword(yytext, yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULT(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQ(yypos, yypos));
";" => (SEMIC(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"[" => (LCOL(yypos, yypos));
"]" => (RCOL(yypos, yypos));
"::" => (TWOP(yypos, yypos)); 
"<" => (LESS(yypos, yypos));
"<=" => (LEQ(yypos, yypos));
"&&" => (AND(yypos, yypos));
"!" => (NOT(yypos, yypos));
"!=" => (DIF(yypos, yypos));
"{" => (LKEY(yypos, yypos));
"}" => (RKEY(yypos, yypos));
"," => (DOT(yypos, yypos));


. => (error("\n *** Lexer error: character invalid ***\n"); raise
Fail("Lexer error: character invalid" ^yytext));










