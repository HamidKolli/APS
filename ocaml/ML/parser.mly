%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO
%token SMCO
%token COMA
%token COL
%token TIMES
%token FLECHE
%token CONST
%token FUN
%token REC
%token IF
%token AND
%token OR
%token BOOL
%token INT

%token VAR
%token PROC
%token SET
%token IFSTAT
%token WHILE
%token CALL
%token VARARG
%token ADR

%token VEC
%token NTH
%token ALLOC
%token VSET
%token LEN
%token RETURN

%type <Ast.stat> stat
%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.typ list> typs
%type <Ast.typ> typ
%type <Ast.cmd list> cmds
%type <Ast.block> prog
%type <Ast.def> def
%type <Ast.arg list> args
%type <Ast.arg> arg
%type <Ast.block> block
%type <Ast.argp> argp
%type <Ast.argp list> argsp
%type <Ast.exprp> exprp
%type <Ast.exprp list> exprsp
%type <Ast.lvalue> lvalue
%type <Ast.stype> stype
%type <Ast.ret> ret

%start prog

%%
prog:  block { $1 }
;

cmds:
  stat                  { [ASTStat $1] }
  |ret                { [ASTReturn $1] }
  | def SMCO cmds {[ASTCmd($1,$3)]}
  | stat SMCO cmds {[ASTStatCmds($1,$3)]}
;

ret:
  RETURN expr {ASTRet($2)}
;
stat:
  ECHO expr             { ASTEcho($2) }
  |SET lvalue expr { ASTSet($2,$3)}
  |IFSTAT expr block block {ASTIf($2,$3,$4)}
  |WHILE expr block {ASTWhile($2,$3)}
  |CALL IDENT exprsp {ASTCall($2,$3)}
;

block :
  LBRA cmds RBRA {ASTBlock($2)}
;

def: 
  CONST IDENT typ expr {ASTConst($2,$3 , $4)}
  |FUN IDENT typ LBRA args RBRA expr {ASTFun($2,$3, $5, $7)}
  |FUN REC IDENT typ LBRA args RBRA expr {ASTFunRec($3,$4, $6, $8)}
  |VAR IDENT stype  {ASTVar($2,$3)}
  |PROC IDENT LBRA argsp RBRA block {ASTProc($2,$4, $6)}
  |PROC REC IDENT LBRA argsp RBRA block {ASTProcRec($3,$5,$7)}
  |FUN IDENT typ LBRA argsp RBRA block {ASTFunSp($2,$3, $5, $7)}
  |FUN REC IDENT typ LBRA argsp RBRA block {ASTFunSpRec($3,$4, $6, $8)}
;

stype : 
   BOOL {ASTStype ("bool") }
  | INT {ASTStype ("int")}
  | LPAR VEC stype RPAR {ASTStypeVEC($3)}
;
 
typ : 
  stype {ASTType($1)}
  | LPAR typs FLECHE typ  RPAR {ASTTypeFun($2,$4)} 
;

typs : typ  {[$1]}
      | typ TIMES typs {$1::$3} 
;

args : arg {[$1]}
      | arg COMA args {$1::$3}
;


arg : IDENT COL typ {ASTArg($1,$3)} 
;

argsp : argp {[$1]}
      | argp COMA argsp {$1::$3};

argp : IDENT COL typ {ASTArgp($1,$3)} 
      | VARARG IDENT COL typ {ASTArgpVar($2,$4)}
;

lvalue : 
       IDENT  {ASTLvalue($1)}
      | LPAR NTH lvalue expr RPAR {ASTLvalueNth($3,$4)}
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR    { ASTIf($3, $4, $5)}
| LPAR AND expr expr RPAR        { ASTAnd($3, $4) }
| LPAR OR expr expr  RPAR       { ASTOr($3, $4) }
| LPAR expr exprsp RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr { ASTLambda($2, $4) }
| LPAR ALLOC expr RPAR {ASTAlloc($3)}
| LPAR LEN expr RPAR {ASTLen($3)}
| LPAR NTH expr expr RPAR {ASTNth($3,$4)}
| LPAR VSET expr expr expr RPAR {ASTVset($3,$4,$5)}
;

exprp: 
  expr {AstExprp($1)}
  | LPAR ADR IDENT RPAR {AstExprpID($3)};
exprsp:
    exprp       { [$1] }
  | exprp exprsp { $1::$2 }

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;
