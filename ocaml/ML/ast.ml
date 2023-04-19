(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)




type typ = 
  ASTType of stype
  |ASTTypeFun of typ list * typ
and
  stype = 
    ASTStype of string
    |ASTStypeVEC of stype
and 
 arg = 
  ASTArg of string * typ
and
 expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * exprp list
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTLambda of arg list * expr
  | ASTAlloc of expr
  | ASTLen of expr
  | ASTNth of expr * expr
  | ASTVset of expr * expr * expr
and
 def = 
  ASTConst of string * typ * expr 
  |ASTFun of string * typ * arg list * expr
  |ASTFunRec of string * typ * arg list * expr
  |ASTVar of string * stype
  |ASTProc of string * argp list * block
  |ASTProcRec of string * argp list * block
  |ASTFunSp of string * typ * argp list * block
  |ASTFunSpRec of string * typ * argp list * block
and
  

 stat =
    ASTEcho of expr
    |ASTSet of lvalue * expr
    |ASTIf of expr * block * block
    |ASTWhile of expr * block 
    |ASTCall of string * exprp list
and
 cmd =
    ASTStat of stat
    |ASTReturn of ret
    |ASTCmd of def *  cmd list
    |ASTStatCmds of stat *  cmd list
 
and
 block = ASTBlock of cmd list
and
  argp = ASTArgp of string * typ
        | ASTArgpVar of string * typ
and 
  exprp =  AstExprp of expr
        |AstExprpID of string

and 
  lvalue = ASTLvalue of string
        | ASTLvalueNth of lvalue * expr
and 
  ret = ASTRet of expr
