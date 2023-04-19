(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof

}
rule token = parse
    [' ' '\t' '\n']       { token lexbuf }     (* skip blanks *)
  | '['              { LBRA }
  | ']'              { RBRA }
  | '('              { LPAR }
  | ')'              { RPAR }
  | ';'               { SMCO }
  | ',' { COMA }
  | ':' { COL }
  | '*' { TIMES }
  | "->" { FLECHE }
  | "CONST" { CONST } 
  | "FUN" { FUN }
  | "REC" { REC }
  | "VAR" { VAR } 
  | "PROC" { PROC }
  | "if" { IF } 
  | "and" { AND } 
  | "or" { OR } 
  | "bool" { BOOL }
  | "int" { INT }

  | "ECHO"  { ECHO }  
  | "SET"   { SET }  
  | "IF"    { IFSTAT }  
  | "WHILE" { WHILE }  
  | "CALL"  { CALL }  
  | "var"   { VARARG }  
  | "adr"   { ADR }

  | "vec"  { VEC}  
  | "alloc"    { ALLOC }  
  | "nth" { NTH }  
  | "len"  { LEN }  
  | "vset"   { VSET }  
  | "RETURN" { RETURN } 




  | ['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
  | ['a'-'z']['a'-'z''A'-'Z''0'-'9']* as lxm { IDENT(lxm) }
  | eof              { raise Eof }

