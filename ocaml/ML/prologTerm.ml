(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast


let rec print_type x = 
  match x with
    |ASTType s -> print_stype s
    |ASTTypeFun (_, t) -> print_type t;
and print_types ts = 
  match ts with
  | [] -> ()
  | t :: [] -> print_type t
  | t :: ts ->( print_type t;Printf.printf ","; print_types ts)

and print_stype st = 
  match st with 
    ASTStype s ->  Printf.printf"%s" s
    |ASTStypeVEC s -> (Printf.printf"vec(";print_stype s ;Printf.printf")")

let  print_arg arg = 
  match arg with
    ASTArg  (id,  t) -> (
    Printf.printf"(%s," id; 
    print_type t;
    Printf.printf")" ) 
let print_argp argp =
  match argp with
   ASTArgp  (id,  t) -> (
    Printf.printf"(%s," id; 
    print_type t;
    Printf.printf")" )
  | ASTArgpVar (id,  t) -> (
    Printf.printf"(%s,ref(" id; 
    print_type t;
    Printf.printf"))" )
let rec  print_argsp argsp = (
    match argsp with
    | [] -> ()
    | t :: [] ->  print_argp t;
    | t :: ts ->( 
    print_argp t;
    Printf.printf ", "; 
    print_argsp ts;
    )
)
  
let rec print_args args = (
  match args with
  | [] -> ()
  | t :: [] ->  print_arg t;
  | t :: ts ->( 
  print_arg t;
  Printf.printf ", "; 
  print_args ts;
  )
)




let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> (Printf.printf "id(%s)" x)
    | ASTApp(e, es) -> (
      Printf.printf"app(";
      print_expr e;
      Printf.printf",";
      Printf.printf "["; 
      print_exprsp es;
      Printf.printf "]"; 
      Printf.printf")";
      )
    | ASTIf (e1, e2, e3) -> (
      Printf.printf"if("; 
      print_expr e1;
      Printf.printf",";
      print_expr e2; 
      Printf.printf",";
      print_expr e3; 
      Printf.printf")";
    )
    | ASTAnd (e1, e2) -> (
      Printf.printf"and("; 
      print_expr e1;
      Printf.printf",";
      print_expr e2; 
      Printf.printf")";
    )
    | ASTOr (e1, e2) -> (
      Printf.printf"or("; 
      print_expr e1;
      Printf.printf",";
      print_expr e2; 
      Printf.printf")";
    )
    | ASTLambda (args, e) -> (
      Printf.printf"abs("; 
      Printf.printf "["; 
      print_args args;
      Printf.printf "]"; 
      Printf.printf",";
      print_expr e;
      Printf.printf")"; 
    )
    | ASTAlloc e -> (
      Printf.printf"alloc(";
      print_expr e;
      Printf.printf")"; 
    )
    | ASTLen e -> (
      Printf.printf"len(";
      print_expr e;
      Printf.printf")"; 
    )
    | ASTNth (e1,e2) -> (
      Printf.printf"nth(";
      print_expr e1;
      Printf.printf","; 
      print_expr e2;
      Printf.printf")"; 
    )
    | ASTVset (e1,e2,e3) -> (
      Printf.printf"vset(";
      print_expr e1;
      Printf.printf","; 
      print_expr e2;
      Printf.printf","; 
      print_expr e3;
      Printf.printf")"; 
    )
and print_exprs es =
  match es with
      [] -> ()
    | [e] -> print_expr e
    | e::es -> (
      print_expr e;
      print_char ',';
      print_exprs es
    )
and print_exprp ep = 
  match ep with
  | AstExprp (e)-> (
    Printf.printf"expr(";
    print_expr e;
    Printf.printf")";)
  | AstExprpID (id) ->(Printf.printf"adr(%s)" id);

and print_exprsp esp =
    match esp with
      [] -> ()
      | [e] -> print_exprp e
      | e::es -> (
      print_exprp e;
      print_char ',';
      print_exprsp es
      )
  

let rec print_def def = 
  match def with
  |ASTConst (id, t, e) -> (
    Printf.printf"const(";
    Printf.printf"id(%s)" id;  
    Printf.printf",";
    print_type t;
    Printf.printf",";
    print_expr e;
    Printf.printf")";
  ) 
   | ASTFun (id, t, args, e) ->(
    Printf.printf"fun( ";
    Printf.printf"id(%s)" id;  
    Printf.printf", ";
    print_type t;
    Printf.printf", [";
    print_args args;
    Printf.printf"], ";
    print_expr e;
    Printf.printf")";
  ) 
  |ASTFunRec (id, t, args, e) ->
    (
    Printf.printf"funRec( ";
    Printf.printf"id(%s)" id;  
    Printf.printf", ";
    print_type t;
    Printf.printf", [";
    print_args args;
    Printf.printf"], ";
    print_expr e;
    Printf.printf")";
  )
  |ASTVar (id, t) -> (
    Printf.printf"var(";
    Printf.printf"id(%s)" id;  
    Printf.printf",ref(";
    print_stype t;
    Printf.printf"))";
  )
  | ASTProc (id, args, b) ->(
    Printf.printf"proc( ";
    Printf.printf"id(%s)" id;  
    Printf.printf", [";
    print_argsp args;
    Printf.printf"], ";
    print_block b;
    Printf.printf")";
  ) 
  |ASTProcRec (id, args, b) ->(
    Printf.printf"procRec( ";
    Printf.printf"id(%s)" id;  
    Printf.printf", [";
    print_argsp args;
    Printf.printf"], ";
    print_block b;
    Printf.printf")";
  )
  | ASTFunSp (id, t, argsp, blk) ->(
    Printf.printf"funSp( ";
    Printf.printf"id(%s)" id;  
    Printf.printf", ";
    print_type t;
    Printf.printf", [";
    print_argsp argsp;
    Printf.printf"], ";
    print_block blk;
    Printf.printf")";
  ) 
  |ASTFunSpRec (id, t, argsp, blk) ->
    (
    Printf.printf"funSpRec( ";
    Printf.printf"id(%s)" id;  
    Printf.printf", ";
    print_type t;
    Printf.printf", [";
    print_argsp argsp;
    Printf.printf"], ";
    print_block blk;
    Printf.printf")";
  )

  
  

and print_stat s =
  match s with
      ASTEcho e -> (
        Printf.printf("echo(");
        print_expr(e);
        Printf.printf(")")
      )
      |ASTSet(lv,e) ->(
        Printf.printf("set(");
        print_lvalue lv;
        Printf.printf", ";
        print_expr e;
        Printf.printf")"
      ) 
      |ASTIf(e,b1,b2) ->(
        Printf.printf("ifStat(");
        print_expr e;
        Printf.printf", ";
        print_block b1;
        Printf.printf", ";
        print_block b2;
        Printf.printf")"
      )
      | ASTWhile(e,b)-> (
        Printf.printf("while(");
        print_expr e;
        Printf.printf", ";
        print_block b;
        Printf.printf")"
      )
      | ASTCall(id,es)-> (
        Printf.printf("call(");
        Printf.printf"id(%s)" id;
        Printf.printf", [";
        print_exprsp es;
        Printf.printf"])"
      )

and print_lvalue v =
  match v with
  ASTLvalue id ->  Printf.printf"id(%s)" id;
  |ASTLvalueNth (lv,e) -> (
    Printf.printf"nth(";
    print_lvalue lv;
    Printf.printf",";
    print_expr e;
    Printf.printf")";
  )

and  print_cmd c =
  match c with
      ASTStat s -> print_stat s
      |ASTReturn ret -> print_return ret
      |ASTCmd (def, cmds) ->(
        print_def def; 
        Printf.printf(",");
        print_cmds cmds;
        )
      |ASTStatCmds (stat, cmds) ->(
        print_stat stat; 
        Printf.printf(",");
        print_cmds cmds;
        )

and print_cmds cmds = 
  match cmds with
  |[] -> ()
  |c :: xs ->( print_cmd c ; print_cmds xs)

and print_return ret = 
  match ret with
  ASTRet e ->  (Printf.printf("return(");
                print_expr e;
                Printf.printf(")"))
	
and print_block block = 
  match block with
    |ASTBlock cmd -> Printf.printf("block(["); print_cmds cmd; Printf.printf("])");

and print_prog p =
  Printf.printf("prog(");
  print_block p;
  Printf.printf(").")
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string "\n"
  with Lexer.Eof ->
    exit 0
