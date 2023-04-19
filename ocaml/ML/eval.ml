open Ast


exception ExceptEval of string

module EnvMap = Map.Make(String)

module TasMap = Map.Make(Int)


type value = Z of int
           |F of expr * string list * (value EnvMap.t)
           |FR of expr * string * string list * (value EnvMap.t)
           |A of int
           |P of block * string list * (value EnvMap.t)
           |PR of block * string list * (value EnvMap.t)
           |FSP of block * string list * (value EnvMap.t) * (value TasMap.t)
           |FSPR of block * string * string list * (value EnvMap.t) * (value TasMap.t)
           |B of int * int
           |Void


let tas = ref  TasMap.empty;;

let tasAdress = ref 0;;


let rec eval_expr e env =
  let app_bool op es = (match List.map (fun x -> eval_exprp x env) es  with
        [Z (n1);Z (n2)] ->  if op n1 n2 then Z (1) else Z (0)  
      | _-> raise (ExceptEval ("application bool error"))
    )in 
  let app op es = (match List.map (fun x -> eval_exprp x env) es  with
        [Z ( n1);Z (n2)] ->  Z (op n1 n2)
      | _-> raise (ExceptEval ("application error"))
    )in 
  match e with
    ASTNum n -> Z( n)
  | ASTId x -> (match x with 
        "true" -> Z( 1)
      |"false" -> Z( 0)
      |_->match EnvMap.find x env with
          Z (e) -> Z (e)
        | A (n) -> TasMap.find n !tas
        |x -> x
    )
  | ASTApp(e, es) -> 
      ( try 
          match eval_expr e env with
            F (e ,args, env2) -> 
              eval_expr e (List.fold_right2 (fun arg exp en -> EnvMap.add arg (eval_exprp exp env) en) args es env2)
          | FR (e, id ,args, env2) -> 
              eval_expr e (EnvMap.add id (FR (e, id ,args, env2)) (List.fold_right2 (fun arg exp en -> EnvMap.add arg (eval_exprp exp env) en) args es env2))
          | FSP (blk ,args, env2,tas2) -> (
              let tmp = !tas in 
              tas := tas2;
              let result = eval_block blk (List.fold_right2 (fun arg exp en -> EnvMap.add arg (eval_exprp exp env) en) args es env2)
              in
              tas := tmp; result
            )
          | FSPR (blk, id ,args, env2,tas2) -> (
              let tmp = !tas in 
              tas := tas2;
              let result = eval_block blk (EnvMap.add id (FSPR (blk, id ,args, env2,tas2)) (List.fold_right2 (fun arg exp en -> EnvMap.add arg (eval_exprp exp env) en) args es env2))
              in
              tas := tmp; result
            )
          | _ -> raise (ExceptEval (Printf.sprintf "Application error: id must be function name"))
          
        with        
          _ -> (match e with
              ASTId x -> (match x with
                  "not" -> (match List.map (fun x -> eval_exprp x env) es with
                      [Z (n)] ->  if n == 0 then Z ( 1) else Z ( 0)
                    | _ -> raise (ExceptEval ("application not error"))
                  )
                |"eq" -> app_bool (==) es
                |"lt" -> app_bool (<) es
                |"add" -> app (+) es 
                |"sub" -> app (-) es 
                |"mul" -> app ( * ) es 
                |"div" -> app (/) es
                |_ -> raise (ExceptEval (Printf.sprintf "application error id not found %s" x))           
              
              )
            |_ -> raise (ExceptEval ("Application Error : the expression must be an id"))
          )
      )
        
  | ASTIf (e1, e2, e3) -> (
      match eval_expr e1 env with
        Z ( n) ->
          if n == 1 then eval_expr e2 env else eval_expr e3 env
      |_ -> raise (ExceptEval ("If Clause: condition must return a bool"))
    )
  | ASTAnd (e1, e2) -> (
      match eval_expr e1 env with
        Z ( n) ->
          if n == 1
          then  eval_expr e2 env  
          else Z ( 0) 
      |_ -> raise (ExceptEval ("If Clause: condition must return a bool"))
              
    )
  | ASTOr (e1, e2) -> ( 
      match eval_expr e1 env with
        Z ( n) ->
          if n == 1
          then 
            Z ( 1) 
          else eval_expr e2 env 
      |_ -> raise (ExceptEval ("If Clause: condition must return a bool"))
      
                    
    )
  | ASTLambda (args, e) -> (
      let aux x xs = 
        match x with 
          ASTArg (s , _) -> s :: xs
      in F (e,(List.fold_right aux args []), env)
    )

  | ASTAlloc len -> (
      let adr = !tasAdress + 1 in
      let rec alloc n =
        if n == 0 then
          ()
        else(tasAdress := !tasAdress + 1 ; tas := (TasMap.add (!tasAdress)  (Z( 0)) (!tas)); alloc (n-1) )
      in
      match eval_expr len env with
        Z(n) -> if n > 0 then 
            ( alloc n ; B(adr,n) ;)
          else (raise (ExceptEval ("Alloc Clause: len must be positif")))
      |_ -> raise (ExceptEval ("Alloc Clause: len must be positif"))
    )
  | ASTLen tab ->(
      match eval_expr tab env with
        B(_,n) -> Z(n)
      |_ -> raise (ExceptEval ("Len Clause: var must be a tab"))
    )
  | ASTNth (tab, index) -> (
      match eval_expr tab env with
        B(a,_) -> (match eval_expr index env with
            Z(i) -> if i >= 0 then TasMap.find (a+i) !tas
              else raise (ExceptEval ("NTH Clause: index must be an positif integer"))
          |_ -> raise (ExceptEval ("NTH Clause: index must be an positif integer"))
        )
      |_ -> raise (ExceptEval ("NTH Clause: var must be a tab"))

    )
  | ASTVset (tab, index, value) ->(
      match eval_expr tab env with
        B(a,n) -> (match eval_expr index env with
            Z(i) -> if i >= 0 then 
                (tas := TasMap.add (a+i) (eval_expr value env) !tas;B(a,n);)
              else raise (ExceptEval ("NTH Clause: index must be an positif integer"))
          | _ -> raise (ExceptEval ("NTH Clause: index must be an positif integer"))
        )
      |_ -> raise (ExceptEval ("NTH Clause: var must be a tab"))
    )

and eval_exprp exprp env = 
  match exprp with
    AstExprp e -> eval_expr e env
  |AstExprpID id -> EnvMap.find id env


and eval_def def env= 
  let aux x xs = match x with ASTArg (s , _) -> s :: xs
  in
  let aux2 x xs = match x with 
      ASTArgp (s , _) -> s :: xs
    |ASTArgpVar (s , _) -> s :: xs
  in
  match def with
  |ASTConst (id, _, e) -> EnvMap.add id (eval_expr e env) env;
  |ASTFun (id, _, args, e) -> EnvMap.add id (F (e,(List.fold_right aux args [] ), env)) env
  |ASTFunRec (id, _, args, e) ->EnvMap.add id (FR (e, id, (List.fold_right aux args [] ), env)) env 
  |ASTVar (id, _) ->(tasAdress := !tasAdress + 1;tas := TasMap.add (!tasAdress) (Z( 0)) (!tas) ;EnvMap.add id (A (!tasAdress)) env)
  |ASTProc (id,args,blk) -> EnvMap.add id (P (blk, (List.fold_right aux2 args [] ), env)) env
  |ASTProcRec (id,args,blk) ->EnvMap.add id (PR (blk, (List.fold_right aux2 args []), env)) env
  |ASTFunSp (id,_,args,blk) -> EnvMap.add id (FSP (blk, (List.fold_right aux2 args [] ), env,!tas)) env
  |ASTFunSpRec (id,_,args,blk) ->EnvMap.add id (FSPR (blk, id,(List.fold_right aux2 args []), env,!tas)) env
        


and eval_stat s env=
  match s with
    ASTEcho e -> (
      match eval_expr e env with 
        Z( n) -> (Printf.printf"%d\n"  n; Void)
      | _ -> raise  (ExceptEval "ECHO Clause : need a integer argument")   
    )
  |ASTSet (lv, e) -> (match eval_lvalue lv env with
        adr -> (tas := TasMap.add adr (eval_expr e env) !tas ; Void)
    )
  |ASTIf (e,blk1,blk2) -> (match eval_expr e env with
        Z( n) ->
          if n == 1 then eval_block blk1 env else eval_block blk2 env
      |_ -> raise (ExceptEval ("IF Clause: condition must be a bool"))
    )
  |ASTWhile (e,blk)->  (
      let rec loop () =
        match eval_expr e env with
          Z( n) ->
            if n == 1 then (match eval_block blk env with 
              Void -> loop ()
              |otherwise -> otherwise)
            else Void
        |_ -> raise (ExceptEval ("WHILE Clause: condition must be a bool"))
      in loop ()
    )
  |ASTCall (id, argsp) -> match EnvMap.find id env with 
      P (blk ,argsID, env2) -> 
        eval_block blk (List.fold_right2 (fun arg exprp en -> EnvMap.add arg (eval_exprp exprp env) en) argsID argsp env2)
    | PR ((blk ,argsID, env2)) ->(
        let env3 = List.fold_right2 (fun arg exprp en -> EnvMap.add arg (eval_exprp exprp env) en) argsID argsp env2 in 
        eval_block blk (EnvMap.add id (PR(blk ,argsID, env3)) env3)
      )
    | _ -> raise (ExceptEval (Printf.sprintf "CALL Clause: id not found %s" id))

and eval_ret ret env =
  match ret with
    ASTRet e -> eval_expr e env

and eval_lvalue lv env =     
  match lv with
    ASTLvalue id -> (let adr = EnvMap.find id env in 
                     match adr with 
                       A(adr) -> adr
                     | _ -> raise (ExceptEval ("LVALUE error : id is not a address")))
  | ASTLvalueNth (lv,e) -> 
      ( match lv with
          ASTLvalue id -> (let adr = EnvMap.find id env in 
                           (match adr with 
                              A(adr) -> (match TasMap.find adr (!tas) with
                                  B(adr,_) -> (match eval_expr e env with
                                      Z(index) -> adr + index
                                    | _ -> raise (ExceptEval ("LVALUE error : Index must be a positive integer"))
                                  )
                                |_ -> raise (ExceptEval ("LVALUE error: id is not a tab1"))
                              )
                            | _ -> raise (ExceptEval ("LVALUE error: id is not an address"))
                           )
                          )
        | ASTLvalueNth (lv2,e2) -> (match eval_lvalue (ASTLvalueNth (lv2,e2)) env with
              adr -> (match TasMap.find adr !tas with
                  B(adr2,_) ->(match eval_expr e env with
                      Z(index) -> adr2 + index
                    | _ -> raise (ExceptEval ("LVALUE error : Index must be a positive integer"))
                  ) 
                |_ -> raise (ExceptEval ("LVALUE error: id is not a tab2"))
              )
          )              
      )

and eval_cmd c env=
  match c with
    ASTStat s -> eval_stat s env
  |ASTReturn ret -> eval_ret ret env
  |ASTCmd (def, cmds) -> eval_cmds cmds (eval_def def env)
  |ASTStatCmds (st,cmds) -> eval_stat st env ; eval_cmds cmds env
    

and eval_cmds cmds env= 
  match cmds with
  |[] -> raise ( ExceptEval "Command not found")
  |[c] ->  eval_cmd c env
  |c :: xs ->( eval_cmd c env ; eval_cmds xs env)
             

and eval_block blk env = 
  match blk with
    ASTBlock (cmds) -> eval_cmds cmds env

let eval_prog p =
  let env = EnvMap.empty in
  eval_block p env;

;;


let fname = Sys.argv.(1) in
let ic = open_in fname in
try
  let lexbuf = Lexing.from_channel ic in
  let p = Parser.prog Lexer.token lexbuf in
  eval_prog p;
  print_string "\n"
with Lexer.Eof ->
  exit 0

