type_expr(_,id(true),bool).
type_expr(_,id(false),bool).
type_expr(_,id(not),fleche([bool],bool)).
type_expr(_,id(eq),fleche([int,int],bool)).
type_expr(_,id(lt),fleche([int,int],bool)).
type_expr(_,id(add),fleche([int,int],int)).
type_expr(_,id(sub),fleche([int,int],int)).
type_expr(_,id(mul),fleche([int,int],int)).
type_expr(_,id(div),fleche([int,int],int)).




type_expr(G,if(E1,E2,E3),T):- type_expr(G,E1,bool) , type_expr(G,E2,T) , type_expr(G,E3,T).
type_expr(G,and(E1,E2),bool):- type_expr(G,E1,bool) , type_expr(G,E2,bool).
type_expr(G,or(E1,E2),bool):- type_expr(G,E1,bool) , type_expr(G,E2,bool).

type_expr([(X,T)|_],id(X),T).
type_expr([(X,ref(T))|_],id(X),T).
type_expr([_|G],id(X),T):- type_expr(G,id(X),T).

type_expr(_,num(_),int).
type_expr(G,app(E,ES),T):- type_expr(G,E,fleche(TS,T)) , type_exprp_list(G,ES,TS).
type_expr(G,abs(Args,E),fleche(TS,T)):- args_to_type(Args,TS),append(Args,G,GN) ,type_expr(GN,E,T).

type_expr(G,alloc(E1),vec(_)):- type_expr(G,E1,int).
type_expr(G,len(E1),int):- type_expr(G,E1,vec(_)).
type_expr(G,nth(E1,E2),T):- type_expr(G,E1,vec(T)), type_expr(G,E2,int).
type_expr(G,vset(E1,E2,E3),vec(T)):- type_expr(G,E1,vec(T)), type_expr(G,E2,int), type_expr(G,E3,T).

type_exprp(G,expr(X),T):- type_expr(G,X,T).

type_exprp(G,adr(X),T):- type_expr(G,id(X),T).

type_exprp_list(_,[],[]).
type_exprp_list(G,[E],[T]) :-  type_exprp(G,E,T).
type_exprp_list(G,[E|ES],[T|TS]) :-  type_exprp(G,E,T) , type_exprp_list(G,ES,TS).

concat(X,G,[X|G]).

args_to_type([],[]).
args_to_type([(_,B)],[B]).
args_to_type([(_,B)|L],[B|Lnew]):- args_to_type(L,Lnew).

type_lvalue(G,X,T) :- type_expr(G,X,ref(T)).
type_lvalue(G,nth(X,E2),T) :- type_lvalue(G,X,vec(T)), type_expr(G,E2,int).

type_stat(G,echo(E),void) :- type_expr(G,E,int).
type_stat(G,set(X,E),void) :- type_lvalue(G,X,T), type_expr(G,E,T). 
type_stat(G,ifStat(E,B1,B2),T) :- type_expr(G,E,bool),type_block(G,B1,T),type_block(G,B2,T). 
type_stat(G,ifStat(E,B1,B2),plus(T,void)) :- type_expr(G,E,bool),type_block(G,B1,T),type_block(G,B2,void). 
type_stat(G,ifStat(E,B1,B2),plus(void,T)) :- type_expr(G,E,bool),type_block(G,B1,void),type_block(G,B2,T). 
type_stat(G,while(E,B),T) :- type_expr(G,E,bool),type_block(G,B,T).
type_stat(G,call(X,E),void) :- type_expr(G,X,fleche(TS,void)), type_exprp_list(G,E,TS).

type_ret(G,return(E),T) :- type_expr(G,E,T).

type_cmds(_,[],void).
type_cmds(G,[C],T) :- type_ret(G,C,T) .
type_cmds(G,[C],T) :- type_stat(G,C,T) .
type_cmds(G,[C|CS],T) :- type_def(G, C, Gn) , type_cmds(Gn,CS,T).  
type_cmds(G,[C|CS],T) :- type_stat(G, C, _) , type_cmds(G,CS,T).  

type_block(G,block(CS),T) :- type_cmds(G,CS,T). 

type_prog(G,prog(CS),T) :- type_block(G,CS,T).



type_def(G,const(id(X),T,E),Gn) :- type_expr(G,E,T) , concat((X,T),G,Gn) .
type_def(G,fun(id(X),T,Args,E),Gn) :- append(Args,G,Gn2), type_expr(Gn2,E,T) , args_to_type(Args,TS),append([(X,fleche(TS,T))],G,Gn).
type_def(G,funRec(id(X),T,Args,E),Gn) :- args_to_type(Args,TS), append([(X,fleche(TS,T))],G,Gn),append(Args,Gn,Gn2), type_expr(Gn2,E,T).
type_def(G,var(id(X),T),Gn) :- concat((X,T),G,Gn).
type_def(G,proc(id(X),Args,B),Gn) :- append(Args,G,Gn2), type_block(Gn2,B,void) , args_to_type(Args,TS),append([(X,fleche(TS,void))],G,Gn).
type_def(G,procRec(id(X),Args,B),Gn) :- args_to_type(Args,TS), append([(X,fleche(TS,void))],G,Gn),append(Args,Gn,Gn2), type_block(Gn2,B,void).
type_def(G,funSp(id(X),T,Args,B),Gn) :- append(Args,G,Gn2), type_block(Gn2,B,T) , args_to_type(Args,TS),append([(X,fleche(TS,T))],G,Gn).
type_def(G,funSpRec(id(X),T,Args,B),Gn) :- args_to_type(Args,TS), append([(X,fleche(TS,T))],G,Gn),append(Args,Gn,Gn2), type_block(Gn2,B,T).


test(Arg) :- type_prog([],Arg,_), write("true").
test() :-  write("false").
main :- read(user_input,Arg), test(Arg).
