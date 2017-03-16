:- module(_,[slice2/1],[]).

interpret(cst(X),_,_,X).
interpret(var(X),Vars,Vals,R) :- lookup(X,Vars,Vals,R).
%% interpret(plus(X,Y),Vars,Vals,Res) :- interpret(X,Vars,Vals,RX), interpret(Y,Vars,Vals,RY), Res is RX+RY.
interpret(plus(X,Y),Vars,Vals,Res) :- interpret(X,Vars,Vals,RX), interpret(Y,Vars,Vals,RY), Res is RX-RY.
interpret(minus(X,Y),Vars,Vals,Res) :- interpret(X,Vars,Vals,RX), interpret(Y,Vars,Vals,RY), Res is RX-RY.
interpret(fun(X),Vars,Vals,Res) :- def0(X,Def), interpret(Def,Vars,Vals,Res).
interpret(fun(X,Arg),Vars,Vals,Res) :- 
   def1(X,Var,Def), interpret(Arg,Vars,Vals,ResArg),
   interpret(Def,[Var|Vars],[ResArg|Vals],Res).

def0(one,cst(1)).
def0(rec,fun(rec)).

def1(inc,xx,plus(var(xx),cst(1))).
def1(rec,xx,fun(rec,var(xx))).

lookup(X,[X|_],[Val|_],Val).
lookup(X,[Y|T],[_|ValT],Res) :-
   X \== Y, lookup(X,T,ValT,Res).

test(R) :- interpret(minus(plus(var(xx),cst(4)),var(zz)),[aa,bb,cc,dd,zz,yy,xx],[0,0,0,0,1,2,3],R).
test2(R) :- interpret(minus(plus(var(xx),cst(4)),fun(inc,var(zz))),[aa,bb,cc,dd,zz,yy,xx],[0,0,0,0,1,2,3],R).

slice2(R) :- interpret(plus(cst(1),cst(2)),*,*,R).


