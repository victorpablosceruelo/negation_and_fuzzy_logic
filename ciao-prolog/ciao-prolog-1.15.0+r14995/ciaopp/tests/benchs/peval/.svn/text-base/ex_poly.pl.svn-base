:- module(_,[main/1],[]).


main(B):-
	C is B + 1,
	q(C).



q(1).
q(2).
q(3).

mylist([]).
mylist([_|X]):-
	mylist(X).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).

exp(Base,Exp,Res):-
	exp_ac(Exp,Base,1,Res).
	
exp_ac(0,_,Res,Res).
exp_ac(Exp,Base,Tmp,Res):-
	Exp > 0,
	Exp1 is Exp - 1,
	NTmp is Tmp * Base,
	exp_ac(Exp1,Base,NTmp,Res).

