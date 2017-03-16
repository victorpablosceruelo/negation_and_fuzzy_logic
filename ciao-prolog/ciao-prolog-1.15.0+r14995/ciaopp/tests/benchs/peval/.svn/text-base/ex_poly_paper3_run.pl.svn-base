:- module(_,[run/1,main/3],[]).

:- include(numbers).

main(A,B,C):-
	exp(B,2,C),
	p(A).


p(B):-
	C is B + 1,
	q(C).

q(1).
q(2).
q(3).
q(4).
q(5).
q(6).


exp(Base,Exp,Res):-
	exp_ac(Exp,Base,1,Res).
	
exp_ac(0,_,Res,Res).
exp_ac(Exp,Base,Tmp,Res):-
	Exp > 0,
	Exp1 is Exp - 1,
	NTmp is Tmp * Base,
	exp_ac(Exp1,Base,NTmp,Res).

