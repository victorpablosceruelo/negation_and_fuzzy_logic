:- module(exp,[exp/3],[]).

:- use_package(assertions).

:- entry exp(_,2,_).

exp(_,0,1).
exp(Base,Exp,Res):-
	Exp > 0,
	Exp1 is Exp - 1,
	exp(Base,Exp1,Res1),
	Res is Res1 * Base.
