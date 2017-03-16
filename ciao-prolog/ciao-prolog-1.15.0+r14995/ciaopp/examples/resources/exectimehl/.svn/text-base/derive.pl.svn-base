:- module(derive, [derive/3], [assertions, nativeprops, predefres(res_all)]).

:- entry derive/3 : arithexpression * ground * var.

derive(U + V, X, DU + DV) :- !, derive(U, X, DU), derive(V, X, DV).
derive(U - V, X, DU - DV) :- !, derive(U, X, DU), derive(V, X, DV).
derive(U * V, X, DU * V + U * DV) :- !, derive(U, X, DU), derive(V, X, DV).
derive(U / V, X, (DU * V - U * DV) / V ** 2) :- !, derive(U, X, DU),
	derive(V, X,
	    DV).
derive(U ** N, X, DU * N * U ** N1) :- !, integer(N), N1 is N-1,
	derive(U, X, DU).
derive(- U,     X,  - DU) :- !, derive(U, X, DU).
derive(exp(U),  X,  exp(U) * DU) :- !, derive(U, X, DU).
derive(log(U),  X,  DU / U) :- !, derive(U, X, DU).
derive(sin(U),  X,  cos(U) * DU) :- !, derive(U, X, DU).
derive(cos(U),  X,  - sin(U) * DU) :- !, derive(U, X, DU).
derive(atan(U), X,  DU/(1+ U ** 2)) :- !, derive(U, X, DU).
derive(X,       X,  1) :- !.
derive(_C,      _X, 0).
