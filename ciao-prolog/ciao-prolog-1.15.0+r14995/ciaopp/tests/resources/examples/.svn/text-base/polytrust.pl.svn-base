:- module(polytrust, [pred1/1, pred2/1], [assertions, resdefs, predefres(res_steps)]).

:- entry pred1(A) : int(A).
pred1(A) :-
	multitype(A).

:- entry pred2(A) : atm(A).
pred2(A) :-
	multitype(A).

:- trust pred multitype/1 : gnd + (not_fails, is_det).
:- trust pred multitype/1 : int + (cost(ub, steps, 3), cost(lb, steps, 2)).
:- trust pred multitype/1 : atm + (cost(ub, steps, 7), cost(lb, steps, 5)).

multitype(a).
multitype(1).

