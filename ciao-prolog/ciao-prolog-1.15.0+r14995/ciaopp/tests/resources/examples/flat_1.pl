:- module(flat_1, [flat/2], [assertions, regtypes, resdefs]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This program flattens a term into a list of atom.").

:- resource res_steps.

:- entry flat/2 : gnd * var.

:- trust comp flat(X, Y) + ( head_cost(ub, res_steps, 1),
	    head_cost(lb, res_steps, 1) ).

flat(X, [X]) :-
	atomic(X), !.
flat(X, [F|List]) :-
	functor(X, F, N),
	flat_(N, X, List).

:- trust comp flat_(X, Y, Z) + ( head_cost(ub, res_steps, 1),
	    head_cost(lb, res_steps, 1) ).

flat_(0, _, []).
flat_(N, X, List) :-
	N > 0,
	arg(N, X, Arg),
	flat(Arg, List1),
	N1 is N -1,
	flat_(N1, X, List2),
	append(List1, List2, List).

:- trust comp append(X, Y, Z) + ( head_cost(ub, res_steps, 1),
	    head_cost(lb, res_steps, 1) ).

append([],    L,  L).
append([H|L], L1, [H|R]) :-
	append(L, L1, R).
