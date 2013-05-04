:- module(flat, [flat/2], [assertions, nativeprops, regtypes,
		ciaopp(tests(resources)),
		predefres(res_steps)]).

%
%  flat.pl			Nai-Wei Lin			November 1991
%
%  This program flattens a term into a list of atom.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- entry flat/2: gnd * var.

flat(X, [X]) :-
	atomic(X), !.
flat(X, [F|List]) :-
	functor(X, F, N),
	flat_(N, X, List).

:- trust comp flat_(X, Y, Z) + (cost(ub, steps, 1), cost(lb, steps, 1)).

:- trust pred arg/3 + not_fails # "Not fails only in this module.".

flat_(0, _, []).
flat_(N, X, List) :-
	N > 0,
	arg(N, X, Arg),
	flat(Arg, List1),
	N1 is N-1,
	flat_(N1, X, List2),
	append(List1, List2, List).


append([],    L,  L).
append([H|L], L1, [H|R]) :-
	append(L, L1, R).
