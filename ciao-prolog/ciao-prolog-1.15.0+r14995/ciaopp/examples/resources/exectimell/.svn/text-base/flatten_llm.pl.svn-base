:- module(_, [flatten/2], [assertions, regtypes,
		ciaopp(examples(resources(exectimell)))]).

:- doc(author, "Edison Mera").

:- entry flatten/2 : gnd * var.

% :- resource res_steps.

% :- resource_delta( ub, res_steps, 1 ).
% :- resource_call( ub, res_steps, 0 ).

append([],    L,  L).
append([H|L], L1, [H|R]) :-
	append(L, L1, R).

flatten(X, [X]) :-
	atomic(X),
	X \== [], !.
flatten([],     []).
flatten([X|Xs], Ys) :-
	flatten(X,  Ys1),
	flatten(Xs, Ys2),
	append(Ys1, Ys2, Ys).
