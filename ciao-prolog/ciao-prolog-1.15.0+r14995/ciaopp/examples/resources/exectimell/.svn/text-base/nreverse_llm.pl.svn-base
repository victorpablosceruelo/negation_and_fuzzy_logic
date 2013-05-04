:- module(_, [nreverse/2], [assertions, regtypes,
		ciaopp(examples(resources(exectimell)))]).

:- entry nreverse(Xs, Ys) : (var(Ys), list(Xs, num)).

append([],     X, X).
append([X|Xs], Y, [X|Zs]) :-
	append(Xs, Y, Zs).

nreverse([],     []).
nreverse([X|Xs], Y) :-
	nreverse(Xs, Y0),
	append(Y0, [X], Y).
