:- module(_, [perm/2],
	    [
		assertions,
		regtypes,
		nativeprops,
		ciaopp(tests(resources)),
		predefres(sta_exectime),
		predefres(res_wamcount)
	    ]).

%
%  perm.pl			Nai-Wei Lin			November 1991
%
%  This program generates a permutation of a list.
%

:- entry perm/2 : list(gnd) * var.
perm([],     []).
perm([X|Xs], [R|Rs]) :-
	select(R, [X|Xs], Y),
	perm(Y, Rs).

select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :-
	select(X, Ys, Zs).
