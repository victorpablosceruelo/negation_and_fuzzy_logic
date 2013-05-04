:- module(_, [perm/2],
	    [
		assertions,
		regtypes,
		nativeprops,
		predefres(res_wamcode)
	    ]).

%
%  perm.pl			Nai-Wei Lin			November 1991
%
%  This program generates a permutation of a list.
%

:- entry perm/2 : list(gnd) * var.
perm([],     []) :- display('+4').
perm([X|Xs], [R|Rs]) :-
	display('+26'),
	select(R, [X|Xs], Y),
	display('+4'),
	perm(Y, Rs),
	display('+1').

select(X, [X|Xs], Xs) :- display('+13').
select(X, [Y|Ys], [Y|Zs]) :- display('+22'),
	select(X, Ys, Zs),
	display('+1').
