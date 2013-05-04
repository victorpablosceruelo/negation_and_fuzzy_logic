:- module(_, [], []).

:- use_module(library(strings)).

:- '$pragma'(analyze_all).

:- export(main/1).
main([a]) :- le(_, 4000000).
main([b]) :- ple(_, 4000000).

:- '$props'(le/2, [impnat = ptoc]).
le(L, N) :-
	J = 0,
	dl(J, N, L).

:- '$props'(dl/3, [impnat = ptoc]).
dl(I, I, []) :- !.
dl(I0, I, [_|L]) :-
	I1 is I0+1,
	dl(I1, I, L).

ple(L, N) :- pdl(0, N, L).

pdl(I, I, []) :- !.
pdl(I0, I, [_|L]) :-
	I1 is I0+1,
	pdl(I1, I, L).

:- export(ap/3).
:- '$props'(ap/3, [impnat = ptoc]).
ap([],X,X).
ap([X|Xs],Y,[X|Zs]) :- ap(Xs, Y, Zs).

:- export(ag/3).
:- '$props'(ag/3, [impnat = ptoc]).
ag([],X,X).
ag(c(X,Xs),Y,c(X,Zs)) :- ag(Xs, Y, Zs).

:- export(gb/1).
:- '$props'(gb/1, [impnat = ptoc]).
gb(X) :-
	Y = _,
	gb2(Y),
	X = Y.

:- '$props'(gb2/1, [impnat = ptoc]).
gb2(_).

:- export(gg/0).
:- '$props'(gg/0, [impnat = ptoc]).
gg :-
	Y = _,
	h0,
	h(Y).

:- import(foo, [h0/0]).
:- import(foo, [h/1]).

