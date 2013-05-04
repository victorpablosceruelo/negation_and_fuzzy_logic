:- module(queens_freeze, [], [benchmark]).

% (Example based on B Prolog 'queens')

:- use_module(library(lists)).
:- use_module(library(freeze)).

name("11-Queens Puzzle (using freeze)").
repeat_count(1).
solve(X) :-
	queens(11, X).

% ---------------------------------------------------------------------------

queens(N, List):-
	make_list(N,List),
	range(1,N,D),
	constrain_queens(List),
%    symetry(N, List),
	label(List,D).
%	display(d(List)), nl.

constrain_queens([]).
constrain_queens([X|Y]):-
	safe(X,Y,1),
	constrain_queens(Y).

safe(_,[],_) :- !.
safe(X,[Y|T],K):-
	freeze(X,freeze(Y,noattack(X,Y,K))),
	% JF: other version (which is slower)
%	freeze(X,S0=1),
%	freeze(Y,S1=S0),
%	freeze(S1,noattack(X,Y,K)),
	K1 is K+1,
	safe(X,T,K1).

noattack(X,Y,K):-
	X =\= Y,
	X+K =\= Y,
	X-K =\= Y.

make_list(0,[]):-!.
make_list(N,[_|Rest]):-
	N1 is N-1,
	make_list(N1,Rest).

range(N,N,[N]) :- !.
range(M,N,[M|Ns]) :-
	M < N,
	M1 is M+1,
	range(M1,N,Ns).

label([],_D) :- !.
label([V|Vs],D):-
	myselect(D,Rest,V),
	label(Vs,Rest).

myselect([X|Xs],Xs,X).
myselect([Y|Ys],[Y|Zs],X) :- myselect(Ys,Zs,X).



