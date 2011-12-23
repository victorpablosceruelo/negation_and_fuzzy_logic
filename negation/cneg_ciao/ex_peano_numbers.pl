:- module(ex_peano_numbers,_,[.(cneg), .(debugger_pkg)]).

%peano(0,0).
%peano(N,s(P1)):-
%	N > 0,
%	N1 is N-1,
%	peano(N1,P1).

natural(0).
natural(s(X)) :- natural(X).

positive(s(0)). 
positive(s(X)) :- positive(X).  

less(0,s(_X)).
less(s(X),s(Y)) :- less(X,Y).

even(0).
even(s(s(X))) :- even(X).

no_even(X) :- cneg_tr([], even(X)).

odd(s(0)).
odd(s(s(X))) :- odd(X).