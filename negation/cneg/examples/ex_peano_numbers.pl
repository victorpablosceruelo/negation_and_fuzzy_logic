:- module(ex_peano_numbers,_,[.(cneg)]).
%:- module(ex_peano_numbers,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/2, test_queens_1/2, test_queens_2/2]).
cneg_choosen_negation(cneg_rt_Chan).

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

no_even(X) :- cneg([], even(X)).

odd(s(0)).
odd(s(s(X))) :- odd(X).