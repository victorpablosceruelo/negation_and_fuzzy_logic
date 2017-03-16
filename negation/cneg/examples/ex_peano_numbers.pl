:- module(ex_peano_numbers,_,[.(cneg)]).
%:- module(ex_peano_numbers,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([]).

test('test', even(X), 'should_succeed', odd(X), 'should_fail').
test('test', odd(X), 'should_succeed', even(X), 'should_fail').


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

no_even(X) :- cneg(even(X)).

odd(s(0)).
odd(s(s(X))) :- odd(X).

square(X) :- natural(Y), prod(Y, Y, X).

prod(0, _Y, 0).
prod(s(0), Y, Y).
prod(s(s(X)), Y, Z) :- prod(s(X), Y, W), add(W, Y, Z).

add(0, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).