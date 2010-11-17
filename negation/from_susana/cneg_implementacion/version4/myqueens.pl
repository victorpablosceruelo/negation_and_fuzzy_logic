:- module(myqueens, [queens/2]).

:- set_prolog_flag(multi_arity_warnings, off).

queens(N, Qs):-
        queens_list(N, Ns),
        queens(Ns, [], Qs).    % To place, placed, result

queens([], Qs, Qs).
queens(Unplaced, Placed, Qs):-
        select(Q, Unplaced, NewUnplaced),
        no_attack(Q, Placed),
        queens(NewUnplaced, [Q|Placed], Qs).

no_attack(Q, Safe):- no_attack(Safe, Q, s(0)).

no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
	add(Y,Nb,YMNb),
	Queen \== YMNb,
	add(Nb,YmNb,Y), % YmNb = Y - Nb
	Queen \== YmNb,
	add(Nb,s(0),Nb1),
        no_attack(Ys, Queen, Nb1).

select(X, [X|Ys], Ys).
select(X, [Y|Ys], [Y|Zs]):-
        select(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]):-
        greater_than(N,0),
	add(s(0),N1,N), % N1 = N - 1
        queens_list(N1, Ns).

num(0).
num(s(X)):-
	num(X).

add(0,X,X):- num(X).
add(s(X),Y,s(Z)):-
	add(X,Y,Z).

greater_than(s(X),0):-
	num(X).
greater_than(s(X),s(Y)):-
	greater_than(X,Y).


