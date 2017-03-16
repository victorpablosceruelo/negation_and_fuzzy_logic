:- module(_,[p/1,p_s/1],[assertions]).

p(X):-
	ground(X),
	!,
	X > 0.
p(_).

:- trust comp p([X]) : ground(X) + equiv(p_s(X)).
:- trust success p(X) : ground(X) => ground(X).


p_s(X):- X > 0.
