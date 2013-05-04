:- module(_,[main/1],[assertions]).

:- pred main/1 + sideff(free).
%% :- pred main/1 + sideff(soft).
%% :- pred main/1 + sideff(hard).

seff(p/1,pure).
seff(q/2,hard).

main(X) :-
	p(X),
	display(X).

:- trust p(X) => int(X).

p(_).

