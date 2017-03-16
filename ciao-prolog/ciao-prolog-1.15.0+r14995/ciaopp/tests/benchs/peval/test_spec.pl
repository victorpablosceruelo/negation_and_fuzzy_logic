:- module(_,[main/1,mylist/1],[assertions,regtypes,functions]).

:- use_module(test_modular_spec).

:- entry main(X) : list(X).

main(L):-
	W = [1,2|L],
	integer(2),
	Num = 2,
	integer(Num),
	mylist(W),
 	mylist([1,2|W]).
%% 	mylist(W).

main(_).

:- regtype mylist/1.

mylist := [] | [_|~mylist].
