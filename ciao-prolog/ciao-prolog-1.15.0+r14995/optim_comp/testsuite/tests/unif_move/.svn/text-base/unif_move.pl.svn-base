:- module(_, [], []).

:- export(main/0).
main :-
	putx2(10,10,10,10), % sets X(2) to 10
	( bar(_, _) -> /*display('unif_move is ok'), nl*/ true ; display('unif_move bug is present'), nl ).

putx2(_,_,_,_).

foo(_).

% bug: it may read X(2) before it is written
bar(A0, B) :-
	A = A0,
	var(A),
	foo(x(A,B)),
	foo(A0).

