/*
Checks that undogoals are executed in next_instance.

Correct output:

?- a.
3
ok2
ok1
ok
no
?- 
*/

:- module(_, _, []).

:- use_module(library(odd)).
:- data f/1.

a :-
	retractall_fact(f(_)),
	asserta_fact(f(1)),
	asserta_fact(f(2)),
	asserta_fact(f(3)),
	f(X),
	display(X), nl,
	undo(display(ok)),
	fail.
