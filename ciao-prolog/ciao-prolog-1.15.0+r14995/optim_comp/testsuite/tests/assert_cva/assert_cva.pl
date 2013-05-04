:- module(_, [], []).
:- use_module(engine(attributes)).

:- export(main/0).
main :-
	( test ->
	    true
	; display('assert_cva test failed!'), nl
	).

:- data d/1.

test :-
	attach_attribute(V, v(_)),
	type(V, attv),
	get_attribute(V, v(_)),
	assertz_fact(d(V)),
	current_fact(d(W)),
	type(W, attv),
	get_attribute(W, v(_)).



	