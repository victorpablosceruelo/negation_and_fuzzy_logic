:- module(hello_world, [main/0], [assertions]).

:- doc(title, "Hello World").

:- use_module(engine(io_basic)).

main :-
	display('Hello World!'), nl.

