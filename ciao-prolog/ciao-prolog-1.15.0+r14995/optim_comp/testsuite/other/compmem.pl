% Test if memory consumption is stable in the compiler
:- module(_, [main/0], []).

:- use_module(compiler(dynamic)).

main :-
	use_module(foo),
	...
..
..
imcomplete, see todo.txt
