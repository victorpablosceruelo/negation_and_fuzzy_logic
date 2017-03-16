:- module(_, [], []).

:- if('$with_compiler_version'(55)). % TODO: put here the last compiler version
compiler(new).
:- else.
compiler(old).
:- endif.

:- export(main/0).
main :-
	% Only one line should be displayed
	( compiler(V),
	  display(V), nl,
	  fail
	; true
	).
