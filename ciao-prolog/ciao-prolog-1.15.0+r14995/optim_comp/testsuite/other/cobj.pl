% test compiler object
:- module(_, [], [compiler(compiler_object)]).

:- all_instantiable.

:- export(main/0).
:- static(main/0).
main :-
	internal_new(This),
	This/cobj:foo,
	internal_free(This).

:- inst_data(a/1).
:- inst_var(x).

foo :-
	add(a(1)),
	add(a(2)),
	add(a(3)),
	get(a(X)),
	  display(a(X)), nl,
	  fail.
foo :-
	set(x, 3),
	set(x, 4),
	get(x, X),
	  display(X), nl,
	  fail.
foo.
