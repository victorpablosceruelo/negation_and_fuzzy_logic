:- module(_, _, [assertions]).

:- '$context'(hello/0, ctx(fixed, [vn(msg, ro)])).
:- pred hello.

hello :-
	'$tvar_get'(msg, Msg),
	display(Msg), nl.

main :-
	'$tvar_i'(msg, 'Hello world!\n', hello).
