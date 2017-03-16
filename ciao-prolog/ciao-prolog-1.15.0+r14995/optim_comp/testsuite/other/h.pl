:- module(_, _, []).

%:- use_module(library(debugger(debugger_support)),[srcdbg_spy/6]).
% :- '$default_preddef'(dynamic).
%:- multifile '$mod_srcdbg'/1.
%:- '$context'('$mod_srcdbg'/1, none).
%:- '$preddef'('$mod_srcdbg'/1, bytecode).
%:- '$insert_debug_info'.

main :-
	display(a), nl,
	X = 1,
	display(b), nl,
	b(X),
	display(b), nl,
	Y = 2,
	display(b), nl,
	\+ X == Y,
	display('mmmmmmm finish mmmmmmm'), nl.

b(_) :-	display(a), nl.
m(_) :- nl.

g(_).
