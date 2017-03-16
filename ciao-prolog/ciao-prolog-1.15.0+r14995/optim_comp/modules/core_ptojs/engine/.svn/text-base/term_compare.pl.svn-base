:- module(term_compare, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Comparing terms").
:- doc(author, "Jose F. Morales").

:- doc(bug, "[Incomplete version of engine(term_compare) for pl2js]").

:- export((==)/2).
A == B :-
	'$compare'(A, B, Value0), Value0 = 0.

:- export((\==)/2).
A \== B :- '$compare'(A, B, Value0), Value0 =\= 0.

:- export((@<)/2).
A @< B :- '$compare'(A, B, Value0), Value0 < 0.

:- export((@=<)/2).
A @=< B :- '$compare'(A, B, Value0), Value0 =< 0.

:- export((@>)/2).
A @> B :- '$compare'(A, B, Value0), Value0 > 0.

:- export((@>=)/2).
A @>= B :- '$compare'(A, B, Value0), Value0 >= 0.

:- export(compare/3).
compare(Value, X, Y) :-
	'$compare'(X, Y, Value0),
	( Value0 = -1 -> Value = (<)
	; Value0 = 1 -> Value = (>)
	; Value = (=)
	).

'$compare'(X, Y, Value) :-
	X.'$compare'(Y, Value).
