:- module(lex_test, [], [assertions]).

:- doc(title, "Test for lexical comparisons").

% TODO: implement 'rank', missing checks for many types (e.g., strings)

:- use_module(engine(io_basic)).

:- export(main/0).
main :-
	write_compare(1, 1,1),
	write_compare(2, 2,1),
	write_compare(3, 1,2),
	write_compare(4, f(2),f(1)),
	write_compare(5, f(1),f(2)),
	write_compare(4, f(2),g(1)),
	write_compare(5, g(1),f(2)),
	write_compare(6, f(1,2),f(1,1)),
	write_compare(7, f(1,1),f(1,2)),
	write_compare(8, f(2,1),f(1,1)),
	write_compare(9, f(1,1),f(2,1)),
	write_compare(10, X,Y),
	write_compare(11, f,f(Y)),
	write_compare(12, f(X),f),
	write_compare(13, f(X),f(Y)),
	write_compare(14, f(X,X),f(Y)),
	write_compare(15, f(X),f(Y,Y)),
	write_compare(16, f(X,X),f(Y,Y)),
	write_compare(17, X,1),
	write_compare(18, 1,X),
	write_compare(19, f(X),1),
	write_compare(20, 1,f(X)),
	write_compare(21, f(1,1),f(1,1)),
	write_compare(22, f(X,X),f(X,X)),
	write_compare(23, f(f(X)),f(f(X))).

write_compare(I, A, B) :-
	display(I),
	display(': compare '), display(A),
	display(' and '), display(B),
	compare(C, A, B),
	display(' => '), display(C), nl.

