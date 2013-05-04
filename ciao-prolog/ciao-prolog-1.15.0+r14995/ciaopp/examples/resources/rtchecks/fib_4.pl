:- module(_, [fib/2], [assertions, regtypes, nativeprops,
		ciaopp(examples(resources(rtchecks)))]).

:- doc(author, "Edison Mera").

:- doc(module, "This program calculates the N-th fibonacci number.").

:- entry fib(X, Y) : num * var.

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(M, N) :-
	M > 1,
	M1 is M-1,
	M2 is M-2,
	fib(M1, N1),
	fib(M2, N2),
	N is N1 + N2.
