:- module(_, [fib/2], [assertions, regtypes,
		ciaopp(tests(resources)),
		predefres(res_arith),
		res_arith(res_arith_each)]).

:- doc(author, "Edison Mera").

:- doc(module, "This program calculate the N-th fibonacci number.").

:- doc(summary, "This consider arithmetics operator by operator,
	which is slower.").

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
