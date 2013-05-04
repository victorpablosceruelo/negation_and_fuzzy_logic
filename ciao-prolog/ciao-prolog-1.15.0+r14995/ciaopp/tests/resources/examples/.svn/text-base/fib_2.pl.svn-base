:- module(fib_2, [fib/2], [assertions, regtypes,
		ciaopp(tests(resources)),
		res_arith(res_arith)]).

:- doc(author, "Edison Mera").

:- doc(module, "This program calculate the N-th fibonacci number.").

:- doc(summary, "This consider arithmetic operators in one-step
	which is faster.").

%:- use_module(engine(arithmetic),[is/2]).

:- doc(author, "Edison Mera").

:- entry fib(X, Y) : num * var.

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(M, N) :-
	M1 is M -1,
	M2 is M -2,
	fib(M1, N1),
	fib(M2, N2),
	N is N1 + N2.
