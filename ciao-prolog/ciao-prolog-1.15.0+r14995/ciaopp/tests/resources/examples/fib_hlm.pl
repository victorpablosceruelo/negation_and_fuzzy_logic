:- module(fib_hlm, [fib/2], [assertions, regtypes,
		ciaopp(tests(resources))]).

:- doc(author, "Edison Mera").

:- doc(module, "This program calculate the N-th fibonacci number.").

:- doc(summary, "This consider arithmetics operator by operator,
	which is slower.").

:- include(res_arith(res_arith_assr)).
:- include(res_exectime_hlm(auto(res_exectime_hlm_63_assr))).
:- include(resources(saved_results(platform))).

:- resource exectime_model4.
:- compound_resource(exectime_model4, [arith, exectime_hlm_63]).
:- trust_default + cost(ub, exectime_model4, 0).
:- trust_default + cost(lb, exectime_model4, 0).

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
