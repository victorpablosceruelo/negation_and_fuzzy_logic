:- module(count, [count/1, mytest/0], [assertions, regtypes,
		ciaopp(tests(resources)),
		predefres(res_steps)]).

:- doc(author, "Edison Mera").

:- doc(module, "This tests reverse counting and a call to
	@pred{count0/2} predicate with the input argument instantiated
	in mytest/0.").

:- entry count/1 : {num}.

:- trust comp count0(A, B) + size_metric(B, void).

count(N) :-
	count0(N, 0).

count0(N, M) :-
	N>0,
	!,
	N1 is N - 1,
	M1 is M + 1,
	count0(N1, M1).
count0(_, _).

:- entry mytest/0.

mytest :-
	count0(5, 0).
