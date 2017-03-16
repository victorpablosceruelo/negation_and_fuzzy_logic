:- module(_, [main/1], [assertions,
		ciaopp(tests(resources)),
		predefres(res_steps)]).

:- doc(author, "Edison Mera").

:- doc(module, "Of course, the trust assertion is wrong.  What we
	are testing here is the warning messages that appears when
	analyzing this program.  Also the analyzer must terminate.").

:- entry main/1 : var.

main(A) :- test1(A).

:- trust pred test1(A) + size_metric(A, length(A)).

test1(A) :-
	A = [1, 2, 3, 4].
