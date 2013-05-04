:- module(reverse, [reverse/2], [assertions, regtypes,
		ciaopp(tests(resources)),
		predefres(res_steps)]).

:- doc(author, "Edison Mera").

:- doc(module, "This tests reverse2/3, which have a cumulative
	parameter.  To allow the analysis can work, we ignore it in
	the size analysis by the usage of the void keywork in the
	@pred{size_metric/2} property.").


:- entry reverse(X, Y) : (var(Y), list(X, num)).

reverse(X, Y) :-
	reverse2(X, [], Y).

:- trust comp reverse2(A, B, C) + (size_metric(B, void)).

reverse2([],    Y,  Y).
reverse2([X|L], Y0, Y) :-
	reverse2(L, [X|Y0], Y).
