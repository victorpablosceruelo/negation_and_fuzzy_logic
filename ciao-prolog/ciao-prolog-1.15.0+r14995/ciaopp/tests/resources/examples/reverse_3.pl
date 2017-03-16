:- module(reverse_3, [reverse/2], [assertions, regtypes, nativeprops,
		ciaopp(tests(resources)),
		predefres(res_steps)]).

:- use_module(library(lists), [append/3]).

:- doc(author, "Edison Mera").

 :- doc(module, "This program shows an example of an assertion
	about a predicate, @pred{append/3}, that is defined in other
	module. Note that there is not lower bound assertion for
	append/3, just to test the warning message").

:- entry reverse(X, Y) : (var(Y), list(X, num)).

:- resource steps_2.
:- head_cost(ub, steps_2, 2).
:- head_cost(lb, steps_2, 2).
:- literal_cost(ub, steps_2, 0).
:- literal_cost(lb, steps_2, 0).
:- trust_default + cost(ub, steps_2, 0).
:- trust_default + cost(lb, steps_2, 0).


reverse([],    []).
reverse([X|T], L) :- reverse(T, L1), append(L1, [X], L).

:- trust pred append(X, Y, Z)
	: (list(X), list(Y), var(Z))
	=> ( list(X), list(Y), list(Z),
	    size(Z, length(Y) + length(X))
	) +
	(
	    cost(ub, steps_2, 2 * length(X) + 2),
	    cost(ub, steps, length(X) + 1),
	    cost(lb, steps, length(X) + 1),
	    not_fails,
	    num_solutions(1),
	    relations(inf)
	).
