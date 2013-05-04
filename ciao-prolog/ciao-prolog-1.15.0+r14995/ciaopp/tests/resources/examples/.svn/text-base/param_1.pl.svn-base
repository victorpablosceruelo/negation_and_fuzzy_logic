:- module(_, [p/0], [assertions, ciaopp(tests(resources)), predefres(res_steps)]).

:- doc(author, "Edison Mera").

:- doc(module, "The cost of p must not be infinite (in fact is 8).
	If you like to see the behavior supposing that get_list/1 is
	implemented, uncomment get_list/1 implementation and comment
	out the trust and impl_defined declaration.").

:- entry p/0.

p :-
	get_list(L),
	q(L).

:- doc(bug, "Here we wrote 5 as size of the first argument,
	but... What happen if it is a global constant?.  We can have a
	parametric cost analysis!. -- EMM").

:- trust pred get_list(_1) : var(_1) => (list(_1), size(_1, 5))
	+ ( cost(ub, steps, 1), cost(lb, steps, 1),
	    is_det, not_fails, relations(inf) ).

:- impl_defined([get_list/1]).

% get_list([1, 2, 3, 4, 5]).

q([]).
q([_|L]) :-
	q(L).
