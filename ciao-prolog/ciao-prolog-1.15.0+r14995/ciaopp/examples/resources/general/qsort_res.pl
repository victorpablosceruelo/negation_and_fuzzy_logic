:- module(_, _, [assertions, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

delta_lists_parallelized(LitInfo, 2) :-
	litinfo_get_lit(LitInfo, Head),
	Head = 'qsort:partition'(_, _, _, _),
	!.
delta_lists_parallelized(_LitInfo, 0) :- !.
