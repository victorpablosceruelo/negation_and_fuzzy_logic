:- module(_, _, [assertions, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

delta_queries(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Head),
	Head = 'query:density'(_, _),
	!.
delta_queries(_LitInfo, 0) :- !.
