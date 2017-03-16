:- module(_, [delta_number_of_asserts/2],
	    [assertions, nativeprops, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

delta_number_of_asserts(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Head),
	Head = 'mfib:assert'(_),
	!.
delta_number_of_asserts(_LitInfo, 0) :- !.
