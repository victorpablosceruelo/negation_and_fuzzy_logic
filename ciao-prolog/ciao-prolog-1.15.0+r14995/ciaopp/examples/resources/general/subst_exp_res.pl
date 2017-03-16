:- module(_, _, [assertions, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

delta_replacements(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Head),
	Head = 'subst_exp:find_replacement'(_, _, _),
	!.
delta_replacements(_LitInfo, 0) :- !.
