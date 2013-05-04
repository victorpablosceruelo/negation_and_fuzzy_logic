:- module(_, _, [assertions, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

delta_qmovements(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Head),
	Head = 'eight_queen:generator'(_, _, _),
	!.
delta_qmovements(_LitInfo, 0).
