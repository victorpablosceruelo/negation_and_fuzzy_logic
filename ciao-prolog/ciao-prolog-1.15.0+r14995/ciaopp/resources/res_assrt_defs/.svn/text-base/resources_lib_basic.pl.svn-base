:- module(resources_lib_basic, [get_default/2], []).

get_default((A, B), C) :-
	!,
	(
	    get_default(A, C)
	;
	    get_default(B, C)
	).
get_default(A, A).
