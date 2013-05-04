:- module(_, [delta_output_elements/2], [assertions, regtypes]).

:- use_module(resources(resources_basic)).

delta_output_elements(LitInfo, 1) :-
	litinfo_get_lit(LitInfo, Head),
	(
	    Head = 'power_set:append_element'(_, _, _)
	;
	    \+ \+ (Head = 'power_set:powset'([], _)) % \+ \+ mandatory
	),
	!.
delta_output_elements(_, 0).
