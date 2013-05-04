:- module(_, [f_gounif/2, term_gounif_size/3], [assertions]).

:- use_module(resources(reslitinfo)).
:- use_module(predefres(res_unif_res)).

f_gounif(LitInfo, Cost) :-
	litinfo_get_lit(LitInfo, Head),
	litinfo_get_mode(LitInfo, Mode),
	term_gounif_size(Head, Mode, Cost).

:- test term_gounif_size(Head, Modes, Count) :
	(
	    Head = list([_A|_As], [_B|_Bs]),
	    Modes = [-, +]
	) =>
	(
	    Count = 1
	).

term_gounif_size(Head, Mode, Size) :-
	term_unif_size_(Head, Mode, '-', 0, 1, 0, Size).
