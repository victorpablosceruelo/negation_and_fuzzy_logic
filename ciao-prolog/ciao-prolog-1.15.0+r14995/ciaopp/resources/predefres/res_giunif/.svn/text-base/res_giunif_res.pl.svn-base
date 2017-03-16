:- module(_, [f_giunif/2, term_giunif_size/3], [assertions]).

:- use_module(resources(reslitinfo)).
:- use_module(predefres(res_unif_res)).

f_giunif(LitInfo, Cost) :-
	litinfo_get_lit(LitInfo, Head),
	litinfo_get_mode(LitInfo, Mode),
	term_giunif_size(Head, Mode, Cost).

:- test term_giunif_size(Head, Modes, Count) :
	(
	    Head = p(a, b(_A, _B), _C),
	    Modes = [+, +, +]
	) =>
	(
	    Count = 2
	).

:- test term_giunif_size(Head, Modes, Count) :
	(
	    Head = list([_A|_As], [_B|_Bs]),
	    Modes = [+, +]
	) =>
	(
	    Count = 2
	).

term_giunif_size(Head, Mode, Size) :-
	term_unif_size_(Head, Mode, '+', 0, 1, 0, Size).
