:- module(_, [f_viunif/2, term_viunif_size/3], [assertions]).

:- use_module(resources(reslitinfo)).
:- use_module(predefres(res_unif_res)).

f_viunif(LitInfo, Cost) :-
	litinfo_get_lit(LitInfo, Head),
	litinfo_get_mode(LitInfo, Mode),
	term_viunif_size(Head, Mode, Cost).

:- test term_viunif_size(Head, Mode, Count) :
	(
	    Head = list_deep_ground_input([
		    a(b(c(d(e(f(g(h(i(j(
					     a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(B
						)
						))))))))))
						))))))))))
						))))))))))
						))))))))))
					   )))))))))|_As], [B|_Bs]
	    ),
	    Mode = [+, -]
	) =>
	(Count = 2) # "Long input".

:- test term_viunif_size(Head, Mode, Count) :
	(
	    Head = p(a, b(_A, _B), _C),
	    Mode = [+, +, +]
	) =>
	(
	    Count = 3
	).

% +:
term_viunif_size(Head, Mode, Size) :-
	term_unif_size_(Head, Mode, '+', 1, 0, 0, Size).
