:- module(add_poly, [add_polynomials/3], [assertions, regtypes,
		ciaopp(examples(resources(exectimehl)))]).

:- entry add_polynomials(X, Y, Z) : list(factor) * list(factor) * var.

:- regtype factor/1.

factor((A, N)) :- num(A), num(N).

add_polynomials([],               Poly,             Poly) :- !.
add_polynomials(Poly,             [],               Poly) :- !.
add_polynomials([(Ai, Ni)|Poly1], [(Aj, Nj)|Poly2], [(Ai, Ni)|Poly]) :-
	Ni > Nj, !,
	add_polynomials(Poly1, [(Aj, Nj)|Poly2], Poly).
add_polynomials([(Ai, Ni)|Poly1], [(Aj, Nj)|Poly2], [(A, Ni)|Poly]) :-
	Ni =:= Nj, !,
	A is Ai + Aj,
	add_polynomials(Poly1, Poly2, Poly).
add_polynomials([(Ai, Ni)|Poly1], [(Aj, Nj)|Poly2], [(Aj, Nj)|Poly]) :-
	Ni < Nj, !,
	add_polynomials([(Ai, Ni)|Poly1], Poly2, Poly).
