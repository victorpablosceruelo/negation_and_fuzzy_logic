%   poly_10
%
%   Ralph Haygood (based on Prolog version by Rick McGeer
%                  based on Lisp version by R. P. Gabriel)
%
%   raise a polynomial (1+x+y+z) to the 10th power (symbolically)

#include "../mtsys_common.pl"

#if defined(MERCURY)

:- type benchmark_data == poly.
:- type benchmark_result == poly.

:- pred dummy_result(poly).
:- mode dummy_result(out) is det.
dummy_result(const(1)).

:- pred benchmark_data(string, int, poly).
:- mode benchmark_data(out, out, out) is det.
benchmark_data("poly", 100, Data) :-
	test_poly(Data).

:- pred benchmark(int, poly, poly).
:- mode benchmark(in, in, out) is det.
benchmark(_N, P, Out) :-
        poly_exp(10, P, Out).

:- type var ---> x ; y ; z.
:- type pterm ---> term(int, poly).
:- type poly ---> poly(var, list(pterm)) ; const(int).

:- pred test_poly1(poly).
:- mode test_poly1(out) is det.

:- pred test_poly2(poly).
:- mode test_poly2(out) is det.

:- pred test_poly3(poly).
:- mode test_poly3(out) is det.

:- pred test_poly(poly).
:- mode test_poly(out) is det.

:- pred poly_add(poly, poly, poly).
:- mode poly_add(in, in, out) is det.

:- pred term_add(list(pterm), list(pterm), list(pterm)).
:- mode term_add(in, in, out) is det.

:- pred add_to_order_zero_term(list(pterm), poly, list(pterm)).
:- mode add_to_order_zero_term(in, in, out) is det.

:- pred poly_exp(int, poly, poly).
:- mode poly_exp(in, in, out) is det.

:- pred poly_mul(poly, poly, poly).
:- mode poly_mul(in, in, out) is det.

:- pred term_mul(list(pterm), list(pterm), list(pterm)).
:- mode term_mul(in, in, out) is det.

:- pred single_term_mul(list(pterm), pterm, list(pterm)).
:- mode single_term_mul(in, in, out) is det.

:- pred mul_through(list(pterm), poly, list(pterm)).
:- mode mul_through(in, in, out) is det.

:- pred lt(var, var).
:- mode lt(in, in) is semidet.

:- pred myeven(int).
:- mode myeven(in) is semidet.

:- pred print_poly(poly, io__state, io__state).
:- mode print_poly(in, di, uo) is det.

:- pred print_var(var, io__state, io__state).
:- mode print_var(in, di, uo) is det.

:- pred print_terms(list(pterm), io__state, io__state).
:- mode print_terms(in, di, uo) is det.

:- pred print_terms_2(list(pterm), io__state, io__state).
:- mode print_terms_2(in, di, uo) is det.

:- pred print_term(pterm, io__state, io__state).
:- mode print_term(in, di, uo) is det.

print_poly(const(N)) -->
        io__write_string("const("),
        io__write_int(N),
        io__write_string(")").
print_poly(poly(Var, Terms)) -->
        io__write_string("poly("),
        print_var(Var),
        io__write_string(", "),
        print_terms(Terms),
        io__write_string(")").

print_var(x) -->
        io__write_string("x").
print_var(y) -->
        io__write_string("y").
print_var(z) -->
        io__write_string("z").

print_terms(Terms) -->
        ( { Terms = [] } ->
                io__write_string("[]\n")
        ;
                io__write_string("["),
                print_terms_2(Terms),
                io__write_string("]")
        ).

print_terms_2([]) --> [].
print_terms_2([Term|Terms]) -->
        print_term(Term),
        ( { Terms = [] } ->
                []
        ;
                io__write_string(", "),
                print_terms_2(Terms)
        ).

print_term(term(N, Poly)) -->
        io__write_string("term("),
        io__write_int(N),
        io__write_string(", "),
        print_poly(Poly),
        io__write_string(")").
test_poly1(P) :-
        P = poly(x, [term(0,const(1)), term(1,const(1))]).

test_poly2(P) :-
        P = poly(y, [term(1,const(1))]).

test_poly3(P) :-
        P = poly(z, [term(1,const(1))]).

test_poly(P) :-
        poly_add(poly(x, [term(0,const(1)), term(1,const(1))]), poly(y, [term(1, const(1))]), Q),
        poly_add(poly(z, [term(1,const(1))]), Q, P).



poly_add(poly(Var1, Terms1), poly(Var2, Terms2), Result) :-
        ( Var1 = Var2 ->
                term_add(Terms1, Terms2, Terms),
                Result = poly(Var1, Terms)
        ; lt(Var1, Var2) ->
                add_to_order_zero_term(Terms1, poly(Var2, Terms2), Terms),
                Result = poly(Var1, Terms)
        ;
                add_to_order_zero_term(Terms2, poly(Var1, Terms1), Terms),
                Result = poly(Var2, Terms)
        ).
poly_add(poly(Var1, Terms1), const(C2), poly(Var1, Terms)) :-
        add_to_order_zero_term(Terms1, const(C2), Terms).
poly_add(const(C1), poly(Var2, Terms2), poly(Var2, Terms)) :-
        add_to_order_zero_term(Terms2, const(C1), Terms).
poly_add(const(C1), const(C2), const(C)) :-
        C is C1 + C2.
term_add([], List2, List2).
term_add([term(E1,C1)|Terms1], [], [term(E1,C1)|Terms1]).
term_add([term(E1,C1)|Terms1], [term(E2,C2)|Terms2], Result) :-
        ( E1 = E2 ->
                poly_add(C1, C2, C),
                term_add(Terms1, Terms2, Terms),
                Result = [term(E1,C)|Terms]
        ; E1 < E2 ->
                term_add(Terms1, [term(E2,C2)|Terms2], Terms),
                Result = [term(E1,C1)|Terms]
        ;
                term_add([term(E1,C1)|Terms1], Terms2, Terms),
                Result = [term(E2,C2)|Terms]
        ).
add_to_order_zero_term(List, C2, Result) :-
        ( List = [term(0,C1)|Terms] ->
                poly_add(C1, C2, C),
                Result = [term(0,C)|Terms]
        ;
                Result = [term(0,C2)|List]
        ).
poly_exp(N, Poly, Result) :-	
        ( N = 0 ->
                Result = const(1)
        ; myeven(N) ->
	        M is N >> 1,
                poly_exp(M, Poly, Part),
                poly_mul(Part, Part, Result)
        ;
                M is N - 1,
                poly_exp(M, Poly, Part),
                poly_mul(Poly, Part, Result)
        ).
poly_mul(poly(Var1, Terms1), poly(Var2, Terms2), Result) :-
        ( Var1 = Var2 ->
                term_mul(Terms1, Terms2, Terms),
                Result = poly(Var1, Terms)
        ; lt(Var1, Var2) ->
                mul_through(Terms1, poly(Var2, Terms2), Terms),
                Result = poly(Var1, Terms)
        ;
                mul_through(Terms2, poly(Var1, Terms1), Terms),
                Result = poly(Var2, Terms)
        ).
poly_mul(poly(Var1, Terms1), const(C2), poly(Var1, Terms)) :-
        mul_through(Terms1, const(C2), Terms).
poly_mul(const(C1), poly(Var2, Terms2), poly(Var2, Terms)) :-
        mul_through(Terms2, const(C1), Terms).
poly_mul(const(C1), const(C2), const(C)) :-
        C is C1 * C2.
term_mul([], _List2, []).
term_mul([_Term1|_Terms1], [], []).
term_mul([Term1|Terms1], [Term2|Terms2], Result) :-
        single_term_mul([Term2|Terms2], Term1, PartA),
        term_mul(Terms1, [Term2|Terms2], PartB),
        term_add(PartA, PartB, Result).
single_term_mul([], _Term, []).
single_term_mul([term(E1,C1)|Terms1], term(E2,C2), [term(E,C)|Terms]) :-
        E is E1 + E2,
        poly_mul(C1, C2, C),
        single_term_mul(Terms1, term(E2,C2), Terms).
mul_through([], _Poly, []).
mul_through([term(E,Term)|Terms], Poly, [term(E,NewTerm)|NewTerms]) :-
        poly_mul(Term, Poly, NewTerm),
        mul_through(Terms, Poly, NewTerms).
lt(x, y).
lt(y, z).
lt(x, z).

myeven(N) :-
        M is N,
        N1 is M * 2,
        N = N1.

#else /* NO MERCURY */

benchmark_data(poly, 100, Data) :-
	test_poly(Data).

benchmark(Data, Out) :-
	poly_exp(10, Data, Out).

test_poly1(P) :-
	P = poly(x, [term(0,1), term(1,1)]).

test_poly2(P) :-
	P = poly(y, [term(1,1)]).

test_poly3(P) :-
	P = poly(z, [term(1,1)]).

test_poly(P) :-
	poly_add(poly(x, [term(0,1), term(1,1)]), poly(y, [term(1, 1)]), Q),
	poly_add(poly(z, [term(1,1)]), Q, P).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(poly_add/3, sht, [nonvar, nonvar, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(poly_add/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(poly_add/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(poly_add/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(poly_add/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(poly_add/3, [should_trim_frame=no]).
#endif
poly_add(poly(Var,Terms1), poly(Var,Terms2), poly(Var,Terms)) :- !,
	term_add(Terms1, Terms2, Terms).
poly_add(poly(Var1,Terms1), poly(Var2,Terms2), poly(Var1,Terms)) :-
	Var1 @< Var2, !,
	add_to_order_zero_term(Terms1, poly(Var2,Terms2), Terms).
poly_add(Poly, poly(Var,Terms2), poly(Var,Terms)) :- !,
	add_to_order_zero_term(Terms2, Poly, Terms).
poly_add(poly(Var,Terms1), C, poly(Var,Terms)) :- !,
	add_to_order_zero_term(Terms1, C, Terms).
poly_add(C1, C2, C) :-
#if (OPT_MASK & OPT_TYPES)
	'$trust_type'(C1, smallint),
	'$trust_type'(C2, smallint),
#endif
	C is C1 + C2.

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(term_add/3, sht, [nonvar, nonvar, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(term_add/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(term_add/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(term_add/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(term_add/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(term_add/3, [should_trim_frame=no]).
#endif
term_add([], X, X) :- !.
term_add(X, [], X) :- !.
term_add([term(E,C1)|Terms1], [term(E,C2)|Terms2], [term(E,C)|Terms]) :- !,
	poly_add(C1, C2, C),
	term_add(Terms1, Terms2, Terms).
term_add([term(E1,C1)|Terms1], [term(E2,C2)|Terms2], [term(E1,C1)|Terms]) :-
#if OPTIMIZED
	'$trust_type'(E1, smallint),
	'$trust_type'(E2, smallint),
#endif
	E1 < E2, !,
	term_add(Terms1, [term(E2,C2)|Terms2], Terms).
term_add(Terms1, [term(E2,C2)|Terms2], [term(E2,C2)|Terms]) :-
	term_add(Terms1, Terms2, Terms).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(add_to_order_zero_term/3, sht, [nonvar, nonvar, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(add_to_order_zero_term/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(add_to_order_zero_term/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(add_to_order_zero_term/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(add_to_order_zero_term/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(add_to_order_zero_term/3, [should_trim_frame=no]).
#endif
add_to_order_zero_term([term(0,C1)|Terms], C2, [term(0,C)|Terms]) :- !,
	poly_add(C1, C2, C).
add_to_order_zero_term(Terms, C, [term(0,C)|Terms]).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(poly_exp/3, sht, [int, nonvar, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(poly_exp/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(poly_exp/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(poly_exp/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(poly_exp/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(poly_exp/3, [should_trim_frame=no]).
#endif
poly_exp(0, _, 1) :- !.
poly_exp(N, Poly, Result) :-
	N/\1 =:= 0, !,
	M is N>>1,
	poly_exp(M, Poly, Part),
	poly_mul(Part, Part, Result).
poly_exp(N, Poly, Result) :-
	M is N - 1,
	poly_exp(M, Poly, Part),
	poly_mul(Poly, Part, Result).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(poly_mul/3, sht, [nonvar, nonvar, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(poly_mul/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(poly_mul/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(poly_mul/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(poly_mul/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(poly_mul/3, [should_trim_frame=no]).
#endif
poly_mul(poly(Var,Terms1), poly(Var,Terms2), poly(Var,Terms)) :- !,
	term_mul(Terms1, Terms2, Terms).
poly_mul(poly(Var1,Terms1), poly(Var2,Terms2), poly(Var1,Terms)) :-
	Var1 @< Var2, !,
	mul_through(Terms1, poly(Var2,Terms2), Terms).
poly_mul(P, poly(Var,Terms2), poly(Var,Terms)) :- !,
	mul_through(Terms2, P, Terms).
poly_mul(poly(Var,Terms1), C, poly(Var,Terms)) :- !,
	mul_through(Terms1, C, Terms).
poly_mul(C1, C2, C) :-
#if OPTIMIZED
	'$trust_type'(C1, smallint),
	'$trust_type'(C2, smallint),
#endif
	C is C1 * C2.

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(term_mul/3, sht, [nonvar, nonvar, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(term_mul/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(term_mul/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(term_mul/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(term_mul/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(term_mul/3, [should_trim_frame=no]).
#endif
term_mul([], _, []) :- !.
term_mul(_, [], []) :- !.
term_mul([Term|Terms1], Terms2, Terms) :-
	single_term_mul(Terms2, Term, PartA),
	term_mul(Terms1, Terms2, PartB),
	term_add(PartA, PartB, Terms).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(single_term_mul/3, sht, [nonvar, str(term/2), var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(single_term_mul/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(single_term_mul/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(single_term_mul/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(single_term_mul/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(single_term_mul/3, [should_trim_frame=no]).
#endif
single_term_mul([], _, []) :- !.
single_term_mul([term(E1,C1)|Terms1], term(E2,C2), [term(E,C)|Terms]) :-
#if (OPT_MASK & OPT_TYPES)
	'$trust_type'(E1, smallint),
	'$trust_type'(E2, smallint),
#endif
	E is E1 + E2,
	poly_mul(C1, C2, C),
	single_term_mul(Terms1, term(E2,C2), Terms).

#if (OPT_MASK & OPT_TYPES)
% :- '$trust_entry'(mul_through/3, sht, [nonvar, nonvar, var]).
#endif
#if (OPT_MASK & OPT_ARGMODES)
:- '$props'(mul_through/3, [argmodes=[in,in,out]]).
#endif
#if (OPT_MASK & OPT_ARGMEMS)
% :- '$props'(mul_through/3, [argmems=[cvar,cvar,cvar]]).
#endif
#if (OPT_MASK & OPT_ARGDEREFS)
% :- '$props'(mul_through/3, [argderefs=[true,true,true]]).
#endif
#if (OPT_MASK & OPT_IMP)
% :- '$props'(mul_through/3, [imp=semidet]).
#endif
#if (OPT_MASK & OPT_TRIM)
:- '$props'(mul_through/3, [should_trim_frame=no]).
#endif
mul_through([], _, []) :- !.
mul_through([term(E,Term)|Terms], Poly, [term(E,NewTerm)|NewTerms]) :-
	poly_mul(Term, Poly, NewTerm),
	mul_through(Terms, Poly, NewTerms).

#endif /* NO MERCURY */
