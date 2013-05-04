:- module(res_arith_res, [f_arith/2], [assertions]).

:- use_module(library(hiordlib)).
:- use_module(resources(resources_basic)).
:- use_module(resources(resources_db)).
:- use_module(resources(res_assrt_defs(infertime_lib))).
:- use_module(res_arith(res_arith_each_res),
	    [valid_arith_builtin/1, resource_arith/2]).

f_arith(LitInfo, Cost) :-
	litinfo_get_lit(LitInfo, Lit),
	litinfo_get_bt(LitInfo, BT),
	valid_arith_builtin(Lit),
	get_platform(BT, Platform),
	cost_arith(Lit, Platform, Cost),
	!.
f_arith(_, 0).

cost_arith(Lit, Platform, Cost) :-
	cleanup_arith_costs_db,
	assert_arith_costs(Platform),
	count_arith(Lit, 0, Cost),
	cleanup_arith_costs_db.

cleanup_arith_costs_db :-
	retractall_fact(arith_costs(_, _)).

assert_arith_costs(Platform) :-
	compound_resource(arith, Resources),
	( platform_constants(Platform, arith, _Approx, Constants)
	-> true
	; true ),
	map(Resources, assert_arith_cost, Constants).

assert_arith_cost(R, C) :-
	(var(C) -> C = 1 ; true), % Default value is 1
	resource_arith(R, AO),
	assertz_fact(arith_costs(AO, C)).

count_arith(Var, Count, Count) :-
	var(Var),
	!.
count_arith(Num, Count, Count) :-
	number(Num),
	!.
count_arith(A+B, Count0, Count) :-
	ground(B),
	B =:= 1,
	!,
	count_arith(++(A), Count0, Count).
count_arith(A-B, Count0, Count) :-
	ground(B),
	B =:= 1,
	!,
	count_arith(--(A), Count0, Count).
count_arith(Term, Count0, Count) :-
	functor(Term, A, N),
	(arith_costs(A/N, Cost) -> true ; Cost = 0),
	Count1 is Count0 + Cost,
	Term =.. [_|Terms],
	count_arith_list(Terms, Count1, Count).

count_arith_list([],           Count,  Count).
count_arith_list([Term|Terms], Count0, Count) :-
	count_arith(Term, Count0, Count1),
	count_arith_list(Terms, Count1, Count).
