:- module(_, _, [assertions]).

:- use_module(library(aggregates)).
:- use_module(library(llists)).
:- use_module(library(sort)).

:- use_module(profcost(caliresource(caliresource_cost))).

not_null_resources(As) :-
	valid_resources(A0),
	findall(A,
	    (
		bench_model(_, C),
		not_null_resources_model(C, A0, A)
	    ),
	    As0),
	flatten(As0, As1),
	sort(As1, As).

not_null_resources_model([0|C], [_|A0], A) :-
	!,
	not_null_resources_model(C, A0, A).
not_null_resources_model([_|C], [R|A0], [R|A]) :-
	not_null_resources_model(C, A0, A).
not_null_resources_model([], [], []).
