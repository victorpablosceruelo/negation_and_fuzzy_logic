:- module(_, [powset/2], [assertions, regtypes, nativeprops,
		ciaopp(tests(resources)),
		library(resdefs(resources_decl))]).

:- use_module(power_set_types, [int_list/1]).

:- load_resource_module(power_set_res).
:- resource output_elements.
:- head_cost(ub, output_elements, delta_output_elements).
:- literal_cost(ub, output_elements, 0).

:- entry powset(A, B) : int_list * var.


% This is the optimized version of powerset:
% :- entry powset2(A,B,C) : int_list * list * var.
% powset2([],    X, X).
% powset2([X|L], P0, P) :-
% 	append_elem(P0, X, P1, P0),
% 	powset2(L, P1, P).

powset([],    [[]]).
powset([X|L], P) :-
	powset(L, P0),
	append_elements(P0, X, P, P0).

append_elements([],     _X, T,      T).
append_elements([L|Ls], X,  [R|Rs], T) :-
	append_element(L, X, R),
	append_elements(Ls, X, Rs, T).

append_element(L, X, [X|L]).
