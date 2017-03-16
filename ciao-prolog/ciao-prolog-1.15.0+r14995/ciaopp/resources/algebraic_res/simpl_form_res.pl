:- module(simpl_form_res,
	    [
		simplification/2,
		time_simplification/2,
		list_simplification/2
	    ], [assertions]).

:- use_module(resources(algebraic_res(normal_form_res)), 
	    [normal_form/2]).
:- use_module(resources(algebraic_res(general_form_res)), 
	    [general_form/2]).

% Added by EMM
time_simplification(A, B) :- resource_simplification(A, B), !.
time_simplification(A, B) :- simplification(A, B).

resource_simplification([],                []).
resource_simplification([OrigValue|Origs], [SimpValue|Simps]) :-
	simplification(OrigValue, SimpValue),
	resource_simplification(Origs, Simps).
% End Added by EMM

%
%  Simplify a list of general-form expressions.
%
list_simplification([],     []).
list_simplification([X|Xs], [Y|Ys]) :-
	simplification(X, Y),
	list_simplification(Xs, Ys).

%
%  Simplify a general-form expression.
%
simplification(X, Y) :-
	normal_form(X, Z),
	general_form(Z, Y).
