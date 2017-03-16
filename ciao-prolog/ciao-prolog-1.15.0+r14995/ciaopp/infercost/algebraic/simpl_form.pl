:- module(simpl_form,
	[
	    simplification/2,
	    list_simplification/2
	], [assertions]).

:- use_module(infercost(algebraic(normal_form)), [normal_form/2]).
:- use_module(infercost(algebraic(general_form)), [general_form/2]).

%
%  Simplify a list of general-form expressions.
%
list_simplification([],[]).
list_simplification([X|Xs],[Y|Ys]) :-
	simplification(X,Y),
	list_simplification(Xs,Ys).

%
%  Simplify a general-form expression.
%
simplification(X,Y) :-
	normal_form(X,Z),
	general_form(Z,Y).

