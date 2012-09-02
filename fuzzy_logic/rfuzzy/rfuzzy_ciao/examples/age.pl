:- module(age, _, [rfuzzy, clpr]).
:- use_module(library(write),[write/1]).

age(X) :- X .>=. 0, X .<. 150.
% Do not use this to define valid natural numbers.
% age(0).
% age(X) :-
%	age(Y),
%	number(Y),
%	X .=. Y + 1,
%	(   X < 150 ;
%	    ( X >= 150, !, fail)
%	).

rfuzzy_type_for(child/1,  [age/1]).
rfuzzy_default_value_for(child/1, 0).
child :# ([ (0, 1), (10, 1), (20, 0) ]) .
child(26) value 1. % I feel like a child ;-)

rfuzzy_type_for(teenager/1, [age/1]).
rfuzzy_default_value_for(teenager/1, 0).
teenager :# ([ (9, 0), (10, 1) , (19, 1), (20, 0) ]) .

rfuzzy_type_for(young/1, [age/1]).
rfuzzy_default_value_for(young/1, 0).
young :# ([ (20, 1), (30, 1) ]) . 
% I'll be young until 30, rules do not apply to my age ;-)
young(X) :~ max((go_out(X), do_not_have_children(X))) .

rfuzzy_type_for(adult/1, [age/1]).
rfuzzy_default_value_for(adult/1, 1).
adult(X) cred (complement, 1) :~ dluka((young(X), child(X))) .

% rfuzzy_type_for(go_out/1, [age/1]).
% rfuzzy_default_value_for(go_out/1, 0) .
go_out :# ([(10, 0), (18, 1), (40, 0.4), (65, 0.4), (70, 0.7), (80, 0)]).

rfuzzy_type_for(do_not_have_children/1, [age/1]).
rfuzzy_default_value_for(do_not_have_children/1, 1) .
rfuzzy_default_value_for(do_not_have_children/1, 0) if older_than_40/1.
do_not_have_children :# ([(10, 1), (15, 0.9), (25, 0.5), (35, 0.1), (40, 0)]).

older_than_40(X) :- X .>. 40.

test_child :- child(X,V), test_print(X,V).
test_young :- young(X,V), test_print(X,V).
test_adult :- adult(X,V), test_print(X,V).

test_print(X, V) :- write(X), write(' -> '), write(V), write('   '), nl, fail.

%:- use_module(library(write),[write/1]).
test(X) :- X .>. 0.8, 
	write(X), nl.
%	functor(X, Name, Arity), 
%	write(Name),  nl,
%	write(Arity), nl, 
%	X =..[Name|Args],
%	write(Args), nl.
