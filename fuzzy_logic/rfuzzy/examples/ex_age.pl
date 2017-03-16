:- module(ex_age, _, [rfuzzy, clpr]).
:- use_module(library(write),[write/1]).

age_type(X) :- X .>=. 0, X .<. 150.
% Do not use this to define valid natural numbers.
% age(0).
% age(X) :-
%	age(Y),
%	number(Y),
%	X .=. Y + 1,
%	(   X < 150 ;
%	    ( X >= 150, !, fail)
%	).

define_database(person/3, 
	[(person_name, rfuzzy_enum_type), 
	  (age, rfuzzy_integer_type), 
	   (amount_of_descendants, rfuzzy_integer_type)]).

person(victor, 30, 0).
person(samuel, 32, 0).
person(oscar, 28, 0).
person(alejandro, 12, 0).
person(susana, 40, 2).

has_children(Person) :- amount_of_descendants(Person, X), X .>. 0.

child(person) :~ defaults_to(0).
child(person) :~ function(age(person), [ (0, 1), (10, 1), (20, 0) ]).
child(person) :~ value(1) if (person_name(person) equals alejandro). 

teenager(person) :~ defaults_to(0).
teenager(person) :~ function(age(person), [ (9, 0), (10, 1) , (19, 1), (20, 0) ]).

young(person) :~ defaults_to(0).
young(person) :~ function(age(person), [ (20, 1), (30, 0.5) ]). 

% I'll be young forever, rules do not apply to me ... ;-)
young(person) :~ value(1) if (name(person) equals victor).
% Susana says the same, but she is not as young as me ... ;-)
young(person) :~ value(0.9) if (name(person) equals susana).

adult(person) :~ defaults_to(0.5).
adult(person) :~ defaults_to(0.7) if (amount_of_descendants(person) is_over 0).
adult(person) :~ function(age(person), [ (20, 0.5), (100, 1) ]). 

old(person) :~ defaults_to(0.5).
old(person) :~ antonym_of(young(person), prod, 1).


test_child :- child(X,V), test_print(X,V).
test_young :- young(X,V), test_print(X,V).
test_adult :- adult(X,V), test_print(X,V).
test_old :- old(X,V), test_print(X,V).

test_print(X, V) :- write(X), write(' -> '), write(V), write('   '), nl, fail.

%:- use_module(library(write),[write/1]).
test(X) :- X .>. 0.8, 
	write(X), nl.
%	functor(X, Name, Arity), 
%	write(Name),  nl,
%	write(Arity), nl, 
%	X =..[Name|Args],
%	write(Args), nl.
