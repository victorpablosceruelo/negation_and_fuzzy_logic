:- module(ex_russels_paradox,_,[.(cneg)]).
% :- module(ex_russels_paradox,_,[.(cneg), debugger_pkg]).

shaves(barber, Person) :- person(Person), cneg([], shaves(Person, Person)).
person(barber).
person(mayor).
