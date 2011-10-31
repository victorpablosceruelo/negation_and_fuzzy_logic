:- module(ex_russels_paradox,_,[.(cneg), debugger_pkg]).
% :- module(ex_ancestor,_,[.(cneg)]).

shaves(barber, Person) :- person(Person), cneg_rt([], shaves(Person, Person)).
person(barber).
person(mayor).
