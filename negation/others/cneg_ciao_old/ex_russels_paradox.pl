:- module(ex_russels_paradox,_,[.(cneg)]).
% :- module(ex_russels_paradox,_,[.(cneg), debugger_pkg]).

shaves(barber, Person) :- person(Person), cneg_rt([], shaves(Person, Person)).
person(barber).
person(mayor).
