:- module(ex_russels_paradox,_,[cneg]).
% :- module(ex_russels_paradox,_,[.(cneg), debugger_pkg]).

test('test', shaves(X, Y), 'should_succeed', fail_and_forget_it((X, Y)), 'should_fail').

shaves(barber, Person) :- person(Person), cneg(shaves(Person, Person)).
person(barber).
person(mayor).
