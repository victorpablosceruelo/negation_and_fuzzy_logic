:- module(no_compass, [read_compass/1], [assertions]).

% :- initialization(init_compass).

:- use_module(library(read)).

% init_compass.

% :- data angle/1.

% angle(0).

%% Simulate a compass which spins around.

:- true pred read_compass(X) : var(X) => (flt(X), ground(X)).

read_compass(X):- read(X).
 %% :-
 %%         retract_fact(angle(Angle)),
 %%         NewAngle is Angle + 1,
 %%         asserta_fact(angle(NewAngle)).
