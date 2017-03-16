:- module(compass_access, [read_compass/1], []).

:- initialization(init_compass).

:- use_module(library(read)).

init_compass.

:- data angle/1.

angle(0).

%% Simulate a compass which spins around.

read_compass(Angle):- 
        retract_fact(angle(Angle)),
        NewAngle is Angle + 1,
        asserta_fact(angle(NewAngle)).
