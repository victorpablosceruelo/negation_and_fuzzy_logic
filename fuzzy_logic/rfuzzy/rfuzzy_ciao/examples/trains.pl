:- module(trains, _, [rfuzzy, clpr]).
:- use_module(library(write),[write/1]).

% Do not use this to define valid natural numbers.
%speed(0). % Units are kilometres per hour
%speed(X) :-
%	speed(Y), number(Y),
%	X .=. Y + 1,
%	(   X =< 500 ; ( X > 500, !, fail) ).
speed(X) :- X .>=. 0.

:- set_prop max_speed/1 => speed/1.
rfuzzy_default_value_for(max_speed/1, 0).
max_speed :# ([ (200, 0), (250, 1), (500, 1) ]).

:- set_prop high_speed/1 => speed/1.
rfuzzy_default_value_for(high_speed/1, 1) .
high_speed :# ([(0, 0), (100, 0), (150, 0.25), (200, 0.5), (250, 1)]).

:- set_prop normal_speed/1 => speed/1.
rfuzzy_default_value_for(normal_speed/1, 1) .
normal_speed :# ([(0, 0), (100, 0.5), (200, 1), (250, 0.5), (300, 0)]).

:- set_prop low_speed/1 => speed/1.
rfuzzy_default_value_for(low_speed/1, 0) .
low_speed :# ([(0, 1), (50, 1), (100, 0.5), (150, 0)]).

%distance(0). % Units are kilometres.
%distance(X) :-
%	distance(Y), number(Y),
%	X .=. Y + 1,
%	(   X =< 500 ; ( X > 500, !, fail) ).
distance(X) :- X .>=. 0.

:- set_prop low_distance/1 => distance/1.
rfuzzy_default_value_for(low_distance/1, 0) .
low_distance :# ([(0, 1), (1, 0.75), (2, 0.5), (3, 0.25), (5, 0)]).

:- set_prop mid_distance/1 => distance/1.
rfuzzy_default_value_for(mid_distance/1, 0) .
mid_distance :# ([(3, 0), (5, 1), (7, 1), (10, 0)]).

:- set_prop high_distance/1 => distance/1.
rfuzzy_default_value_for(high_distance/1, 1) .
high_distance :# ([(0, 0),(10, 0), (100, 0.5), (200, 1)]).

% D is distance, S is speed.
:- set_prop reduce_speed/2 => distance/1, speed/1.
rfuzzy_default_value_for(reduce_speed/2, 1) .
reduce_speed(D, S) :~ prod mid_distance(D), high_speed(S).
:- set_prop activate_brakes/2 => distance/1, speed/1.
rfuzzy_default_value_for(activate_brakes/2, 0.5) .
activate_brakes(D, S) :~ max low_distance(D), reduce_speed(D, S).
disable_brakes(D, S) cred (complement, 1) :~ max activate_brakes(D, S), activate_brakes(D, S).
:- set_prop accelerate/2 => distance/1, speed/1.
rfuzzy_default_value_for(accelerate/2, 1) .
accelerate(D, S)  :~ min disable_brakes(D, S), high_distance(D).


