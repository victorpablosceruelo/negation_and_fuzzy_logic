:- module(ex_trains, _, [rfuzzy, clpr]).

% Activate/Deactivate debug.
:- activate_rfuzzy_debug.

% Do not use this to define valid natural numbers.
%speed(0). % Units are kilometres per hour
%speed(X) :-
%	speed(Y), number(Y),
%	X .=. Y + 1,
%	(   X =< 500 ; ( X > 500, !, fail) ).
% speed(X) :- X .>=. 0.
define_database(speed/1, [(speed_value, rfuzzy_integer_type)]).
speed(X) :- current_input(S), get_code(S, X).

fast(speed) :~ function(speed_value(speed), [(0, 0), (100, 0.5), (200, 1), (250, 0.5), (300, 0)]).

define_modifier(not_so/1, under, 0.4).
define_modifier(very/1, over, 0.5).
define_modifier(too_much/1, over, 0.7).

%distance(0). % Units are kilometres.
%distance(X) :-
%	distance(Y), number(Y),
%	X .=. Y + 1,
%	(   X =< 500 ; ( X > 500, !, fail) ).
define_database(distance/1, [(distance_value, rfuzzy_integer_type)]).
distance(X) :- current_input(S), get_code(S, X).

far(distance) :~ function(distance_value(distance), [(0, 1), (1, 0.75), (2, 0.5), (3, 0.25), (5, 0)]).
define_antonym(far/1, close/1, prod, 1).

reduce_speed(distance, speed) :~ defaults_to(1).
reduce_speed(distance, speed) :~ rule(prod, (far(distance), very(fast(speed)))).

activate_brakes(distance, speed) :~ defaults_to(0.5) .
activate_brakes(distance, speed) :~ rule(max, (close(distance), reduce_speed(distance, speed))).

define_antonym(activate_brakes/2, disable_brakes/2, prod, 1).
% disable_brakes(D, S) cred (complement, 1) :~ max((activate_brakes(D, S), activate_brakes(D, S))).

accelerate(distance, speed) :~ defaults_to(1) .
accelerate(distance, speed)  :~ rule(min, (disable_brakes(distance, speed), very(far(D)))).


