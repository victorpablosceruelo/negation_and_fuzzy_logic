:- module(good_destination,_,[rfuzzy, clpr, debugger_pkg]).

% Define the individuals belonging to the set cities.
city(madrid).
city(istambul).
city(moscow).
city(sydney).

% Define that only cities are valid individuals for
% the fuzzy set good_destination.
:- set_prop good_destination/1 => city/1.

% A city is a good destination with a truth value of 0.1
% if we can not compute a more accurate value.
rfuzzy_default_value_for(good_destination/1, 0.3).

% The rule to determine the grade of belonging of 
% a player to the fuzzy set of good_player has a 
% confidence of 0.8. Its result is a combination of 
% how much (the truth value) he/she is swift, tall and 
% an experienced player. 
good_destination(Place) cred (prod,1) :~ prod((nice_weather(Place), many-sights(Place))).


:- set_prop nice_weather/1 => city/1.
rfuzzy_default_value_for(nice_weather/1, 0.5).

nice_weather(madrid, 0.8).
nice_weather(madrid, 0.7).
nice_weather(madrid, 0.2).

:- set_prop many_sights/1 => city/1.
rfuzzy_default_value_for(many_sights/1, 0.2).

many_sights(madrid, 0.6).
many_sights(istambul, 0.7).
many_sights(sydney, 0.6).

% Queries (examples).

% ?- good_player(X, Y). 
% ?- good_player(X, rat(4,125)). 