:- module(good_player,_,[rfuzzy, clpr, debugger_pkg]).

% Define the individuals belonging to the set player.
player(john).
player(karl).
player(mike).
player(lebron).
player(deron).
player(damian).
player(aito).

% Define that only player are valid individuals for
% the fuzzy set good_player.
:- set_prop good_player/1 => player/1.

% An individual is a good player with a truth value of 0.1
% if we can not compute a more accurate value.
:- default(good_player/1, 0.1).

% The rule to determine the grade of belonging of 
% a player to the fuzzy set of good_player has a 
% confidence of 0.8. Its result is a combination of 
% how much (the truth value) he/she is swift, tall and 
% an experienced player. 
good_player(J) cred (prod,0.8) :~ prod((swift(J), tall(J), experience(J))).


:- set_prop experience/1 => player/1.
:- default(experience/1, 0.1).

experience(lebron) value 0.4.
experience(deron) value 0.3.


% Tall predicate depending on the player's height.
tall(Player, V) :- height(Player, H), tall_func(H, V).
tall_func :# ([ (1500, 0), (1800, 0.5), (2000, 1), (3000, 1) ]).

% Height of players in mm.
height(john, 1700).
height(marcus, 1780).
height(aito, 1800).
height(damian, 1850).
height(karl, 1900).
height(lebron, 1950).
height(deron, 2000).

:- set_prop swift/1 => player/1.
:- default(swift/1,0.5).

swift(john) value 1.
swift(karl) value 0.6 .
swift(mike) value 0.9 .
swift(lebron) value 1.
swift(deron) value 0.8.

% Queries (examples).

% ?- good_player(X, Y). 
% ?- good_player(X, rat(4,125)). 