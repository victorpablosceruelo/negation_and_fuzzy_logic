:- module(ex_good_player,_,[rfuzzy, clpr]).

% Define the database.
define_database(player/4, 
	[(id, rfuzzy_enum_type), 
	  (swift, rfuzzy_truth_value_type),
	   (height, rfuzzy_integer_type), % Height of players in mm.
	    (years_playing, rfuzzy_integer_type)]).

% Define the individuals belonging to the set player.
player(john, 1, 1700, 2).
player(karl, 0.6, 1900, 3).
player(mike, 0.9, null, null).
player(lebron, 1, 1950, null).
player(deron, 0.8, 2000, 7).
player(damian, null, 1850, 1).
player(aito, null, 1800, null).
player(marcus, null, 1780, null).


% If no info provided a player is considered to be swift with a truth value of 0.5.
swift(player) :~ defaults_to(0.5).

tall(player) :~ defaults_to(0).
tall(player) :~ function(height(player), [ (1500, 0), (1800, 0.5), (2000, 1), (3000, 1) ]).

experience(player) :~ defaults_to(0.1).
experience(player) :~ function(years_playing(player), [(0,0), (2, 0.2), (5, 1)]).


% A player is a good player with a truth value of 0.1
% if we can not compute a more accurate value.
good_player(player) :~ defaults_to(0.1).

% The rule to determine the grade of belonging of 
% a player to the fuzzy set of good_player has a 
% confidence of 0.8. Its result is a combination of 
% how much (the truth value) he/she is swift, tall and 
% an experienced player. 
good_player(player)  :~ rule(prod, (swift(player), tall(player), experience(player))) with_credibility (prod,0.8). 

% Queries (examples).

% ?- good_player(X, Y). 
% ?- good_player(X, rat(4,125)). 