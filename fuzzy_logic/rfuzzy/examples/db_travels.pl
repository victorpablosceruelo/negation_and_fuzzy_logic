:- module(db_travels,_,[rfuzzy, clpr]).

% Define the database.
define_database(city/3, 
	[(id, rfuzzy_enum_type), 
	  (nice_weather, rfuzzy_truth_value_type),
	   (many_sights,  rfuzzy_truth_value_type)]).

% Define the individuals belonging to the set cities.
city(madrid, 0.8, 0.6).
city(istambul, 0.7, 0.7).
city(moscow, 0.2, null).
city(sydney, null, 0.6).

nice_weather(city) :~ defaults_to(0.5).
many_sights(city) :~ defaults_to(0.2).

% A city is a good destination with a truth value of 0.1
% if we can not compute a more accurate value.
good_destination(city) :~ defaults_to(0.3).

% A city is a good destination if it has nice_weather and many_sights
% We 
good_destination(city)  :~ rule(prod, (nice_weather(city), many_sights(city))) with_credibility (prod,1).

% Queries (examples).

% ?- good_player(X, Y). 
% ?- good_player(X, rat(4,125)). 