:- module(db_leisure,_,[rfuzzy, pkgs_output_debug, clpr]).
% Compilation time debug can be activated  by adding to the packages list [rfuzzy, clpr] the package pkgs_output_debug.
% Running time debug can be activated removing the comment marker % at the beginning of the following line.
:- activate_rfuzzy_debug.

% Define the restaurants database format.
rfuzzy_define_database(restaurant/7, 
	[(restaurant_id, rfuzzy_string_type), 
	  (restaurant_type, rfuzzy_enum_type), 
	   (food_type, rfuzzy_enum_type),
	    (years_since_opening, rfuzzy_integer_type), 
	     (distance_to_the_city_center, rfuzzy_integer_type), 
	      (price_average, rfuzzy_integer_type), 
	       (menu_price, rfuzzy_integer_type)]).

% restaurant(restaurant_id,                        2,        3,      4,          5,         6,        7, ).
%restaurant(rfuzzy_default_values, 1,        0,       null,      null,     null,       800).  <- nonsense ??
restaurant(kenzo,                           fast_casual,     japanese,           5,    null,       50,     null).
restaurant(burguer_king,               fast_food,        american,         10,    null,       10,     5).
restaurant(pizza_jardin,                 fast_casual,     italian,                5,    null,       15,     null).
restaurant(subway,                         fast_food,       sandwiches,        5,    null,      15,      10).
restaurant(derroscas,                     fast_food,        mediterranean,   3,    null,      25,      null).
restaurant(il_tempietto,                  fast_casual,    italian,                5,    null,       20,     null).
restaurant(kono_pizza,                   fast_food,       pizza,                  4,    null,      15,      null).
restaurant(paellador,                       fast_food,       paella,                8,     null,      40,     null).
restaurant(tapasbar,                        fast_food,       tapas,                 3,     null,      10,     null).
restaurant(meson_del_jamon,        fast_food,       spanish,              8,     100,      20,     15).
restaurant(museo_del_jamon,        fast_food,       spanish,              8,     150,      20,     15).
restaurant(zalacain,                         fine_dining,    basque,             15,     null,      60,     50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rfuzzy_similarity_between(restaurant, food_type(spanish), food_type(tapas), 0.6).
rfuzzy_similarity_between(restaurant, food_type(mediterranean), food_type(spanish), 0.6) cred (prod, 0.8).
rfuzzy_similarity_between(restaurant, food_type(mediterranean), food_type(italian), 0.6) cred (prod, 0.8).

near_the_city_center(restaurant) :~ defaults_to(0).
near_the_city_center(restaurant):~ function(distance_to_the_city_center(restaurant), [ (0, 1), (100, 1), (1000, 0.1) ]).

traditional(restaurant) :~ function(years_since_opening(restaurant), [ (0, 1), (5, 0.2), (10, 0.8), (15, 1), (100, 1) ]).
traditional(restaurant) :~ function(years_since_opening(restaurant), [ (0, 1), (5, 0.2), (10, 0.8), (15, 1), (100, 1) ]) only_for_user bartolo.

rfuzzy_type_for(is_zalacain/1, [restaurant]).
is_zalacain(Restaurant) :- restaurant_id(Restaurant, Restaurant_Id), Restaurant_Id = zalacain.

cheap(restaurant) :~ defaults_to(0.5).
cheap(restaurant) :~ defaults_to(0.2) if (near_the_city_center(restaurant) is_over 0.7).
cheap(restaurant) :~ defaults_to(0.1) if (restaurant_id(restaurant) equals zalacain).
cheap(restaurant) :~ function(price_average(restaurant), [ (0, 1), (10, 1), (15, 0.9), (20, 0.8), (30, 0.5), (50, 0.1), (100, 0) ]).

unexpensive(restaurant) :~ synonym_of(cheap(restaurant), prod, 1).
expensive(restaurant) :~ antonym_of(cheap(restaurant), prod, 1).

rfuzzy_quantifier(special_very/2, TV_In, TV_Out) :-
 	Thershold .=. 0.7,
	Min_In .=. TV_In - Thershold, 
	min(0, Min_In, Dividend), 
	TV_Out .=. ((Dividend)/(0 - Thershold)).

tempting_restaurant(restaurant) :~ defaults_to( 0.1).
tempting_restaurant(restaurant) :~ rule(min, (near_the_city_center(restaurant), fnot(special_very(expensive(restaurant))), very(traditional(restaurant)))) with_credibility (min, 0.7).
tempting_restaurant(restaurant) :~ rule(near_the_city_center(restaurant)) with_credibility (min, 0.5) .

% More tests (maybe not needed in this DB).
not_very_expensive(restaurant) :~ rule(fnot(very(expensive(restaurant)))).

rfuzzy_aggregator(max_with_min_a_half/3, TV_In_1, TV_In_2, TV_Out) :-
	max(TV_In_1, TV_In_2, TV_Aux), min(TV_Aux, 0.5, TV_Out).


test1(A, B) :- A .=. B.
test2(A, B) :- A .>. B.
test3(A, B, C) :- C .=. A + B.

% Define the films database format.
rfuzzy_define_database(film/7, 
	[(film_name, rfuzzy_string_type), 
	  (release_year, rfuzzy_integer_type), 
	   (duration_in_minutes, rfuzzy_integer_type),
	    (genre, rfuzzy_enum_type), 
	     (original_language, rfuzzy_integer_type), 
	      (directed_by, rfuzzy_integer_type), 
	       (distributed_by, rfuzzy_enum_type)]).

film('The Godfather', 1972, 207, drama, english, 'Francis Ford Coppola', 'Paramount Pictures').
film('Casablanca', 1946, 172, romance, english, 'Michael Curtiz', 'Warner Bros').
film('Cera una volta il West', 1968, 165, western, italian, 'Sergio Leone', 'MYMONETRO').
film('El laberinto del fauno', 2006, 107, drama, spanish, 'Guillermo del Toro', 'Esperanto Films').
film('Il buono, il brutto, il cattivo', 1967, 141, adventure, italian, 'Sergio Leone', 'United Artists').
film('Finding Nemo', 2003, 112, comedy, english, 'Andrew Stanton and Lee Unkrich', 'Disney - PIXAR').


rfuzzy_fuzzification(modern(film), release_year(film)) :- function([ (1970, 0), (2000, 1), (2010, 1)]).
rfuzzy_fuzzification(long_duration(film), duration_in_minutes(film)) :- function([ (120, 0), (180, 1), (600, 1)]) .

% similarity over genres
% rfuzzy_similarity_between(film, genre(drama), genre(drama), 1).
rfuzzy_similarity_between(film, genre(drama), genre(romance), 0.6).
rfuzzy_similarity_between(film, genre(drama), genre(western), 0.1).
rfuzzy_similarity_between(film, genre(drama), genre(adventure), 0.1).
rfuzzy_similarity_between(film, genre(drama), genre(comedy), 0).
rfuzzy_similarity_between(film, genre(romance), genre(drama), 0.6).
%rfuzzy_similarity_between(film, genre(romance), genre(romance), 1).
rfuzzy_similarity_between(film, genre(romance), genre(western), 0.4).
rfuzzy_similarity_between(film, genre(romance), genre(adventure), 0.3).
rfuzzy_similarity_between(film, genre(romance), genre(comedy), 0.3).
rfuzzy_similarity_between(film, genre(western), genre(drama), 0.1).
rfuzzy_similarity_between(film, genre(western), genre(romance), 0.4).
%rfuzzy_similarity_between(film, genre(western), genre(western), 1).
rfuzzy_similarity_between(film, genre(western), genre(adventure), 0.8).
rfuzzy_similarity_between(film, genre(western), genre(comedy), 0.1).
rfuzzy_similarity_between(film, genre(adventure), genre(drama), 0.1).
rfuzzy_similarity_between(film, genre(adventure), genre(romance), 0.3).
rfuzzy_similarity_between(film, genre(adventure), genre(western), 0.8).
%rfuzzy_similarity_between(film, genre(adventure), genre(adventure), 1).
rfuzzy_similarity_between(film, genre(adventure), genre(comedy), 0.2).
rfuzzy_similarity_between(film, genre(comedy), genre(drama), 0).
rfuzzy_similarity_between(film, genre(comedy), genre(romance), 0.3).
rfuzzy_similarity_between(film, genre(comedy), genre(western), 0.1).
rfuzzy_similarity_between(film, genre(comedy), genre(adventure), 0.2).
%rfuzzy_similarity_between(film, genre(comedy), genre(comedy), 1.

% similarity over languages
% rfuzzy_similarity_between(film, original_language(english), original_language(english), 1).
rfuzzy_similarity_between(film, original_language(english), original_language(spanish), 0.2).
rfuzzy_similarity_between(film, original_language(english), original_language(italian), 0.2).
rfuzzy_similarity_between(film, original_language(spanish), original_language(english), 0.2).
% rfuzzy_similarity_between(film, original_language(spanish), original_language(spanish), 1).
rfuzzy_similarity_between(film, original_language(spanish), original_language(italian), 0.7).
rfuzzy_similarity_between(film, original_language(italian), original_language(english), 0.2).
rfuzzy_similarity_between(film, original_language(italian), original_language(spanish), 0.7).
% rfuzzy_similarity_between(film, original_language(italian), original_language(italian), 1).

% funny is an example of a discrete attribute
%rfuzzy_type_for(funny/1, [film]).
%funny(genre('drama')) value 0 .
%funny('romance') value 0.4 .
%funny('western') value 0.2 .
%funny('adventure') value 0.2 .
%funny('comedy') value 1 .
