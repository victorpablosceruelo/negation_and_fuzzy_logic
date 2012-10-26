:- module(database,_,[rfuzzy, clpr]).
% or clpq. We can use clpr or clpq.
% debug with pkgs_output_debug, 

% Define the file where we want debug msgs.
% :- define_pkgs_output_debug_file('~/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/rfuzzy_ciao/debug_restaurant.pl').

% Activate/Deactivate debug.
% :- activate_rfuzzy_debug.

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

near_function :# (0, [ (0, 1), (100, 1), (1000, 0.1) ], 1000) .

rfuzzy_default_value_for(near_the_city_center(restaurant), 0).
rfuzzy_define_fuzzification(near_the_city_center(restaurant), distance_to_the_city_center(restaurant), near_function).

traditional_function :# (0, [ (0, 1), (5, 0.2), (10, 0.8), (15, 1), (100, 1) ], 100) .
rfuzzy_define_fuzzification(traditional(restaurant), years_since_opening(restaurant), traditional_function).

rfuzzy_type_for(is_zalacain/1, [restaurant]).
is_zalacain(Restaurant) :- restaurant_id(Restaurant, Restaurant_Id), Restaurant_Id = zalacain.

cheap_restaurant_function :# (0, [ (0, 1), (10, 1), (15, 0.9), (20, 0.8), (30, 0.5), (50, 0.1), (100, 0) ], 100) .

rfuzzy_default_value_for(cheap(restaurant), 0.5).
rfuzzy_default_value_for(cheap(restaurant), 0.2) if thershold(near_the_city_center(restaurant), over, 0.7).
rfuzzy_default_value_for(cheap(restaurant), 0.1) if is_zalacain(restaurant).
rfuzzy_define_fuzzification(cheap(restaurant), price_average(restaurant), cheap_restaurant_function).

rfuzzy_synonym(cheap(restaurant), unexpensive(restaurant), prod, 1).
rfuzzy_antonym(cheap(restaurant), expensive(restaurant), prod, 1).

rfuzzy_quantifier(my_very/2, TV_In, TV_Out) :-
 	Thershold .=. 0.7,
	Min_In .=. TV_In - Thershold, 
	min(0, Min_In, Dividend), 
	TV_Out .=. ((Dividend)/(0 - Thershold)).

rfuzzy_default_value_for(tempting_restaurant(restaurant), 0.1).
tempting_restaurant(restaurant) cred (min, 0.7) :~ min((near_the_city_center(restaurant), fnot(my_very(expensive(restaurant))), very(traditional(restaurant)))).
tempting_restaurant(restaurant) cred (min, 0.5) :~ near_the_city_center(restaurant).

% More tests (maybe not needed in this DB).
not_very_expensive(restaurant) :~ fnot(very(expensive(restaurant))).

rfuzzy_aggregator(max_with_min_a_half/3, TV_In_1, TV_In_2, TV_Out) :-
	max(TV_In_1, TV_In_2, TV_Aux), min(TV_Aux, 0.5, TV_Out).


test1(A, B) :- A .=. B.
test2(A, B) :- A .>. B.
test3(A, B, C) :- C .=. A + B.

% TYPE DECLARATION
rfuzzy_define_database(house/7, 
	[(house_code, rfuzzy_enum_type), 
	  (house_type, rfuzzy_enum_type), 
	   (house_size, rfuzzy_integer_type),
	    (house_rooms_number, rfuzzy_integer_type), 
	     (price, rfuzzy_integer_type), 
	      (house_distance_to_the_center, rfuzzy_integer_type), 
	       (house_distance_to_the_beach, rfuzzy_integer_type)]).

% DATABASE
house(lfs2168,'apartment',114,5,630000,2,5700).
house(lfs2144,'apartment',77,3,420000,7,3500).
house(lfs2147,'apartment',80,2,675000,12,200).
house(lfs2145,'apartment',224,8,790000,20,100).
house(c358,'apartment',74,3,340000,5,3100).
house(lfs2110,'apartment',415,9,2500000,8,2400).
house(lfs2124,'apartment',63,2,275000,15,450).
house(lfs2123,'apartment',62,3,285000,6,1000).
house(lfs2155,'villa',2300,9,3000000,13,800).
house(lfs2111,'villa',700,10,1100000,9,4500).
house(lfs2047,'villa',1750,11,1650000,15,1000).
house(lfs2041,'villa',4000,13,2500000,4,1800).
house(es13462,'villa',600,6,4000000,6,1500).
house(lfs1942,'villa',900,10,3100000,3,3400).
house(lfs1917,'villa',210,5,590000,13,5000).
house(lfb143,'villa',1200,9,2750000,7,4000).
house(5607/152,'town_house',161,7,815000,6,1200).
house(es13340,'town_house',1025,8,2800000,25,7000).
house(lfs1939,'town_house',860,9,1800000,14,2400).
house(lfs1938,'town_house',520,11,1990000,19,80).

% FUZZY FUNCTIONS OVER QUANTITATIVE ATTRIBUTES
expensive_func :# (50000, [(50000,0),(100000,0.1),(250000,0.2),(350000,0.3),(450000,0.5),(550000,0.6),
	            (800000,0.7),(1000000,0.8),(1500000,0.9),(2500000,1)], 2500000).
cheap_func :# (0, [(0,1),(30000,1),(50000,0.8),(100000,0.7),(250000,0.5),(350000,0.3),
	            (450000,0.1),(550000,0)], 550000).
big_func :# (0, [(0,0),(50,0.1),(80,0.2),(120,0.3),(200,0.4),(300,0.5),(500,0.7),(1000,0.8),(1500,0.9),(2500,1)], 2500).
small_func :# (0, [(0,1),(50,1),(80,0.9),(100,0.8),(150,0.7),(200,0.5),(300,0.2),(400,0.1),(500,0)], 500).
close_to_center_func :# (0, [(0,1),(2,1),(4,0.8),(7,0.6),(10,0.5),(12,0.3),(15,0.2),(20,0)], 20).
far_from_center_func :# (0, [(0,0),(7,0),(8,0.1),(10,0.3),(14,0.4),(20,0.7),(25,0.8),(30,1)], 30).
close_to_beach_func :# (0, [(0,1),(100,1),(1000,0.5),(2000,0)], 2000).

% FUZZY FUNCTIONS OVER THE DATABASE
rfuzzy_define_fuzzification(expensive(house), price(house), expensive_func).
rfuzzy_define_fuzzification(cheap(house), price(house), cheap_func).
rfuzzy_define_fuzzification(big(house), size(house), big_func).
rfuzzy_define_fuzzification(small(house), size(house), small_func).
rfuzzy_define_fuzzification(close_to_center(house), distance_to_the_center(house), close_to_center_func).
rfuzzy_define_fuzzification(far_from_center(house), distance_to_the_center(house), far_from_center_func).
rfuzzy_define_fuzzification(close_to_beach(house), distance_to_the_beach(house), close_to_beach_func).

% CRISP FUNCTIONS
equal(X,X).
greater(X,Y):- X.>.Y.

% QUANTIFIERS
houses_very_func :# (0, [(0,0),(0.5,0),(0.8,0.8),(1,1)], 1).
houses_little_func :# (0, [(0,1),(0.1,1),(0.4,0),(1,0)], 1).

rfuzzy_quantifier(houses_very/2, TV_In, TV_Out) :-
	houses_very_func(TV_In, TV_Out).

rfuzzy_quantifier(houses_little/2, TV_In, TV_Out) :-
	houses_little_func(TV_In, TV_Out).

% QUALIFIED FUZZY FUNCTIONS
very_expensive(house):~ houses_very(expensive(house)).
very_cheap(house):~ houses_very(cheap(house)).
very_big(house):~ houses_very(big(house)).
very_small(house):~ houses_very(small(house)).
very_close_to_center(house):~ houses_very(close_to_center(house)).
very_far_from_center(house):~ houses_very(far_from_center(house)).
very_close_to_beach(house):~ houses_very(close_to_beach(house)).

little_expensive(house):~ houses_little(expensive(house)).
little_cheap(house):~ houses_little(cheap(house)).
little_big(house):~ houses_little(big(house)).
little_small(house):~ houses_little(small(house)).
little_close_to_center(house):~ houses_little(close_to_center(house)).
little_far_from_center(house):~ houses_little(far_from_center(house)).
little_close_to_beach(house):~ houses_little(close_to_beach(house)).

% Our main aggregator
rfuzzy_aggregator(my_prod/3, TV_In_1, TV_In_2, TV_Out) :-
	TV_Out .=. TV_In_1 * TV_In_2.
test_my_prod(M) :- X .=. 0.7, Y .=. 0.5, my_prod(X, Y, M).


% Rules
rfuzzy_default_value_for(q1(house),0.5).
q1(house):~ my_prod((cheap(house), close_to_center(house))).

rfuzzy_default_value_for(q2(house),0.5).
q2(house):~ my_prod((cheap(house), little_big(house), close_to_beach(house))).

rfuzzy_default_value_for(q3(house),0.5).
q3(house):~ my_prod((close_to_beach(house), very_far_from_center(house))).

rfuzzy_type_for(is_apartment/1, [house]).
is_apartment(House) :- house_type(House, House_Type), House_Type = 'apartment'.
rfuzzy_default_value_for(q4(house), 0) if is_apartment(house).

rfuzzy_default_value_for(q5(house),0.5).
q5(house):~ my_prod((expensive(house), big(house), very_close_to_beach(house))).% missing crisp and not* parts 

rfuzzy_default_value_for(q6(house),0.5).
q6(house):~ my_prod((very_cheap(house))). % 2 missing crisp parts

rfuzzy_default_value_for(q7(house),0.5).
q7(house):~ my_prod((close_to_center(house), close_to_beach(house))). % 2 missing crisp parts
