:- module(restaurant_db,_,[rfuzzy, clpr]).
% or clpq. We can use clpr or clpq.
% debug with pkgs_output_debug, 

% Define the file where we want debug msgs.
% :- define_pkgs_output_debug_file('~/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/rfuzzy_ciao/debug_restaurant.pl').

% Activate/Deactivate debug.
:- activate_rfuzzy_debug.

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
restaurant(il_tempietto,                  fast_casusal,   italian,                5,    null,       20,     null).
restaurant(kono_pizza,                   fast_food,       pizza,                  4,    null,      15,      null).
restaurant(paellador,                       fast_food,       paella,                8,     null,      40,     null).
restaurant(tapasbar,                        fast_food,       tapas,                 3,     null,      10,     null).
restaurant(meson_del_jamon,        fast_food,       spanish,              8,     100,      20,     15).
restaurant(museo_del_jamon,        fast_food,       spanish,              8,     150,      20,     15).
restaurant(zalacain,                         fine_dining,    basque,             15,     null,      60,     50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

near_function :# (0, [ (0, 1), (100, 1), (1000, 0.1) ], 1000) .

% rfuzzy_type_for('fuzzy_rule', near_the_city_center/1, [restaurant]).
rfuzzy_default_value_for(near_the_city_center, 0).
rfuzzy_define_fuzzification(near_the_city_center, distance_to_the_city_center, near_function).

traditional_function :# (0, [ (0, 1), (5, 0.2), (10, 0.8), (15, 1), (100, 1) ], 100) .
rfuzzy_define_fuzzification(traditional, years_since_opening, traditional_function).

rfuzzy_type_for('crisp_rule', is_zalacain/1, [restaurant]).
is_zalacain(Restaurant) :- restaurant_id(Restaurant, Restaurant_Id), Restaurant_Id = zalacain.

rfuzzy_type_for('fuzzy_rule', cheap/1, [restaurant]).
rfuzzy_default_value_for(cheap/1, 0.5).
rfuzzy_default_value_for(cheap/1, 0.2) if thershold(near_the_city_center/1, over, 0.7).
rfuzzy_default_value_for(cheap/1, 0.1) if is_zalacain/1.

rfuzzy_synonym(cheap/1, unexpensive/1, prod, 1).
rfuzzy_antonym(cheap/1, expensive/1, prod, 1).

very(Fuzzy_Predicate_Functor_In, Truth_Value) :-
	functor(Fuzzy_Predicate_Functor_In, _FP_Name, FP_Arity), 
	arg(FP_Arity, Fuzzy_Predicate_Functor_In, FP_Truth_Value),
	Fuzzy_Predicate_Functor_In,
	Thershold .=. 0.7,
	Min_In .=. FP_Truth_Value - Thershold, 
	min(0, Min_In, Dividend), 
	Truth_Value .=. ((Dividend)/(0 - Thershold)).

rfuzzy_quantifier(very/2).

rfuzzy_type_for('fuzzy_rule', tempting_restaurant/1, [restaurant]).
rfuzzy_default_value_for(tempting_restaurant/1, 0.1).
tempting_restaurant(R) cred (min, 0.7) :~ min((near_the_city_center(R), fnot(very(expensive(R))), traditional(R))).
tempting_restaurant(R) cred (min, 0.5) :~ near_the_city_center(R).

% More tests (maybe not needed in this DB).
not_very_expensive_restaurant(R) :~ fnot(very(expensive(R))).

max_with_min_a_half(X, Y, Z) :- max(X, Y, W), min(W, 0.5, Z).
rfuzzy_aggregator(max_with_min_a_half/3).



