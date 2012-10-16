:- module(restaurant_db,_,[rfuzzy, pkgs_output_debug, clpr]).
% or clpq. We can use clpr or clpq.
% debug with pkgs_output_debug, 

% Define the file where we want debug msgs.
% :- define_pkgs_output_debug_file('~/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/rfuzzy_ciao/debug_restaurant.pl').

% Activate/Deactivate debug.
% :- activate_rfuzzy_debug.

rfuzzy_type_for('database', restaurant/7, [rfuzzy_id_type, rfuzzy_truth_value_type, rfuzzy_truth_value_type, 
	rfuzzy_number_type, rfuzzy_truth_value_type, rfuzzy_truth_value_type, rfuzzy_number_type]).
% restaurant 2 -> traditional
% restaurant 3 -> low_distance
% restaurant 4 -> distance_to_the_city_center
% restaurant 5 -> near_the_city_center
% restaurant 6 -> cheap
% restaurant 7 -> distance_to_us
% restaurant(name,                        2,        3,      4,          5,         6,        7, ).
restaurant(rfuzzy_default_values, 1,        0,       null,      null,     null,       800).
restaurant(kenzo,                            0.5,    null,    null,      1,         0.3,       150).
restaurant(burguer_king,                null,    null,    null,      null,     null,      500).
restaurant(pizza_jardin,                  null,    null,    null,      null,     null,      250).
restaurant(subway,                          null,    null,    null,      null,    1,          null).
restaurant(derroscas,                       null,    null,   null,      null,     1,          null).
restaurant(il_tempietto,                   null,     null,   null,      null,     null,     100).
restaurant(kono_pizza,                    null,    null,    null,      null,     null,      null).
restaurant(paellador,                       0.87,    null,   null,      null,     null,      null).
restaurant(tapasbar,                        null,     null,   null,      null,     null,      null).
restaurant(meson_del_jamon,        null,     null,   100,      null,     null,      null).
restaurant(museo_del_jamon,        null,     null,   150,      null,     null,      null).
restaurant(zalacain,                         null,     null,    null,      null,     null,      null).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rfuzzy_db_value_for(traditional, restaurant, 2).
rfuzzy_db_value_for(low_distance, restaurant, 3).
rfuzzy_db_value_for(distance_to_the_city_center, restaurant, 4).

near_function :# ([ (0, 1), (100, 1), (1000, 0.1) ]) .

% rfuzzy_type_for('fuzzy_rule', near_the_city_center/1, [restaurant]).
rfuzzy_define_fuzzification(near_the_city_center, distance_to_the_city_center, near_function).
rfuzzy_db_value_for(near_the_city_center, restaurant, 5).

is_zalacain(zalacain).

rfuzzy_type_for('fuzzy_rule', cheap/1, [restaurant]).
rfuzzy_default_value_for(cheap/1, 0.5).
rfuzzy_default_value_for(cheap/1, 0.2) if thershold(near_the_city_center/1, over, 0.7).
rfuzzy_default_value_for(cheap/1, 0.1) if is_zalacain/1.
rfuzzy_db_value_for(cheap, restaurant, 6).

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
tempting_restaurant(R) cred (min, 0.7) :~ min((low_distance(R), fnot(very(expensive(R))), traditional(R))).
tempting_restaurant(R) cred (min, 0.5) :~ low_distance(R).

rfuzzy_db_value_for(distance_to_us, restaurant, 7).
rfuzzy_define_fuzzification(near_to_us, distance_to_us, near_function).

% More tests (maybe not needed in this DB).
not_very_expensive_restaurant(R) :~ fnot(very(expensive(R))).

max_with_min_a_half(X, Y, Z) :- max(X, Y, W), min(W, 0.5, Z).
rfuzzy_aggregator(max_with_min_a_half/3).



