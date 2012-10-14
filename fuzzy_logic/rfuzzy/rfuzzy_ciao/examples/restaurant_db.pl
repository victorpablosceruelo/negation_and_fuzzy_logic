:- module(restaurant_db,_,[rfuzzy, pkgs_output_debug, clpr]).
% or clpq. We can use clpr or clpq.
% debug with pkgs_output_debug, 

% Define the file where we want debug msgs.
% :- define_pkgs_output_debug_file('~/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/rfuzzy_ciao/debug_restaurant.pl').

% Activate/Deactivate debug.
:- activate_rfuzzy_debug.

rfuzzy_type_for('crisp_rule', restaurant/7, [rfuzzy_id_type, rfuzzy_truth_value_type, rfuzzy_truth_value_type, 
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

rfuzzy_db_value_for(traditional/1, restaurant, 2, rfuzzy_truth_value_type).
rfuzzy_db_value_for(low_distance/1, restaurant, 3, rfuzzy_truth_value_type).
rfuzzy_db_value_for(distance_to_the_city_center/1, restaurant, 4, rfuzzy_number_type).

near_function :# ([ (0, 1), (100, 1), (1000, 0.1) ]) .

rfuzzy_type_for('fuzzy_rule', near_the_city_center/1, [restaurant]).
rfuzzy_define_fuzzification(near_the_city_center/1, distance_to_the_city_center, near_function).
rfuzzy_db_value_for(near_the_city_center/1, restaurant, 5, rfuzzy_truth_value_type).

is_zalacain(zalacain).

rfuzzy_type_for('fuzzy_rule', cheap/1, [restaurant/1]).
rfuzzy_default_value_for(cheap/1, 0.5).
rfuzzy_default_value_for(cheap/1, 0.2) if thershold(near_the_city_center/1, over, 0.7).
rfuzzy_default_value_for(cheap/1, 0.1) if is_zalacain/1.
rfuzzy_db_value_for(cheap/1, restaurant, 6, rfuzzy_truth_value_type).

rfuzzy_synonym(cheap/1, unexpensive/1, prod, 1).
rfuzzy_antonym(cheap/1, expensive/1, prod, 1).
rfuzzy_quantifier(very/1, over, 0.7).

rfuzzy_type_for('fuzzy_rule', tempting_restaurant/1, [restaurant/1]).
rfuzzy_default_value_for(tempting_restaurant/1, 0.1).
tempting_restaurant(R) cred (min, 0.7) :~ min((low_distance(R), fnot(very(expensive(R))), traditional(R))).
tempting_restaurant(R) cred (min, 0.5) :~ low_distance(R).

rfuzzy_db_value_for(distance_to_us/1, restaurant, 7, rfuzzy_number_type).
rfuzzy_define_fuzzification(near_to_us/1, distance_to_us/2, near_function/2).

% More tests (maybe not needed in this DB).
not_very_expensive_restaurant(R) :~ fnot(very(expensive(R))).

max_with_min_a_half(X, Y, Z) :- max(X, Y, W), min(W, 0.5, Z).
rfuzzy_aggregator(max_with_min_a_half/3).



