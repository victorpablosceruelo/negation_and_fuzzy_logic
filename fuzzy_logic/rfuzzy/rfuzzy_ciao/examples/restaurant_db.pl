:- module(restaurant,_,[rfuzzy, pkgs_output_debug, clpr]).
% or clpq. We can use clpr or clpq.
% debug with pkgs_output_debug, 

% Define the file where we want debug msgs.
% :- define_pkgs_output_debug_file('~/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/rfuzzy_ciao/debug_restaurant.pl').

% Activate/Deactivate debug.
% :- activate_rfuzzy_debug.

% restaurant 2 -> traditional
% restaurant 3 -> low_distance
% restaurant 4 -> distance_to_the_city_center
% restaurant 5 -> near_the_city_center
% restaurant 6 -> cheap
% restaurant(name,                        2,        3,      4,          5,         6 , , ).
restaurant(rfuzzy_default_values, 1,        0,       null,      null,     ).
restaurant(kenzo,                            0.5,    null,    null,      1).
restaurant(burguer_king,                null,    null,    null,      null).
restaurant(pizza_jardin,                  null,    null,    null,      null).
restaurant(subway,                          null,    null,    null,      null).
restaurant(derroscas,                       null,    null,   null,      null).
restaurant(il_tempietto,                   null,     null,    null,      null).
restaurant(kono_pizza,                    null,    null,     null,      null).
restaurant(paellador,                       0.87,    null,   null,      null).
restaurant(tapasbar,                        null,     null,   null,      null).
restaurant(meson_del_jamon,        null,     null,   100,      null).
restaurant(museo_del_jamon,        null,     null,   150,      null).
restaurant(zalacain,                         null,     null,    null,      null).

expensive_restaurant(zalacain).


rfuzzy_db_value_for(traditional/1, restaurant, 2, rfuzzy_truth_value_type).
%rfuzzy_type_for('fuzzy_rule', traditional/1, [restaurant]).
% rfuzzy_default_value_for(traditional/1, 1).
% traditional(kenzo) value 0.5.
% traditional(paellador) value 0.87.

rfuzzy_db_value_for(low_distance/1, restaurant, 3, rfuzzy_truth_value_type).
% rfuzzy_type_for('fuzzy_rule', low_distance/1, [restaurant]).
% :- set_prop low_distance_function/1 => restaurant/1.
% rfuzzy_default_value_for(low_distance/1, 0).

rfuzzy_db_value_for(distance_to_the_city_center/2, restaurant, 4, rfuzzy_number_type).
% rfuzzy_type_for('crisp_rule', distance_to_the_city_center/2, [restaurant, ]).
%distance_to_the_city_center(meson_del_jamon, 100).
%distance_to_the_city_center(museo_del_jamon, 150).

near_function :# ([ (0, 1), (100, 1), (1000, 0.1) ]) .
rfuzzy_type_for('fuzzy_rule', near_the_city_center/1, [restaurant]).
rfuzzy_define_fuzzification(near_the_city_center/1, distance_to_the_city_center, near_function).
rfuzzy_db_value_for(near_the_city_center/1, restaurant, 5).
% near_the_city_center(burguer_king) value 1.

% before:
% :- set_prop cheap/1 => restaurant/1.
rfuzzy_type_for('fuzzy_rule', cheap/1, [restaurant/1]).
% before: 
% :- default(cheap/1, 0.5).
rfuzzy_default_value_for(cheap/1, 0.5).
rfuzzy_default_value_for(cheap/1, 0.2) if thershold(near_the_city_center/1, over, 0.7).
rfuzzy_default_value_for(cheap/1, 0.1) if expensive_restaurant/1.

cheap(kenzo) value 0.3.
cheap(subway) value 1.
cheap(derroscas) value 1.

rfuzzy_synonym(cheap/1, unexpensive/1, prod, 1).
rfuzzy_antonym(cheap/1, expensive/1, prod, 1).

rfuzzy_quantifier(very/1, over, 0.7).

rfuzzy_type_for('fuzzy_rule', tempting_restaurant/1, [restaurant/1]).
rfuzzy_default_value_for(tempting_restaurant/1, 0.1).
tempting_restaurant(R) cred (min, 0.7) :~ min((low_distance(R), fnot(very(expensive(R))), traditional(R))).
tempting_restaurant(R) cred (min, 0.5) :~ low_distance(R).

distance_to_us(kenzo, 150).
distance_to_us(burguer_king, 500).
distance_to_us(il_tempietto, 100).
distance_to_us(pizza_jardin, 250).
% distance_to(unknown, 800).

rfuzzy_define_fuzzification(near_to_us/1, distance_to_us/2, near_function/2).

max_with_min_a_half(X, Y, Z) :- max(X, Y, W), min(W, 0.5, Z).
rfuzzy_aggregator(max_with_min_a_half/3).

adequate_restaurant(R) :~ fnot(very(expensive(R))).
rfuzzy_type_for('fuzzy_rule', preferred_restaurant/2, [restaurant/1, restaurant/1]).
