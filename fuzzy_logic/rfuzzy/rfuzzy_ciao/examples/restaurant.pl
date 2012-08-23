:- module(restaurant,_,[rfuzzy, pkgs_output_debug, clpr]).
% or clpq. We can use clpr or clpq.
% debug with pkgs_output_debug, 

% Define the file where we want debug msgs.
% :- define_pkgs_output_debug_file('~/secured/negation_and_fuzzy_logic/fuzzy_logic/rfuzzy/rfuzzy_ciao/debug_restaurant.pl').

:- prop restaurant/1.

:- set_prop preferred_restaurant/2 => restaurant/1, restaurant/1.

:- set_prop tempting_restaurant/1 => restaurant/1.
:- default(tempting_restaurant/1, 0.1).
tempting_restaurant(R) cred (min, 0.7) :~ min((low_distance(R), cheap(R), traditional(R))).

restaurant(kenzo).
restaurant(burguer_king).
restaurant(pizza_jardin).
restaurant(subway).
restaurant(derroscas).
restaurant(il_tempietto).
restaurant(kono_pizza).
restaurant(paellador).
restaurant(tapasbar).
restaurant(meson_del_jamon).
restaurant(museo_del_jamon).

% :- set_prop low_distance_function/1 => restaurant/1.
:- default(low_distance_function/1, 0).
near_function :# ([ (0, 1), (200, 1), (1000, 0.1) ]) .

distance_to_us(kenzo, 150).
distance_to_us(burguer_king, 500).
distance_to_us(il_tempietto, 100).
distance_to_us(pizza_jardin, 250).
% distance_to(unknown, 800).

rfuzzy_define_fuzzification(near_to_us/2, distance_to/2, near_function/2).

%low_distance(kenzo) value 1.
%low_distance(burguer_king) value 0.6.
%low_distance(il_tempietto) value 1.
%low_distance(pizza_jardin) value 0.8.
%low_distance(unknown) value 0.2.

% before:
% :- set_prop cheap/1 => restaurant/1.
rfuzzy_types_for(cheap/1, [restaurant/1]).
% before: 
% :- default(cheap/1, 0.5).
rfuzzy_default_value_for(cheap/1, 0.5).

cheap(kenzo) value 0.3.
cheap(subway) value 1.
cheap(derroscas) value 1.

rfuzzy_sinonym(cheap, unexpensive, 1).
rfuzzy_antonym(cheap, expensive, 1).

:- set_prop traditional/1 => restaurant/1.
:- default(traditional/1, 1).

traditional(kenzo) value 0.5.
traditional(paellador) value 0.87.

rfuzzy_quantifier(very, 0.7, [cheap/1]).