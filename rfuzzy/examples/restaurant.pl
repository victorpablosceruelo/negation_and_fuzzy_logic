:- module(restaurant,_,[rfuzzy, clpr, debugger_pkg]).

:- prop restaurant/1.

:- set_prop preferred_restaurant/2 => restaurant/1, restaurant/1.

:- set_prop tempting_restaurant/1 => restaurant/1.
:- default(tempting_restaurant/1, 0.1).
tempting_restaurant(R) :~ prod((low_distance(R), cheap(R), traditional(R))).

restaurant(kenzo).
restaurant(burguer_king).
restaurant(pizza_jardin).
restaurant(subway).
restaurant(derroscas).
restaurant(il_tempietto).
restaurant(kono_pizza).
restaurant(paellador).
restaurant(tapasbar).

% :- set_prop low_distance_function/1 => restaurant/1.
:- default(low_distance_function/1, 0).
low_distance_function :# ([ (0, 1), (200, 1), (1000, 0.1) ]) .

distance(kenzo, 150).
distance(burguer_king, 500).
distance(il_tempietto, 100).
distance(pizza_jardin, 250).
distance(unknown, 800).

fuzzify(low_distance/2, distance/2, low_distance_function/2).

%low_distance(kenzo) value 1.
%low_distance(burguer_king) value 0.6.
%low_distance(il_tempietto) value 1.
%low_distance(pizza_jardin) value 0.8.
%low_distance(unknown) value 0.2.

:- set_prop cheap/1 => restaurant/1.
:- default(cheap/1, 0.5).

cheap(kenzo) value 0.2.
cheap(subway) value 1.
cheap(derroscas) value 1.

:- set_prop traditional/1 => restaurant/1.
:- default(traditional/1, 1).

traditional(kenzo) value 0.5.
traditional(paellador) value 0.87.
