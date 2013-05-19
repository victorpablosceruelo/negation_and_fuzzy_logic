:- module(db_relocation,_,[rfuzzy,clpr]).
% Compilation time debug can be activated  by adding to the packages list [rfuzzy, clpr] the package pkgs_output_debug.
% Running time debug can be activated removing the comment marker % at the beginning of the following line.
% :- activate_rfuzzy_debug.

% TYPE DECLARATION
rfuzzy_define_database(house/7, 
	[(code, rfuzzy_enum_type), 
	  (house_type, rfuzzy_enum_type), 
	   (size, rfuzzy_integer_type),
	    (rooms_number, rfuzzy_integer_type), 
	     (price, rfuzzy_integer_type), 
	      (distance_to_the_center, rfuzzy_integer_type), 
	       (distance_to_the_beach, rfuzzy_integer_type)]).

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

% FUZZY FUNCTIONS OVER QUANTITATIVE ATTRIBUTES USING INFO FROM THE DATABASE
rfuzzy_fuzzification(expensive(house), price(house)) :- function([(50000,0),(100000,0.1),(250000,0.2),(350000,0.3),(450000,0.5),(550000,0.6),(800000,0.7),(1000000,0.8),(1500000,0.9),(2500000,1)]).
rfuzzy_fuzzification(cheap(house), price(house)) :- function([(0,1),(30000,1),(50000,0.8),(100000,0.7),(250000,0.5),(350000,0.3),
	            (450000,0.1),(550000,0)]).
rfuzzy_fuzzification(big(house), size(house)) :- function([(0,0),(50,0.1),(80,0.2),(120,0.3),(200,0.4),(300,0.5),(500,0.7),(1000,0.8),(1500,0.9),(2500,1)]).
rfuzzy_fuzzification(small(house), size(house)) :- function([(0,1),(50,1),(80,0.9),(100,0.8),(150,0.7),(200,0.5),(300,0.2),(400,0.1),(500,0)]).
rfuzzy_fuzzification(close_to_center(house), distance_to_the_center(house)) :- function([(0,1),(2,1),(4,0.8),(7,0.6),(10,0.5),(12,0.3),(15,0.2),(20,0)]).
rfuzzy_fuzzification(far_from_center(house), distance_to_the_center(house)) :- function([(0,0),(7,0),(8,0.1),(10,0.3),(14,0.4),(20,0.7),(25,0.8),(30,1)]).
rfuzzy_fuzzification(close_to_beach(house), distance_to_the_beach(house)) :- function([(0,1),(100,1),(1000,0.5),(2000,0)]).

% CRISP FUNCTIONS
equal(X,X).
greater(X,Y):- X.>.Y.

% QUANTIFIERS
rfuzzy_quantifier(a_little/2, TV_In, TV_Out) :-
	TV_Out .=. TV_In / TV_In.

% Our main aggregator
rfuzzy_aggregator(special_prod/3, TV_In_1, TV_In_2, TV_Out) :-
	TV_Out .=. TV_In_1 * TV_In_2.

% Rules
rfuzzy_default_value_for(cheap_and_close_to_the_center(house),0.5).
cheap_and_close_to_the_center(house):~ special_prod((cheap(house), close_to_center(house))).

rfuzzy_default_value_for(not_very_expensive(house),0.5).
not_very_expensive(house):~ fnot(very(expensive(house))).

rfuzzy_default_value_for(big_and_very_close_to_the_beach(house),0.5).
big_and_very_close_to_the_beach(house):~ special_prod((big(house), very(close_to_beach(house)))).% missing crisp and not* parts 

rfuzzy_default_value_for(very_cheap_house(house),0.5).
very_cheap_house(house):~ prod((very(cheap(house)))). % 2 missing crisp parts

rfuzzy_default_value_for(close_to_the_center_and_the_beach(house),0.5).
close_to_the_center_and_the_beach(house):~ prod((close_to_center(house), close_to_beach(house))). % 2 missing crisp parts

% This will be changed in a future version of rfuzzy.
%rfuzzy_type_for(is_apartment/1, [house]).
%is_apartment(House) :- 
%	house(T1, T2, T3, T4, T5, T6, T7), 
%	House = house(T1, T2, T3, T4, T5, T6, T7), 
%	house_type(House, House_Type), 
%	House_Type = 'apartment'.
%rfuzzy_default_value_for(q4(house), 0) if is_apartment(house).


