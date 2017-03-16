:- module(ex_human_development,_,[rfuzzy, clpr]).

define_database(country/4, 
	[(id, rfuzzy_string_type), 
	  (living_standard, rfuzzy_truth_value_type), 
	   (literacy_rate, rfuzzy_truth_value_type),
	    (long_life, rfuzzy_truth_value_type)]).

country(japan,      0.95,  null,  0.82).
country(spain,      0.9,    0.97, 0.79).
country(china,      0.65,  0.9,   null).
country(australia, 0.95,  null,  0.8).
country(cuba,       null,   0.96, 0.77).
country(brasil,      0.63,  0.88, null).
country(egypt,      null,   0.55, null). 
country(kenya,     0.13,  0.73, 0.48).
country(senegal,  0.25,  0.39, 0.59).

living_standard(country) :~ defaults_to(0.48).
literacy_rate(country) :~ defaults_to(0.99).
long_life(country) :~ defaults_to(0.72).

human_development(country) :~ rule(prod, (literacy_rate(country),long_life(country),living_standard(country))).

