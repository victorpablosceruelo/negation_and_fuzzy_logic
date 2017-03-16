:- module(db_shopping, _, [rfuzzy, clpr]).

% :- activate_rfuzzy_debug.

% Database declaration
define_database(car/17, 
	[(id, rfuzzy_enum_type), 
	  (manufacturer, rfuzzy_enum_type),
	   (class,  rfuzzy_enum_type),
	    (year_of_production_start, rfuzzy_integer_type),
	     (year_of_production_end, rfuzzy_integer_type), 
	      (car_length_in_mm, rfuzzy_integer_type),
	       (car_width_in_mm, rfuzzy_integer_type),
		(car_height_in_mm, rfuzzy_integer_type),
		 (horsepower_in_kw, rfuzzy_integer_type),
		  (acceleration_to_100kph_in_s, rfuzzy_integer_type),
		   (max_speed_in_kph, rfuzzy_integer_type),
		    (number_of_cylinders, rfuzzy_integer_type),
		     (number_of_valves, rfuzzy_integer_type),
		      (fuel_tank_capacity_in_litres, rfuzzy_integer_type),
		       (fuel_type, rfuzzy_enum_type),
			(consumption_in_litres_each_100_km, rfuzzy_integer_type),
			 (price_in_euros, rfuzzy_integer_type)]).

% We define the individuals in our database.
car('Fiat Cinquecento', 'Fiat Auto Poland', 'City Car',                     1991, 1998,  3230, 1490, 1435,    40,    18,  140, 4,  null, 35,   g, 5.9,         null).
car('VW Caddy', 'Volkswagen Commercial Vehicles', 'Work Van', 1980, null,    4405, 1794, 1833,    80, 10.3, 110, 4,   null, 60,   g, 46.3,   45000).
car('Alfa Romeo GT', 'Alfa Romeo', 'Sports Car',                            2003, 2010, 4489, 1763, 1362,  237, 10.6, 130, 6,   null, 63,   g, 35,      30000).
car('Aston Martin Bulldog', 'Aston Martin', 'Sports Car',                  1980, null,    4376, 1698, 1100,  522, 12,    191, 8,   null, 113, g, 30,    150000).

car('Ford Fiesta', 'Ford Motor Company', 'City Car',                      2008, null,      3950, 1722, 1481,   96, 13.9, 166, 4,   16,     45, d, 6.5,    12000).
car('Ford Probe', 'Ford Motor Company', 'Sports Coupe',               1989, 1997,  4496, 1735, 1316,  165,   8,   220, 6,   24,     60, g, 9.6,    20000).
car('Ford S Max 1.8 TDCi', 'Ford Motor Company', 'Large MPV',  2006, 2010,   4768, 1884, 1658,  125, 11.6, 190, 4,   8,     70, d, 6.2,     30000).

car('Peugeot 207CC', 'Peugeot', 'Supermini',                                2006, 2012,    4045, 1748, 1472,  156,  8.5, 220, 4,   16,     50, g, 7.4,    21000).
car('Peugeot 306', 'Peugeot', 'Family Car',                                    1993, 2002,    4030, 1680, 1380,   90,   13, 182, 4,    8,     60, d, 6.2,      15000).

car('Audi TT', 'Audi AG', 'Sports Car',                                             1998, null,     4178, 1735, 1352,  200,  6.6, 240, 4,   16,     55, g, 7.7,    40000).
car('Audi TT Quattro Sport', 'Audi AG', 'Sports Car',                      2006, null,     4178, 1842, 1352,  250,  5.7, 250, 6,   24,     60, g, 9.4,    48000).

car('Mini Cooper', 'British Motor Corporation', 'Economy Car',       2009, null,    3699, 1683, 1414,  120,  9.8, 198, 4,   16,     40, g, 5.7,    24000).


%  vw_phaeton.
% ferrari

expensive(car) :~ defaults_to(0).
expensive(car) :~ function(price_in_euros(car), [ (0, 0), (10000, 0.5), (30000, 1), (1000000, 1) ]).
expensive(car) :~ function(price_in_euros(car), [ (0, 1), (10000, 0.78), (30000, 0.38), (1000000, 0) ]) only_for_user 'victorpablosceruelo_at_gmail_com'.
fast(car) :~ defaults_to(0).
fast(car) :~ function(max_speed_in_kph(car), [ (0, 0), (60, 0), (100, 0.4), (150, 0.75), (200, 1), (1000, 1) ]).
large(car) :~ defaults_to(0).
large(car) :~ function(car_length_in_mm(car), [ (0, 0), (3000, 0.2), (4000, 0.5), (5000, 1), (10000, 1) ]).
wide(car) :~ defaults_to(0).
wide(car) :~ function(car_width_in_mm(car), [ (0, 0), (1400, 0.2), (1500, 0.5), (1700, 1), (3000, 1) ]).
old(car) :~ defaults_to(0).
old(car) :~ function(year_of_production_end(car), [ (1900, 1), (2000, 1), (2005, 0.5), (2010, 0.1), (2015, 0) ]).


big(car) :~ defaults_to(0).
big(car) :~ rule(min, (large(car), wide(car))).

small(car) :~ antonym_of(big(car), prod, 1).
cheap(car) :~ antonym_of(expensive(car), prod, 1).

unexpensive(car) :~ synonym_of(cheap(car), prod, 1).
familiar(car) :~ synonym_of(big(car), prod, 1).

define_modifier(rather/2, TV_In, TV_Out) :-
	TV_Out .=. TV_In * TV_In.
define_modifier(very/2, TV_In, TV_Out) :-
	TV_Out .=. TV_In * TV_In * TV_In.
define_modifier(little/2, TV_In, TV_Out) :-
	TV_Out * TV_Out .=. TV_In.
define_modifier(very_little/2, TV_In, TV_Out) :-
	TV_Out * TV_Out * TV_Out .=. TV_In.

define_negation_op(godel_neg/2, TV_In, TV_Out) :-
	((TV_In .=. 0, TV_Out .=. 1) ; (\+(TV_In .=. 0), TV_Out .=. 0)).

define_connective(max_with_min_a_half/3, TV_In_1, TV_In_2, TV_Out) :-
	max(TV_In_1, TV_In_2, TV_Aux), min(TV_Aux, 0.5, TV_Out).
