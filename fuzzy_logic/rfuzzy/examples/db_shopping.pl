:- module(db_shopping, _, [rfuzzy, clpr, pkgs_output_debug]).

% Database declaration
rfuzzy_define_database(car/11, 
	[(id, rfuzzy_enum_type), 
	  (manufacturer, rfuzzy_enum_type),
	   (class,  rfuzzy_enum_type),
	    (year_of_production_start, rfuzzy_integer_type),
	     (year_of_production_end, rfuzzy_integer_type), 
	      (car_length_in_mm, rfuzzy_integer_type),
	       (car_width_in_mm, rfuzzy_integer_type),
		(car_height_in_mm, rfuzzy_integer_type),
		 (horsepower_in_kw, rfuzzy_integer_type),
		  (acceleration_0_to_100_kph_in_s, rfuzzy_integer_type),
		   (max_speed_in_kph, rfuzzy_integer_type),
		    (number_of_cylinders, rfuzzy_integer_type),
		     (number_of_valves, rfuzzy_integer_type),
		      (fuel_tank_capacity_in_litres, rfuzzy_integer_type),
		       (consumption_in_litres_each_100_km, rfuzzy_integer_type),
			(price_in_euros, rfuzzy_integer_type)]).

% We define the individuals in our database.
car('Fiat Cinquecento', 'Fiat Auto Poland', 'City Car',                     1991, 1998,  3230, 1490, 1435,    40,    18,  140, 4,  null, 35, 5.9,         null).
car('VW Caddy', 'Volkswagen Commercial Vehicles', 'Work Van', 1980, null,    4405, 1794, 1833,    80, 10.3, 110, 4,   null, 60, 46.3,   45000).
car('Alfa Romeo GT', 'Alfa Romeo', 'Sports Car',                            2003, 2010, 4489, 1763, 1362,  237, 10.6, 130, 6,   null, 63, 35,      30000).
car('Aston Martin Bulldog', 'Aston Martin', 'Sports Car',                  1980, null,    4376, 1698, 1100,  522, 12,    191, 8,   null, 113, 30,    150000).

car('Ford Fiesta', 'Ford Motor Company', 'City Car',                      2008, null,      3950, 1722, 1481,   96, 13.9, 166, 4,   16,     45, 6.5,    12000).
car('Ford Probe', 'Ford Motor Company', 'Sports Coupe',               1989, 1997,  4496, 1735, 1316,  165,   8,   220, 6,   24,     60, 9.6,   20000).
car('Peugeot 207CC', 'Peugeot', 'Supermini',                                2006, 2012,    4045, 1748, 1472,  156,  8.5, 220, 4,   16,     50, 7.4,    21000).

car('Audi TT', 'Audi AG', 'Sports Car',                                             1998, null,     4178, 1735, 1352,  200,  6.6, 240, 4,   16,     55, 7.7,    40000).
car('Audi TT Coupe', 'Audi AG', 'Sports Coupe',                            1998, 2006,    4041, 1764, 1346,  222, 226, 45000).
car('Audi TT Quattro Sport', 'Audi AG', 'Sports Car',                      1998, null,     4141, 1842, 1346,   237, 250, 48000).

%  vw_phaeton.
% ferrari

expensive(car) :~ function(price_in_euros(car), [ (0, 0), (10000, 0.5), (30000, 1), (1000000, 1) ]).
fast(car) :~ function(max_speed_in_mph(car), [ (0, 0), (60, 0), (100, 0.4), (150, 0.75), (200, 1), (1000, 1) ]).
large(car) :~ function(car_length_in_mm(car), [ (0, 0), (3000, 0.2), (4000, 0.5), (5000, 1), (10000, 1) ]).
wide(car) :~ function(car_width_in_mm(car), [ (0, 0), (1400, 0.2), (1500, 0.5), (1700, 1), (3000, 1) ]).
big(car) :~ rule(min, (large(car), wide(car))).

