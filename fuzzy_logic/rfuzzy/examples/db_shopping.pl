:- module(db_shopping, _, [rfuzzy, clpr, pkgs_output_debug]).

% Database declaration
rfuzzy_define_database(car/11, 
	[(id, rfuzzy_enum_type), 
	  (manufacturer, rfuzzy_enum_type),
	   (class,  rfuzzy_enum_type),
	    (year_of_production_start, rfuzzy_integer_type),
	     (year_of_production_end, rfuzzy_integer_type), 
	      (car_length, rfuzzy_integer_type),
	       (car_width, rfuzzy_integer_type),
		(height, rfuzzy_integer_type),
		 (horsepower_in_kw, rfuzzy_integer_type),
		  (max_speed_in_mph, rfuzzy_integer_type),
		   (price_in_euros, rfuzzy_integer_type)]).

% We define the individuals in our database.
car('Fiat Cinquecento', 'Fiat Auto Poland', 'City Car',                     1991, 1998, 3230, 1490, 1435, 31, 93, null).
car('VW Caddy', 'Volkswagen Commercial Vehicles', 'Work Van', 1980, null,   4405, 1794, 1833, 80, 110, 45000).
car('Alfa Romeo GT', 'Alfa Romeo', 'Sport Car',                            2003, 2010, 4489, 1763, 1362, 237, 130, 30000).
car('Aston Martin Bulldog', 'Aston Martin', 'Sport Car',                  1980, null,    4376, 1698, 1100, 522, 191, 150000).
car('Ford Fiesta', 'Ford Motor Company', 'City Car',                      1976, null,    3924, 1685, 1468, 73, 114, 12000).

%  vw_phaeton.
% ferrari

expensive(car) :~ function(price_in_euros(car), [ (0, 0), (10000, 0.5), (30000, 1), (1000000, 1) ]).
fast(car) :~ function(max_speed_in_mph(car), [ (0, 0), (100, 0.5), (150, 1), (1000, 1) ]).
large(car) :~ function(car_length(car), [ (0, 0), (100, 0.5), (150, 1), (1000, 1) ]).
wide(car) :~ function(car_width(car), [ (0, 0), (100, 0.5), (150, 1), (1000, 1) ]).
big(car) :~ rule(min, (large(car), wide(car))).