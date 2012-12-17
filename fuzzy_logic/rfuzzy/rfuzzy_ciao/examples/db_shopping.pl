:- module(db_shopping, _, [rfuzzy, clpr]).
% Compilation time debug can be activated  by adding to the packages list [rfuzzy, clpr] the package pkgs_output_debug.
% Running time debug can be activated removing the comment marker % at the beginning of the following line.
% :- activate_rfuzzy_debug.

% TYPE DECLARATION
rfuzzy_define_database(car/11, 
	[(id, rfuzzy_enum_type), 
	  (manufacturer, rfuzzy_enum_type),
	   (class,  rfuzzy_enum_type),
	    (year_of_production_start, rfuzzy_integer_type),
	     (year_of_production_end, rfuzzy_integer_type), 
	      (length, rfuzzy_integer_type),
	       (width, rfuzzy_integer_type),
		(height, rfuzzy_integer_type),
		 (horsepower_in_kw, rfuzzy_integer_type),
		  (max_speed_in_mph, rfuzzy_integer_type),
		   (price_in_euros, rfuzzy_integer_type)]).

% We define the individuals belonging to th set car.
car('Fiat Cinquecento', 'Fiat Auto Poland', 'City Car', 1991, 1998, 3230, 1490, 1435, 31, 93, null).
car('VW Caddy', 'Volkswagen Commercial Vehicles', 'Work Van', 1980, null, 1794, 4405, 1833, 80, 110, 45000).
car('Alfa Romeo GT', 'Alfa Romeo', 'Sport Car', 2003, 2010, 4489, 1763, 1362, 237, 130, 30000).
car('Aston Martin Bulldog', 'Aston Martin', 'Sport Car', 1980, null, 4376, 1698, 1100, 522, 191, 150000).
car('Ford Fiesta', 'Ford Motor Company', 'City Car', 1976, null, 3924, 1685, 1468, 73, 114, 12000).
%  vw_phaeton.
% ferrari

rfuzzy_fuzzification(expensive(car), price_in_euros(car)) :- function([ (0, 0), (10000, 0.5), (30000, 1), (1000000, 1) ]).
% Future syntax
% expensive(car) :~ function(price_in_euros(car), [(0, 0), (10000, 0.5), (30000, 1)]).

rfuzzy_fuzzification(fast(car), max_speed_in_mph(car)) :- function([ (0, 0), (100, 0.5), (150, 1), (1000, 1) ]).