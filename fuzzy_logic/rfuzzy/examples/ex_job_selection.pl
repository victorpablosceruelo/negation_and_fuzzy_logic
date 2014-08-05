:- module(ex_job_selection,_,[rfuzzy, clpr]).

define_database(position/5, 
	[(id, rfuzzy_string_type), 
	  (interest, rfuzzy_truth_value_type), 
	   (distance, rfuzzy_truth_value_type),
	    (salary, rfuzzy_truth_value_type),
	     (future_development, rfuzzy_truth_value_type)]).

position(consultant,              0.6,   0.4,  0.8,  0.5).
position(systems_analyst,   0.8,   0.1,   0.9,  0.3).
position(developer,              0.6,   0.5,   0.6,  0.8).
position(programmer,          0.4,   0.5,   0.5,  0.7).
position(teacher,                  0.4 ,  0.85, 0.3,  0.5).

interest(position) :~ defaults_to(0.1).
distance(position) :~ defaults_to(0.1).
salary(position) :~ defaults_to(0.1).
future_development(position) :~ defaults_to(0.1).

job_offer(position) :~ rule(prod, (interest(position), distance(position), salary(position), future_development(position))) with_credibility (min, 0.8).
