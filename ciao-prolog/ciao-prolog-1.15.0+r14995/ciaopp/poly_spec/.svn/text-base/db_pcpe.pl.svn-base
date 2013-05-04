:- module(db_pcpe,
	[ 
	    clean_up_db/0,
	    max_id/1,
	    evals/1,
	    all_solutions/1,
	    processed/2,
	    exported/1,
	    predname/3,
	    oracle_calibration/1,
	    best_value/1,
	    time_efficiency/0,
	    bnb/0,
	    heuristic/0,
	    pred_consistency/0,
	    modes_consistency/0,
	    profiling/0,
	    profiling_engine/0,
	    rtquery/1,
	    rt_iterations/1,
	    bound_residual/1,
	    max_size/1,
	    increment_time/2,
	    ask_time/2,
	    increment_oracle/2,
	    oracle_stats/3	    
	],
	[]).

:- use_package(assertions).

:- doc(title,"Database of facts for PCPE").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module contains data that is asserted by
	PCPE. They are all together to facilitate the cleaning up of
	all data. It also contains predicates of common use by other
	modules.").

:- use_module(fitness_pcpe, [original_score/2]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(lists), [append/3]).

:- pred max_id(Id) #"Keeps track of ids assigned to each configuration".

:- data max_id/1.

:- pred evals(E) #"Keeps track of number of evaluations performed".

:- data evals/1.

:- pred best_value(B) #"Best value found so far (BnB)".

:- data best_value/1.

:- pred all_solutions(Sols) #"Solutions found in analysis".

:- data all_solutions/1.

:- pred processed(A,B) #"Already processed predicates".

:- data processed/2.

:- pred exported(B) #"Exported predicates".

:- data exported/1.

:- pred predname(A,B,C) #"Original names of predicates predicates".

:- data predname/3.

:- pred rtquery(A) #"Runtime queries".

:- data rtquery/1.


:- pred bound_residual(A) #"Maximum size of residual program".

:- data bound_residual/1.


:- pred rt_iterations(A) #"Amount of iterations for rtqueries".

:- data rt_iterations/1.


:- pred time(A,B) #"Time spent in a given stage of the algorithm".

:- data time/2.

:- data oracle_stats/3.


:- pred clean_up_db # "Retracts all facts in the DB".

clean_up_db:-
	retractall_fact(max_id(_)),
	retractall_fact(evals(_)),
	retractall_fact(best_value(_)),
	retractall_fact(processed(_,_)),
	retractall_fact(all_solutions(_)),
	retractall_fact(exported(_)),
	retractall_fact(rtquery(_)),
	retractall_fact(rt_iterations(_)),
	retractall_fact(predname(_,_,_)),
	retractall_fact(time(_,_)),
	retractall_fact(oracle_stats(_,_,_)).

:- pred bnb #"Succeeds when branch and bound pruning is to be performed".

bnb :-
	current_pp_flag(poly_strategy,all_sols),
	current_pp_flag(poly_pruning,bnb).
bnb :-
	current_pp_flag(poly_strategy,all_sols),
	current_pp_flag(poly_pruning,both).

:- pred heuristic #"Succeeds when heuristic pruning is to be
	performed".

heuristic :-
%	current_pp_flag(poly_strategy,all_sols),
	current_pp_flag(poly_pruning,heuristic).
heuristic :-
	current_pp_flag(poly_strategy,all_sols),
	current_pp_flag(poly_pruning,both).


:- pred pred_consistency #"Succeeds when predicate-consistency
	heuristics pruning is to be performed".

pred_consistency:-
	heuristic,
	current_pp_flag(polyvar_pcpe,pred).

:- pred modes_consistency #"Succeeds when modes-consistency
	heuristics pruning is to be performed".

modes_consistency:-
	heuristic,
	current_pp_flag(polyvar_pcpe,modes).



:- pred time_efficiency #"Succeeds when the fitness function measures
	time efficiency".

time_efficiency :- current_pp_flag(poly_fitness,speedup).
time_efficiency :- current_pp_flag(poly_fitness,balance).
time_efficiency :- current_pp_flag(poly_fitness,bounded_size).


:- pred profiling #"Succeeds when profiling is to be performed".

profiling:-
	current_pp_flag(poly_strategy,all_sols),
	current_pp_flag(poly_pruning,bnb),
	time_efficiency.


:- pred profiling_engine #"Succeeds if the current engine has been
	compiled for profiling".

profiling_engine :-
	get_debug(A),
	A == '-profile'.


:- pred ask_time(+Time, -Info)
	#"Returns the accumulated @var{Time} in @var{Info}".

ask_time(Time,T):-
	current_fact(time(Time,T)),!.
ask_time(_,0).



:- pred increment_time(+Time,+Info)
	#"Increments the current @var{Time} in @var{T_e} units".

increment_time(Time,T_e):-
	current_fact(time(Time,T_i)),!,
	NTime is T_i + T_e,
	retractall_fact(time(Time,_)),
	asserta_fact(time(Time,NTime)).
increment_time(Time,T_e):-
	asserta_fact(time(Time,T_e)).


:- pred max_size(+) #"Determines if the value of the flag
	@tt{pcpe_bounded_size} is well-formed".

max_size(Max):-
	current_pp_flag(pcpe_bounded_size,B),
	valid_input(B,Num,Code),
	determine_size(Code,Num,Max).

valid_input(B,Num,Code):-
	atom_codes(B,C),
	valid_rep(C,[],CN,Code),
	number_codes(Num,CN).


valid_rep([0'k],CN,CN,kb):-!. 
valid_rep([0'K],CN,CN,kb):-!.
valid_rep([0'm],CN,CN,mb):-!. 
valid_rep([0'M],CN,CN,mb):-!.
valid_rep([0'g],CN,CN,gb):-!. 
valid_rep([0'G],CN,CN,gb):-!.
valid_rep([0'x],CN,CN,x):-!. 
valid_rep([0'X],CN,CN,x):-!. 
valid_rep([D],T,CN,num):-!,
	digit(D),
	append(T,[D],CN).
valid_rep([C|Cs],T,CN,Code):-
	digit(C),
	append(T,[C],Temp),
	valid_rep(Cs,Temp,CN,Code).



determine_size(num,Num,Num).
determine_size(kb,Num,Max):-
	Max is Num * 1024.
determine_size(mb,Num,Max):-
	Max is Num * 1024 * 1024.
determine_size(gb,Num,Max):-
	Max is Num * 1024 * 1024 * 1024.
determine_size(x,Num,Max):-
	original_score(bytecode,[size(Orig_Size)]),
	Max is Num * Orig_Size.



digit(N):- N>=0'0, N=<0'9.
digit(0'.).



:- pred oracle_calibration(+) #"Set this predicate to true if the
	oracle is being calibrated, set it to false for normal
	behaviour. It takes as parameter the phase of the
	algorithm. The first phase writes the specialization history
	of all solutions, while the second uses that history to
	estimate the mfv of each configuration".

oracle_calibration(_):- fail.
%oracle_calibration(first).
%oracle_calibration(second).

:- pred increment_oracle(+Ties,+Children).

increment_oracle(Ties,Confs):-
	current_fact(oracle_stats(Depth,CT,CC)),!,
	ND is Depth + 1,
	NT is CT + Ties,
	NC is CC + Confs,
	retractall_fact(oracle_stats(Depth,_,_)),
	asserta_fact(oracle_stats(ND,NT,NC)).
increment_oracle(Ties,Confs):-
	asserta_fact(oracle_stats(2,Ties,Confs)).
