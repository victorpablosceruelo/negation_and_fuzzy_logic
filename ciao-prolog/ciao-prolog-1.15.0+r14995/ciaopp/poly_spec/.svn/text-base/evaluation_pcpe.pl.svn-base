:- module(evaluation_pcpe,
	[
	    eval_solutions/3, 
	    eval_solution/2, 
	    eval_int_confs/3,
	    eval_conf/2,
	    eval_orig_code/1
	],[]).


:- use_package(assertions).
:- use_package(.(notime_stats)).

:- doc(title,"Evaluation of solutions").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module evaluates configurations (both final
	and non-final) using the corresponding fitness function, and
	choosing the best (or bests) solution(s) of all.").



:- use_module(stats_pcpe, [get_bests/2]).
:- use_module(db_pcpe).
:- use_module(breadth_first_spec, [bf_spec/7]).
:- use_module(common_spec, [initial_id/2, filter_covered/3]).
:- use_module(fitness_pcpe).
:- use_module(profiling_pcpe, 
	[ profile_conf/1 ]).
:- use_module(filenames_pcpe, 
	[ get_best_solution_file_name/2, get_output_file_name/2 ]).
:- use_module(benchmarking, 
	[global_time_ellapsed/3]).
:- use_module(codegen_pcpe, 
	[ print_intermediate_configuration/3, print_solution/4 ]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(spec(mem_usage), 
	[reset_mem_usage/0, ask_mem_usage/2]).
:- use_module(library(prolog_sys), [statistics/2]). 
:- use_module(library(sort), [sort/2]). 
:- use_module(library(write)).
:- use_module(library(system), [copy_file/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred eval_orig_code(-Val) #"Evaluates the original program, based
	on the current fitness function. If value is not needed then
	it just assigns 1 as value".

eval_orig_code(Val):-
	(bnb ->
	    current_pp_flag(poly_fitness, Fit),
	    original_score(Fit,Score),
	    peel_off_value(Score,Val)
	 ;
	    Val=1).

:- push_prolog_flag(multi_arity_warnings,off).

:- pred peel_off_value(+Score,-Val) #"Given a score of the current
	configuration, it get rids of the functor of the score and
	returns the current value".

peel_off_value(Score,Val):-
	current_pp_flag(poly_fitness, Fit),	
	peel_off_value(Fit,Score,Val).

peel_off_value(bytecode,Score,Val) :-
	member(size(Val),Score).
peel_off_value(speedup,Score,Val) :-
	member(exec_time(Val),Score),
	(Val < 0 -> write('Score lower than 0:'),write(Score),nl; true).
peel_off_value(memory,Score,Val) :-
	member(mem(Val),Score).
peel_off_value(bounded_size,Score,Val):-
	peel_off_value(speedup,Score,Val).
peel_off_value(balance,Score,Bal) :-
	member(size(Red),Score),
	member(exec_time(Sp),Score),
	balance(Sp,Red,Bal).

:- pop_prolog_flag(multi_arity_warnings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pred eval_int_confs(+Confs,+,-) # "If a configuration in @var{Confs} is
         a solution, then it leaves it current value. Otherwise, it
         closes the configuration and evaluates it".

eval_int_confs([],_Ref,[]).
eval_int_confs([e([],V,Value)|Confs],Ref,[e([],V,Value)|Evaluated_Confs]):-
	eval_int_confs(Confs,Ref,Evaluated_Confs).
eval_int_confs([Conf|Confs],Ref,[Closed_Conf|Evaluated_Confs]):-
	close_and_eval(Conf,Closed_Conf),
	eval_int_confs(Confs,Ref,Evaluated_Confs).

:- pred close_and_eval(+Conf,-) # "Closes a given configuration
	  @var{Conf} by calling pcpe with @tt{dyn} as global control
	  and @tt{inst} as a local control rule. Then evaluates to
	  obtain a fitness value, and returns the original
	  configuration with such a fitness value".

close_and_eval(e(TV,V,Val),e(TV,V,Fit)):-
	close_conf(e(TV,V,Val),_Atoms,Closed_Conf),
	eval_conf(Closed_Conf,Fit).

:- pred close_conf(+Conf,-,-) # "Closes a given configuration
	  @var{Conf} by calling pcpe with @tt{dyn} as globa control
	  and @tt{inst} as a local control rule".

close_conf(e([(A,_)|T],V,Val),NC,Closed_Conf):-	
	filter_covered([A|T],V,NC),
	initial_id(NC,NTV),
	G=[dyn],
	U=[[local_control(inst)]],
	Int = intermediate,
	bf_spec([e(NTV,V,Val)],G,U,Int,[],[Closed_Conf],tree(1,_)).





:- push_prolog_flag(multi_arity_warnings,off).

:- pred eval_conf(+Conf,-Val) # "Evaluates a given
	configuration @var{Conf}, using a selected @tt{strategy}".

eval_conf(Conf,Val):-
	statistics(runtime,[_GT,_]),
	current_pp_flag(poly_strategy,Strat),
	current_pp_flag(poly_fitness,Fitness),
	eval_conf(Strat,Fitness,Conf,Val),
	statistics(runtime,[_,T_e]),
	increment_time(int_eval,T_e).

eval_conf(all_sols,Fit,Conf,Val):-
	(bnb ->
	    bb_eval(Fit,Conf,Val)
	;
	    print_intermediate_configuration(Conf,none,[]),
	    score(Fit,'tmp',Score),
	    peel_off_value(Score,Val)).
eval_conf(oracle,_,_,1).

:- pop_prolog_flag(multi_arity_warnings).

:- pred bb_eval(+Fit, +Conf, -Val) # "It performs a branch and
	bound evaluation of a given configuration @var{Conf}, based on
	a given fitness function @var{Fit}. For the @tt{speedup}
	fitness function makes use of a profiler in order to estimate
	the time of intermediate configurations".

bb_eval(bytecode,e(Atoms,V,_),Val):-
	Atoms=[(A,_)|T],
	print_intermediate_configuration(e([],V,1),imports,[A|T]),
	score(bytecode,'tmp',Score),
	peel_off_value(Score,Val).
bb_eval(memory,e(Atoms,V,_),Val):-
	Atoms=[(A,_)|T],
	print_intermediate_configuration(e([],V,1),imports,[A|T]),
	score(memory,'tmp',Score),
	peel_off_value(Score,Val).
bb_eval(speedup,Conf,Val):-
	close_conf(Conf,Atoms,Closed_Conf),
	print_intermediate_configuration(Closed_Conf,prof,Atoms),
	profile_conf(Val).
bb_eval(bounded_size,Conf,Val):-
	bb_eval(bytecode,Conf,Size),
	bound_residual(Max),
	(Size < Max ->
	    current_fact(best_value(Ref)),
	    Val is Ref * 2 %Val > Ref, thus it will be pruned
	 ;
	    bb_eval(speedup,Conf,Val)).
bb_eval(balance,Conf,Val):-
	Conf=e([(A,_)|T],V,_),
	print_intermediate_configuration(e([],V,1),imports,[A|T]),
	score(bytecode,'tmp',Score),
	close_conf(Conf,Atoms,Closed_Conf),
	print_intermediate_configuration(Closed_Conf,prof,Atoms),
	profile_conf(Speed),
	peel_off_value([exec_time(Speed)|Score],Val).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pred eval_solution(+Sol,-Val) #"Evaluates solution @var{Sol} and
	returns its current score @var{Val}.".

eval_solution(Sol,Val):- 
	current_pp_flag(poly_fitness,Fit),
	print_solution([Sol],1,1,_),
	score(Fit,1,Score),
	peel_off_value(Score,Val).



:- pred eval_solutions(+Counter,+Sols,-Info) # "Evaluates generated
	solutions (final configurations). @var{Counter} is used to
	generate the names of the files corresponding to each
	solution. Some information @var{Info} about the evaluation is
	returned upon exit".

%eval_solutions(_,[]):-!.
eval_solutions(1,_,[total_time(0),memory(0)]):-
	\+ oracle_calibration(_),!,
	write_solution(1),
	inform_user(['{No evaluation is needed (only one solution found).}']).
eval_solutions(Counter,solutions(_),[best(Best),fits(EvalInfo),Time,Mem]):-
	statistics(runtime,[GT1,_]),
	reset_mem_usage,
	catch(eval_all_solutions(Counter,EvalInfo),E,inform_user(E)),
        get_bests(EvalInfo,St),
	St=(_,_,_,bests([(Best,_Stats1)|_])),
	inform_user(['{PCPE found ',Counter,' solutions. Best candidate is ',Best,'}']),
%	member((Best,Stats2),Sols),
%	append(Stats1,Stats2,BStats),
	write_solution(Best),
	statistics(runtime,[GT2,_]),
	global_time_ellapsed(GT2,GT1,TT),
	Time = total_time(TT),
 	ask_mem_usage(Delta,_Details),
	Mem = memory(Delta),
	inform_user(['{evaluated by PCPE in ', TT, ' msec.}']).


write_solution(Best):-
	get_output_file_name(Best,F1),
	get_best_solution_file_name(Best,F2),
	copy_file(F1,F2,[overwrite]),
	inform_user(['{written file ',F2,'}']).


eval_all_solutions(Counter,Info):-
	eval_programs(Counter,Scores),
	all_fitness(Scores,Fitness),	
	sort(Fitness,Info).

eval_programs(0,[]):-!.
eval_programs(Counter,[(Counter,Info)|T]):-
	Counter > 0,
	current_pp_flag(poly_fitness,Fit),
	score(Fit,Counter,Info),
	Counter1 is Counter - 1, 
	eval_programs(Counter1,T).

/*
merge_info([],[],[]).
merge_info([(C,I1)|T1],[(C,I2)|T2],[(C,I3)|T3]):-
	append(I1,I2,I3),
	merge_info(T1,T2,T3).
*/
