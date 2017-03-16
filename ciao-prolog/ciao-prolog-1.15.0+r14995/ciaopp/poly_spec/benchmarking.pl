:- module(benchmarking,
	[
	    calc_k/1,
	    do_bench/2,
	    do_dummy/2,
	    global_time_ellapsed/3       
	],[assertions, isomodes, regtypes]).

:- doc(title,"Benchmarking of solutions").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module implements benchmarking of the
	different solutions, running them several iterations if
	time-efficiency is wanted.").


:- use_module(filenames_pcpe).
:- use_module(db_pcpe, [time_efficiency/0, rt_iterations/1]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(prolog_sys), [statistics/2]). 
:- use_module(library(filenames), [no_path_file_name/2, basename/2]).
:- use_module(library(compiler), [use_module/1]).

:- pred global_time_ellapsed(+T1,+T2,-Diff) # "Estimates the global
	time ellapsed @var{Diff} between a time @var{T1} and a time
	@var{T2}".

global_time_ellapsed(GT_After,GT_Before,TE):-
	TE is round(1000*(GT_After - GT_Before))/1000.


:- regtype rtmod/1.
rtmod([A]) :-
        atom(A) .

:- pred do_bench(RT, Time) 
	: rtmod *  term 
	=> rtmod *  number 
        # "Runs the benchmark @var{RT} for @var{Count} iterations.
	@var{Time} is the net time (in msecs) taken by each iteration
	running the benchmark".

do_bench(RT,Time) :-
	statistics(runtime,[GT1,_]),
	use_module(RT),  
	no_path_file_name(RT,RT1),
	basename(RT1,Bench),
	push_prolog_flag(gc,off),
	\+ \+ (Bench:pcpe_rt_test->true;true),
	statistics(runtime,[GT2,_]),
	global_time_ellapsed(GT2,GT1,Time),
	pop_prolog_flag(gc).

:- pred do_dummy(RT, Time) 
	: rtmod *  term 
	=> rtmod *  number 
        # "Runs the a dummy benchmark @var{RT} for @var{Count} iterations.
	@var{Time} is the net time (in msecs) taken by each iteration
	running the benchmark".

do_dummy(RT,Time) :-
	statistics(runtime,[GT1,_]),
	use_module(RT),  
	no_path_file_name(RT,RT1),
	basename(RT1,Bench),
	push_prolog_flag(gc,off),
	\+ \+ (Bench:pcpe_dummy_test->true;true),
	statistics(runtime,[GT2,_]),
	global_time_ellapsed(GT2,GT1,Time),
	pop_prolog_flag(gc).


initial_iterations(100).


:- pred calc_k(K) : term => int # "Estimates number of iterations @var{K} to
	be run based on the original prog".

calc_k(1):-current_pp_flag(poly_fitness,memory),!.
calc_k(K):-
	time_efficiency, !,
	push_prolog_flag(gc,off),
	initial_iterations(I),
	estimate_iterations(I,K1),
	estimate_iterations(K1,K), %% twice for higher precision
	pop_prolog_flag(gc).
calc_k(0).


estimate_iterations(Curr,K):-
	adjust_iterations(Curr),
	get_src_name(Orig),
	create_driver_file(Orig,RT),
	do_bench(RT,T),
	desired_time(D),
	(T < 10 ->
	    NC is Curr * 10,
	    estimate_iterations(NC,K)
	;
	    (T > D ->
	        NC is integer(Curr / 2),
	        estimate_iterations(NC,K)
	    ;
		K is integer((D * Curr)/ T))).

adjust_iterations(It):-
	retractall_fact(rt_iterations(_)),
	asserta_fact(rt_iterations(It)).



:- pred desired_time(-) # "Time in msecs to be spent per bench".

desired_time(D):-
	current_pp_flag(pcpe_evaltime,D).
