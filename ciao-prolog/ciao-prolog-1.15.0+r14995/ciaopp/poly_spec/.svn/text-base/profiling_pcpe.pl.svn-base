:- module(profiling_pcpe,
	[ 
	    profile_conf/1,
	    profiler_ready/2
	],[]).

:- use_package(assertions).
:- use_package(api(ciaopp_api)).

:- doc(title,"Integration of PCPE and profiler").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module integrates PCPE and the profiler,
	allowing to profile a given (intermediate) configuration and
	estimate a lower bound of the fitness value of it.").

:- use_module(filenames_pcpe).
:- use_module(db_pcpe, [predname/3]).
:- use_module(library(filenames), 
	[ no_path_file_name/2, basename/2 ]).
:- use_module(library(terms),    [atom_concat/2]).
:- use_module(library(compiler), [use_module/1]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(terms_check), [variant/2]).
:- use_module(library(profiler(profiler_utils))).
:- use_module(library(profiler(profiler_base))).


:- pred profile_conf(Time) : term => num # "Profiles the main
	predicate of the corresponding benchmark, and returns the
	@var{Time} for it".

profile_conf(Val):-
	profile_bench(Val, _).

:- pred profile_bench(-Info,-Pred) # "It profiles the current
	intermediate program, and returns the profiling @var{Info} and
	the name of the main predicate @var{Pred} being profiler".

profile_bench(Val, Pred) :-
	profile_reset,
	load_module(M),
	profile(M:pcpe_rt_test),
%	atom_concat([M,':$cc$pcpe_rt_test'],Pred),
	atom_concat([M,':pcpe_rt_test'],Pred),
	profile_info(Prof_Info),
%       get_profile_cc_summary_exectime(Pred/1, Prof_Info,Val).
	get_profile_cc_summary_data(Pred/1,time,Prof_Info,Val).
%	get_profile_flat_data(Pred/1,time,Prof_Info,Val).

:- pred load_module(-) # "Dynamically loads a given module".

load_module(Bench):-
	get_tmp_file_name(Tmp),
	basename(Tmp,Base),
	create_driver_file(Base,Drv),
	basename(Drv,RT),
	use_module(RT),
	no_path_file_name(RT,Bench).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred profiler_ready(+Atoms,-Dirs) #"Prepares the output program to
	be used with the profiler, adding as cost centers the
	predicates corresponding to atoms in @var{Atoms}.".

profiler_ready([],[]):-!.
profiler_ready(Atoms,Dirs):-
	filter_preds(Atoms,Preds),
	add_directives(Preds,Dirs).


:- pred add_directives(+Preds,-Dirs) : list(Preds) #"Adds a
	@tt{profile} directive for every predicate in
	@var{Preds}. Returns a list of directives @var{Dirs} so they
	can be later removed. ".

add_directives([],[]).
add_directives([MN/A|T],[cost_center(N/A)|R]):-
	atom_concat([_Mod,':',N],MN),
	add_directive(cost_center(N/A)),
	add_directives(T,R).

:- pred filter_preds(+Atoms,-Preds) # "Given a list @var{Atoms} of
	atoms of interest, it obtains the names @var{Preds} of the
	predicates corresponding to such atoms in the resulting
	program".

filter_preds(A,P):-
	filter_preds_(A,Tmp),
	sort(Tmp,Tmp_s),
	eliminate_ids(Tmp_s,P).

filter_preds_([],[]).
filter_preds_([A|T],[(Id,Pred)|R]):-
	functor(A,Name,Arity),
	functor(B,Name,Arity),
	current_fact(predname(Id,B,Pred)),
	variant(A,B),!,
	filter_preds_(T,R).
filter_preds_([_|T],R):- % Pred did not generate code
	filter_preds_(T,R).


eliminate_ids([],[]).
eliminate_ids([(_,P)|T],[P|R]):-
	eliminate_ids(T,R).
	

