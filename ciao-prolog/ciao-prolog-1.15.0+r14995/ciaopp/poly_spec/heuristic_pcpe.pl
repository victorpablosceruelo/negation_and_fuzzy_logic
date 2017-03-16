:- module(heuristic_pcpe,
	[ 
	    query/1,
	    init_fixpoint/0,
	    cleanup_fixpoint/0,
	    get_all_solutions/1
	],
	[]).

:- use_package(assertions).

:- doc(title,"Poly-Controlled Partial Evaluation based on Heuristics").

:- doc(author, "Germ@'{a}n Puebla").
:- doc(author, "Claudio Ochoa").

:- doc(module," This module contains the implementation of a
	poly-controlled partial evaluation algorithm. It generates
	sets of candidate specializations. It allows the use of
	fitness fuctions to be applied to non-final states. This does
	not guarantee that the final solution obtained is optimal.").

:- use_package(hiord).

:- use_module(customization).
:- use_module(oracle_calibration, [init_history/0]).
:- use_module(common_spec, [initial_id/2]).
:- use_module(db_pcpe).
:- use_module(benchmarking, [calc_k/1]).
:- use_module(breadth_first_spec, [bf_spec/7]).
:- use_module(depth_first_spec, [df_spec/9]).
:- use_module(search_tree_pcpe).
:- use_module(evaluation_pcpe, [eval_orig_code/1]).
:- use_module(ciaopp(preprocess_flags), 
	[ current_pp_flag/2, set_pp_flag/2 ]).
:- use_module(api(api_direc_assrt), [get_directive/1]).
:- use_module(library(aggregates), [findall/3]).

:- true pred init_fixpoint # "Initializes the DB".

init_fixpoint:-
	cleanup_fixpoint.

:- true pred cleanup_fixpoint # "Cleans up the DB".

cleanup_fixpoint:-
	clean_up_db.

:- true pred get_all_solutions(A) : term(A) => list(A) # "Returns a
	list @var{A} containing all solutions found by PCPE".


get_all_solutions(Solutions):-
	current_fact(all_solutions(Solutions)),!.
get_all_solutions([]).

:- true pred query(+Queries) : list(Queries) # "Takes a list of
	initial @var{Queries}, and performs poly-controlled partial
	evaluation on such queries".

query([]):-!.
query(Queries):-
	search_type(Search),
	Search \== nil,!,
	get_all_options(G,U,D_Lim),!,
	assert_exported(Queries),
	eval_orig_code(Ref), 
	initial_id(Queries,QL),
	(Search == df ->
	     set_default_depth,
	     df_spec([e(QL,[],Ref)],G,U,1,D_Lim,no,[],Sols,tree(1,Tree))
	;
	     bf_spec([e(QL,[],Ref)],G,U,no,[],Sols,tree(1,Tree))),
	(current_pp_flag(output_info,high) ->  gen_tree(Tree); true), %	     check_equivs(Solutions))
	asserta_fact(all_solutions(Sols)),!.
query(_).

:- true pred assert_exported(+L) : list(L) #"Asserts all predicates in
        @var{L}. These are the exported predicates, needed for later
        renaming".

assert_exported(L):-
	retractall_fact(exported(_)),
	assertall_exported(L).

assertall_exported([]).
assertall_exported([H|T]):-
	assertz_fact(exported(H)),
	assertall_exported(T).



:- pred search_type(-) #"Determines the type of search to be
	performed, based on the chosen search/pruning strategy. It
	also fix some parameters so they are compatible with the
	chosen strategy".

search_type(nil):-
	profiling,
	\+ profiling_engine,!,
	error(['{Current engine does not support profiling}']), 
	error(['{Please reconfigure Ciao enabling profiling.}']),  
	error(['{Run ciaosetup configure --help}']).
search_type(nil):-
	need_rtquery,
	\+ check_rtquery, !,
	error(['{Please add a directive :- pcpe_rtquery(Call,Iterations) in your source file.}']),
	error(['{This is needed to measure time efficiency.}']),  
	error(['{Check CiaoPP documentation for further details on this directive.)}']).
search_type(nil):-
	current_pp_flag(poly_fitness,bounded_size),
	\+ wellformed_size, !,
	error(['{There is an error in the syntax of parameter pcpc_bounded_size.}']),
	error(['{Valid format is N+[.N+][S] where N is a digit and S is a prefix in {K,G,M,X}.}']),  
	error(['{For example: 1000, 15K, 2M, 1.5G, 0.5X are valid formats}']).
search_type(nil):-
	current_pp_flag(poly_fitness,bounded_size),
	current_pp_flag(poly_strategy,oracle),!,
	error(['{Oracle cannot be used with bounded_size fitness function}']).
search_type(bf) :- 
	oracle_calibration(_),!,
	init_history,
	set_pp_flag(min_crit,equal),
	set_pp_flag(poly_pruning,heuristic),
	set_pp_flag(polyvar_pcpe,pred). % chtrees need to be computed
search_type(bf) :- 
	current_pp_flag(poly_strategy,oracle),!,
	set_pp_flag(min_crit,equal). % chtrees need to be computed
search_type(df).
	

:- pred set_default_depth #" Sets the depth at which configurations
	are to be closed if the current strategy is
	@tt{branch_and_bound}".

set_default_depth:-
	bnb,
	current_pp_flag(poly_depth_lim,D_Lim),
	(D_Lim =< 0 ->  % set default depth 
	    default_depth_lim(D),
	    set_pp_flag(poly_depth_lim,D) 
	;
	    true).
set_default_depth.


need_rtquery:- time_efficiency.
need_rtquery:- current_pp_flag(poly_fitness,memory).

:- pred check_rtquery #"If time efficiency is going to be evaluated,
	it checks whether runtime tests have been included in the
	source file. If yes, then it asserts them for later use, fails
	otherwise".

check_rtquery :-
	findall(Q,get_directive(direc(_,_,pcpe_rtquery(Q),_,_)),L),
	L \== [],!,
	assertall_rtqueries(L),
	calc_k(K),
	retractall_fact(rt_iterations(_)),	
	assertz_fact(rt_iterations(K)).

assertall_rtqueries([]).
assertall_rtqueries([Q|T]):-
	assertz_fact(rtquery(Q)),
	assertall_rtqueries(T).



:- pred wellformed_size #"Determines if the value of the flag
	@tt{pcpe_bounded_size} is well-formed and assertz the maximum
	allowed size of the residual program".


wellformed_size :-
	max_size(K),
	retractall_fact(bound_residual(_)),	
	assertz_fact(bound_residual(K)).
	
