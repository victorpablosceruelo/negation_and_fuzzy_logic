:- module(oracle,
	[ 
	    oracle/2, 
	    assess/3
	],[]).

:- use_package(assertions).
:- use_package(.(notime_stats)).

:- doc(title,"Oracle-based Poly-Controlled Partial Deduction").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module contains the implementation of an
	oracle function, that assess intermediate configurations and
	chooses the best one based on local conditions").

:- use_module(db_pcpe). 
:- use_module(oracle_calibration).
:- use_module(qsort_confs, [qsort_confs/2]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(spec(sp_clauses), [orig_clause/3]).
:- use_module(library(lists), [reverse/2, length/2, last/2, append/3]).
:- use_module(library(aggregates), [findall/3]).

:- use_module(library(prolog_sys), [statistics/2]). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          
%                           LOCAL FLAGS

:- pred stats #"If on, it updates statistics such as ties by the
oracle, etc.".

stats:-fail.
%stats.


:- pred debug #"When on, it displays extra info on the console".

debug:-fail.
%debug.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred oracle(+,-) # "Given a list of configurations, it chooses one
	(or some) of them, plus any final configuration, and discards
	the rest. Configurations are chosen based on an assessing
	function that considers several parameters, such as unfolding
	steps, atoms remaining to be visited, clauses and literals
	already generated, etc, and greatly depends on the current
	fitness function being used".

oracle(Confs,Pick):-
	Confs = [e([1],_,_)|_],!,
	choose_best(Confs,Pick).
oracle(Confs,[Pick]):-
	oracle_calibration(second),!,
	follow_spec_history(Confs,GU),
	choose_by_hist(Confs,GU,1,_N,Pick),
	end_level.
oracle(Confs,PickedConfs):-
	statistics(runtime,[_GT,_]),
	oracle_(Confs,PickedConfs),
	statistics(runtime,[_,T_e]),
	increment_time(oracle,T_e).


oracle_([],[]).
oracle_([C],[C]).
oracle_(Confs,PickedConfs):-
	(debug -> display(Confs),nl ; true),
	filter_sols(Confs,NoSols,Sols),
	choose_bests(NoSols,Best),
	(stats ->
	    choose_bests(NoSols,Bests),
	    length(Bests,TT),
	    (TT \== 0 -> Ties is TT - 1; Ties = 0),   
	     length(Confs,Children)
	;
	    true),
 	increment_oracle(Ties,Children),
	append(Best,Sols,PickedConfs).


:- pred filter_sols(+Confs,-NoFinal,-Final) # "Splits a list of
	configurations @var{Confs} into non final and final
	configurations".

filter_sols([],[],[]).
filter_sols([e([],TV,Val)|T],NS,[e([],TV,Val)|Sols]):-!,
	filter_sols(T,NS,Sols).
filter_sols([Conf|T],[Conf|NS],Sols):-
	filter_sols(T,NS,Sols).

choose_best([],[]):-!.
choose_best(L,[Best]):-
	qsort_confs(L,SL),
	last(SL,Best).

choose_bests([],[]):-!.
choose_bests(L,Bests):-
	qsort_confs(L,SL),
	last(SL,e(_,_,Val)),
	reverse(SL,RL),
	filter_bests(RL,Val,Bests).


filter_bests([e(TV,V,Val)|T],Val,[e(TV,V,Val)|R]):-!,
	filter_bests(T,Val,R).
filter_bests(_,_,[]).

:- pred size_type(-Type) #"Determines metric used to measure size".

%size_type(count).
size_type(term).

:- pred assess(+Observables,+Write,-Q) #"The oracle assigns a quality
	value @var{Q} based on the statistics collected from a vector
	of observables @var{Observables}. These observables are
	written to a file depending on the value of the flag
	@var{Write}, for calibration of the oracle.".

:- push_prolog_flag(multi_arity_warnings,off).
assess(Observables,Write,Q):-
	statistics(runtime,[_,_]),
	current_pp_flag(poly_fitness,Fit),
	assess(Fit,Observables,Write,Q),
	statistics(runtime,[_,T_e]),
	increment_time(oracle,T_e).


assess(speedup,o(_,_,_,Unf_stats,_),_,Q):-
	(member(derivation_steps(Ds),Unf_stats), 
	 member(evaluations(Ev),Unf_stats),
	 member(nondet-nl(NdNl),Unf_stats) ->
	    Q is ( Ds + 1.3 * Ev) - NdNl
	;
	    Q = 0,
    	    error(['{In order to use oracle you need to enable the collect of unfolding stats.}'])).
assess(memory,Observables,Write,Q):-
	assess(bytecode,Observables,Write,Q).
assess(bytecode,o(A,History,Tree,Unf_stats,DeltaS),Write,Q):-
	(member(atoms(Atoms_count),Unf_stats) ->
	    size_type(Size_type),
	    orig_size(Size_type,[A],Size_orig),
	    size(Size_type,Tree,Size_new),
	    DSa is Size_new/Size_orig,
	    orig_size(Size_type,DeltaS,DSf),	    
	    (oracle_calibration(second), Write==write ->
	        write_conf(History,values([DSa,DSf,Atoms_count]))
	    ;   
		true),	    
	    Q is (-37.7 * Atoms_count) + (0.51 * DSa) + (-0.25 * DSf)	
	;
	    Q = 0,
    	    error(['{In order to use oracle you need to enable the collect of chtrees.}'])).
%	Q is (-0.25 * Ds) + Ev - (3.17 * DSa) - (1.35 * DSf) - Nn.
assess(balance,o(A,History,Tree,Unf_stats,DeltaS),Write,Q):-
	(member(derivation_steps(Ds),Unf_stats), 
	 member(evaluations(Ev),Unf_stats),
	 member(nondet-nl(NdNl),Unf_stats)  ->
	    size_type(Size_type),
	    orig_size(Size_type,[A],Size_orig),
	    size(Size_type,Tree,Size_new),
	    DSa is Size_new/Size_orig,
	    orig_size(Size_type,DeltaS,DSf),	    
	    (oracle_calibration(second), Write==write ->
	        write_conf(History,values([Ds,Ev,NdNl,DSa,DSf]))
	    ;   
		true),
	    Q is Ds + Ev + (-3.17 * DSa) + (-1.21 * DSf) - NdNl
% path max fit | term_count | all sols to solver
%	    Q is (1.1646 * Ds) + (-7.3457 * Ev) + (-17.4865 * DSa) + (-1.0 * DSf)  + (-100 * NdNl)
% path max fit | term_count | mean over benchs
%	    Q is (2.158 * Ds) + (-66.935 * Ev) + (-26.839 * DSa) + (0.125 * DSf)  + (-100 * NdNl)
% path max fit | term_size |  all sols to solver
%	    Q is (10.7632 * Ds) + (-52.523 * Ev) + (-355.119 * DSa) + (-1.0 * DSf)  + (-100 * NdNl)
% path max fit | term_size | mean over benchs
%	    Q is (5.217 * Ds) + (216.598 * Ev) + (-1223.267 * DSa) + (-1.0 * DSf)  + (-1000 * NdNl)
	;
	    Q = 0,
    	    error(['{In order to use oracle you need to enable the collect of unfolding stats.}'])).
%	



:- pop_prolog_flag(multi_arity_warnings).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Measuring size of clauses.
%


orig_size(Size_type,Atoms,Size):-
	get_original_predicates(Atoms,Origs),
	size_orig_cls(Origs,Size_type,Size).

size_orig_cls([],_,0).
size_orig_cls([Cl|T],Type,N):-
	size(Type,Cl,N1),
	size_orig_cls(T,Type,N2),
	N is N1 + N2.

:- pred size(+,+Clauses,-Size) #"Estimates the size of a list of
	clauses @var{Clauses} by estimating the size of their heads
	and their bodies".


size(count,Clauses,N):- 
	count_terms(Clauses,N).
size(term,Clauses,N):-
	size_clauses(Clauses,N).

:- pred count_terms(+L,-N) #"Estimates the size of a list of clauses
	@var{L} by simply counting heads and literals in their
	bodies".

count_terms([],0).
count_terms([clause(_,B)|T],N):-
	length(B,N1),
	count_terms(T,N2),
	N is 1 + N1 + N2.

:- pred size_clauses(+L,-N) #"Estimates the size of a list of clauses
	@var{L} by counting the variables, functors and constants in
	ther heads and literals in their bodies".


size_clauses([],0).
size_clauses([clause(H,B)|T],N):-
	size_term(H,N1),
	size_terms(B,N2),
	size_clauses(T,N3),
	N is N1 + N2 + N3.

size_terms([],0).
size_terms([H|T],N):-
	size_term(H,N1),
	size_terms(T,N2),
	N is N1 + N2.

size_term(T,1):-
	var(T),!.
size_term(T,N):-
	T =.. [_|Args],
	size_terms(Args, N1),
	N is N1 + 1.

:- pred get_original_predicates(+,-).
get_original_predicates([],[]).
get_original_predicates([H|T],[Cl|T2]):-
	findall((clause(H,Body)), orig_clause(H, Body, _Counter),Cl),
	get_original_predicates(T,T2).

