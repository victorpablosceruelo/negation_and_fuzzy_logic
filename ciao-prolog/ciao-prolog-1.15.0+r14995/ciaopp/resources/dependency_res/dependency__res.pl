:- module(dependency__res, [dependency_analysis/7],
	    [assertions, resources(inferres_decl)]).

%
%  dependency.pl		Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for performing the data dependency
%  analysis for the predicates in the program in topologically sorted order.
%

:- use_module(resources(resources_basic)).
:- use_module(resources(determinacy_res(mutual_exclusion_res)),
	    [mutual_exclusive_classes/3]).
:- use_module(resources(init_res(symtable_res)),
	    [
		find_symbol_field/4,
		insert_symbol_field/4
	    ]).
:- use_module(resources(init_res(initsystem_basic_res)), [clause_type/2]).
:- use_module(infer(infer),                              [get_info/5]).
:- use_module(resources(dependency_res(build_adg_res)),
	    [argument_dependency_graph/8]).
:- use_module(resources(dependency_res(build_ldg_res)),
	    [literal_dependency_graph/2]).

:- pred dependency_analysis/7 :: list(predname) * list(bottom_entry)
	* list(symbol_entry) * list * list * list * nnegint
	+ (not_fails, is_det) #
"Perform the data dependency analysis for a strongly connected component.".
dependency_analysis([], _, _, [], [], [], 1).
dependency_analysis([Pred|CompList], BT, ST, [Adg|AList], [Ldg|LList],
	    [Gvars|GList], Error) :-
	dependency_predicate(Pred, BT, ST, Adg, Ldg, Gvars, Error1),
	dependency_analysis(CompList, BT, ST, AList, LList, GList, Error2),
	Error is Error1 * Error2.

:- pred dependency_predicate/7 :: predname * list(bottom_entry)
	* list(symbol_entry) * list * list * list * nnegint
	+ (not_fails, is_det) #
	"Perform the data dependency analysis for a predicate.".
dependency_predicate(Pred, BT, ST, Adg, Ldg, Gvars, Error) :-
	find_symbol_field(ST, Pred, clause, ClauseKeys),
	find_symbol_field(ST, Pred, mode,   Mode),
	mutual_exclusive_classes(ClauseKeys, Mode, Classes),
%% Commented out by PLG 2 Oct 97
%% 	nl,
%% 	write('* Mutually exclusive classes of clauses for predicate '),
%% 	write(Pred),
%% 	write(' :'),
%% 	nl,nl,
%% 	write(Classes),nl,
% End commented
	number_of_clauses(ClauseKeys, NumClauses),
	select_best_mutex_info(Pred, NumClauses, Classes, NClasses),
	insert_symbol_field(ST, Pred, mutex, NClasses),
	dependency_clauses(ClauseKeys, BT, ST, Adg, Ldg, Gvars, Error).

:- pred number_of_clauses/2 :: list(clause_key_t) * nnegint
	+ (not_fails, is_det).
% rtcheck -- EMM
number_of_clauses(Clauses, 0) :-
	var(Clauses),
	!. % choicepoint detected with rtcheck -- EMM
number_of_clauses([_|CList], NumClauses) :-
	number_of_clauses(CList, NumCLs),
	NumClauses is NumCLs + 1.

select_best_mutex_info(Pred, NumClauses, Classes, NClasses) :-
	Pred = F / A,
	functor(Goal, F, A),
	( ( get_info(is_det, pred, _, Goal, (_, [Mutex, _Det])),
		Mutex == mut_exclusive ) ->
	    create_pairwise_mutex_classes(NumClauses, [], NClasses)
	;
	    NClasses = Classes
	).

:- pred create_pairwise_mutex_classes/3 :: int * list * list
	+ (not_fails, is_det).
% rtcheck -- EMM
create_pairwise_mutex_classes(0, L,  L) :- !. % choicepoint detected with rtcheck -- EMM
create_pairwise_mutex_classes(N, In, Out) :-
	N > 0,
	N1 is N - 1,
	create_pairwise_mutex_classes(N1, [[N]|In], Out).

:- pred dependency_clauses/7 :: list(clause_key_t) * list(bottom_entry)
	* list(symbol_entry) * list * list* list * nnegint
	+ (not_fails, is_det) #
"Perform the data dependency analysis for the set of clauses in a predicate.".
dependency_clauses(ClauseKeys, _, _, [], [], [], 1) :-
	var(ClauseKeys),
	!.
dependency_clauses([ClauseKey|CList], BT, ST, [Adg|AList], [Ldg|LList],
	    [Gvars|GList], Error) :-
	clause_key(ClauseKey, ClausePPKey, Key),
	dependency_clause(ClausePPKey, Key, BT, ST, Adg, Ldg, Gvars, Error1),
	dependency_clauses(CList, BT, ST, AList, LList, GList, Error2),
	Error is Error1 * Error2.

:- pred dependency_clause/8 :: clause_ppkey_t * atm * list(bottom_entry)
	* list(symbol_entry) * term * term * term * nnegint
	+ (not_fails, is_det) #
	"Perform the data dependency analysis for a clause.".
dependency_clause(ClausePPKey, Key, BT, ST, Adg, Ldg, Gvars, Error) :-
	clause_type(ClausePPKey, Type),
	argument_dependency_graph(Type, ClausePPKey, Key, BT, ST, Adg, Gvars,
	    Error),
	literal_dependency_graph(Adg, Ldg).
