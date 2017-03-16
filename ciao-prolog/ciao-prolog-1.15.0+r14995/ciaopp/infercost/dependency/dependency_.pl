:- module(dependency_, [dependency_analysis/7], []).

%
%  dependency.pl		Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for performing the data dependency
%  analysis for the predicates in the program in topologically sorted order.
%

:- use_module(infercost(determinacy(mutual_exclusion)), 
	[mutual_exclusive_classes/3]).
:- use_module(infercost(init(symtable)), 
	[
	    find_symbol_field_clause/3,
	    find_symbol_field/4,
	    insert_symbol_field/4
	]).
:- use_module(infercost(init(initsystem_basic)), [clause_type/2]).
:- use_module(infer(infer), [get_info/5]).
:- use_module(infercost(dependency(build_adg)), 
	[argument_dependency_graph/7]).
:- use_module(infercost(dependency(build_ldg)), 
	[literal_dependency_graph/2]).

%
%  Perform the data dependency analysis for a strongly connected component.
%
dependency_analysis([],_,_,[],[],[],1).
dependency_analysis([Pred|CompList],BT,ST,[Adg|AList],[Ldg|LList],
		    [Gvars|GList],Error) :-
	dependency_predicate(Pred,BT,ST,Adg,Ldg,Gvars,Error1),
	dependency_analysis(CompList,BT,ST,AList,LList,GList,Error2),
	Error is Error1*Error2.

%
%  Perform the data dependency analysis for a predicate.
%
dependency_predicate(Pred,BT,ST,Adg,Ldg,Gvars,Error) :-
	find_symbol_field_clause(ST,Pred,Clauses),
	find_symbol_field(ST,Pred,(mode),Mode),
	mutual_exclusive_classes(Clauses,Mode,Classes),
        %% Commented out by PLG 2 Oct 97
 %% 	nl,
 %% 	write('* Mutually exclusive classes of clauses for predicate '),
 %% 	write(Pred),
 %% 	write(' :'),
 %% 	nl,nl,
 %% 	write(Classes),nl,
        % End commented
        number_of_clauses(Clauses,NumClauses), 
        select_best_mutex_info(Pred,NumClauses,Classes,NClasses),
        insert_symbol_field(ST,Pred,mutex,NClasses),
	dependency_clauses(Clauses,BT,ST,Adg,Ldg,Gvars,Error).

number_of_clauses(Clauses,0):- 
        var(Clauses).
number_of_clauses(Clauses,NumClauses):-
	nonvar(Clauses),
	Clauses = [_Clause|CList],
        number_of_clauses(CList,NumCLs),
        NumClauses is NumCLs + 1.

select_best_mutex_info(Pred,NumClauses,Classes,NClasses):-
        Pred = F/A,
        functor(Goal,F,A),  
        ((get_info(is_det,pred,_,Goal,(_,[Mutex,_Det])),
          Mutex == mut_exclusive) -> 
              create_pairwise_mutex_classes(NumClauses,[],NClasses)
              ; 
              NClasses = Classes
        ).
 
create_pairwise_mutex_classes(0,L,L).
create_pairwise_mutex_classes(N,In,Out):-
        N > 0,
        N1 is N - 1,
        create_pairwise_mutex_classes(N1,[[N]|In],Out).                  

%
%  Perform the data dependency analysis for the set of clauses in a predicate.
%
dependency_clauses(Clauses,_,_,[],[],[],1) :-
	var(Clauses),
	!.
dependency_clauses(Clauses,BT,ST,[Adg|AList],[Ldg|LList],[Gvars|GList],Error) :-
	nonvar(Clauses),
	Clauses = [Clause|CList],
	dependency_clause(Clause,BT,ST,Adg,Ldg,Gvars,Error1),
	dependency_clauses(CList,BT,ST,AList,LList,GList,Error2),
	Error is Error1*Error2.

%
%  Perform the data dependency analysis for a clause.
%
dependency_clause(Clause,BT,ST,Adg,Ldg,Gvars,Error) :-
	clause_type(Clause,Type),
	argument_dependency_graph(Type,Clause,BT,ST,Adg,Gvars,Error),
	literal_dependency_graph(Adg,Ldg).
