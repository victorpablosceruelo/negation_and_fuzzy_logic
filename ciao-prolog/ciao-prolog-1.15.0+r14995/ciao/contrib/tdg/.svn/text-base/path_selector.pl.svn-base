:- module(path_selector,[select_paths/4],[assertions]).

:- doc(title,"Path selector for regular tree languages as RUL's").

:- doc(author, "M. Zamalloa").

:- doc(module,"This module provides the operations to automatically select a finite set of 
	trace terms from the corresponding regular tree language as a RUL describing the possibly
	infinite set of trace terms. The RUL program is represented as a set of trace_rul_clause/2
	facts (see rul_paths_generator). It provides different coverage criteria.").

:- use_module(library(lists), [append/3, length/2, reverse/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(ordered_lists).
:- use_module(trace_terms).
:- use_module(library(write), _).

:- use_module(rul_paths_generator, [trace_rul_clause/2]).

%% Implemented Criteria:
%% ---------------------
%% Criteria I: Find all trace terms whose depth is less than a given depth
%% Criteria II: All trace terms in which there are no more than k recursive calls
%% Criteria III: Minimal set of trace terms covering a given set of labels at least once


%% Example program
clause_(s(nr1),[]).
clause_(s(nr2(A,B)),[s(A),t(B)]).
clause_(t(ap1),[]).
clause_(t(ap2(A)),[t(A)]).


select_paths(RType,ii,[K],Paths) :-
	A =..[RType,TrTerm],
	findall(TrTerm,solve_II([A],K,[]),Paths).
select_paths(RType,iii,[Labels,K],Paths) :-
	A =..[RType,_TrTerm],
	cleanup_label_info, cleanup,
	set_uncovered(Labels),
	solve_III([(1,node([A],1,0,[]))],K),
	findall(TrTerm,(trace(RTr),reverse(RTr,Tr),tr_to_trterm(Tr,TrTerm)),Paths),
	cleanup_label_info, cleanup.


solve_II([],_K,_T).
solve_II([A|R],K,AppTable) :-
	check_criteria_II(A,K,AppTable),
	update_app_table(AppTable,A,AppTable_p),
	trace_rul_clause(A,B),
	append(B,R,NewGoal),
	solve_II(NewGoal,K,AppTable_p).

check_criteria_II(A,K,AppTable) :-
	functor(A,F,_),
	get_table(AppTable,F,N),
	N < (K+1).

:- data uncovered/1.
:- data trace/1.

cleanup :-
	retractall_fact(uncovered(_)),
	retractall_fact(trace(_)).

solve_III(Goals,_K) :- is_empty(Goals),!.
solve_III(Goals,K) :- 
	extract_first(Goals,node([],_F,_G,Labels),RemGoals),!,
%	write(Labels),nl,
	assertz_fact(trace(Labels)),
	update_uncovered(Labels),
	continue_solving(RemGoals,K).
solve_III(Goals,K) :- 
	extract_first(Goals,node([A|R],_F,G,Ls),RemGoals),
	G < K, % We bound the tree depth to ensure termination
	G_p is G + 1,
	findall((F_p,node(NewGoal,F_p,G_p,[Label|Ls])),
	           (trace_rul_clause(A,B),append(B,R,NewGoal),length(NewGoal,H),
		    F_p is G_p + H,take_label(A,Label)),
		NewGoals),
	insert_all(RemGoals,NewGoals,Goals_p),
	solve_III(Goals_p,K).

%% Commenting the following rule we get all solutions in one call -> deterministic
%continue_solving(_Gs,_K) :- check_criteria_III.
continue_solving(Gs,K) :- check_criteria_III,!,solve_III(Gs,K).
continue_solving(_,_).

set_uncovered(Ls) :-
	member(L,Ls),
	assertz_fact(uncovered(L)),
	fail.
set_uncovered(_).
	
check_criteria_III :- uncovered(_),!.
	
update_uncovered(Ls) :-
	member(L,Ls),
	retractall_fact(uncovered(L)),
	fail.
update_uncovered(_).

take_label(A,L) :-
	A =..[_|[Arg]],
	functor(Arg,L,Arity),
	set_label_info(L,Arity).


%% Appearances table (used in Criteria II)
%% ---------------------------------------

get_table([],_,0).
get_table([R1|_Rs],F,N) :-
	R1 =..[F,N],!.
get_table([_R1|Rs],F,N) :-
	get_table(Rs,F,N).

update_app_table([],A,[R1]) :-
	functor(A,F,_),
	R1 =..[F,1].
update_app_table([R1|Rs],A,[R1_p|Rs]) :-
	functor(A,F,_),
	R1 =..[F,N],!,
	N_p is N + 1,
	R1_p =..[F,N_p].
update_app_table([R1|Rs],A,[R1|Rs_p]) :-
	update_app_table(Rs,A,Rs_p).
