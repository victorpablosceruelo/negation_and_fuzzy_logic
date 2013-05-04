:- module(common_bench,
	[
	    bench_rep/3,
	    bench_rep_sort/4,
	    test_driver/2,
	    increasing_agents/3,
	    delete_min_max/2,
% 	    reset_all_wams/0,
	    average_list/2
	],
	[]).

:- use_module(library(andprolog_nd(apll_nd))).

:- use_package(andprolog_nd).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(system), [pause/1]).
:- use_module(library(format), [format/2]).
:- use_module(library(between)).
:- use_module(library(dynamic)).
:- use_module(library(sort)).
:- use_module(library(lists)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(concurrency)).


:- dynamic time/1.
:- dynamic seq_average/1.

delete_min_max(L,NL1):-
        sort(L,NL),
        delete_min_max_(NL,NL1).
delete_min_max_([X],[X]):- !.
delete_min_max_([X,Y],[X,Y]):- !.
delete_min_max_([_X,Y,_Z],[Y]):- !.
delete_min_max_([_|SL],List):-
        append(List,[_],SL),
	!.

sum_list([],0).
sum_list([X|R],Sum):-
    sum_list(R,N1),
    Sum is N1 + X.

average_list(List,Av):-
        sum_list(List,Sum),
        length(List,N),
        Av is Sum / N.


:- meta_predicate bench_rep(?, goal, ?).

%% Warm up memory & cache
bench_rep(_Reps, Goal, _):-
	release_all_for_unwinding,
	once(Goal),
%       reset_all_wams, 
% 	eng_release_all_for_unwinding,
 %% 	pause(2),
	fail.

%% Actually execute benchmark
bench_rep(Reps, Goal, Name/Arity):- 
        between(1, Reps, _),
	release_all_for_unwinding,
        statistics(walltime, [T1,_]),
        once(Goal),
        statistics(walltime, [T2,_]),
        Delta is T2 - T1,
	asserta_fact(time(Delta)),
        format("-- ~w/~w in ~f ms.~n", [Name, Arity, Delta]),
%       reset_all_wams,
% 	eng_release_all_for_unwinding,
 %% 	pause(2),
        fail.
bench_rep(_, _, _) :-
	findall(Time,(time(Time),retract_fact(time(Time))),L),
	delete_min_max(L,L2),
	average_list(L2,Average),
	format("     Time Average     = ~f ms.~n", [Average]),
	(
	    current_fact(seq_average(_)) ->
	    seq_average(SeqAverage),
	    SpeedUp is SeqAverage / Average,
	    format("     SpeedUp obtained = ~f~n", [SpeedUp])
	;
	    asserta_fact(seq_average(Average))
	).

:- meta_predicate bench_rep_sort(?, goal, ?, ?).

%% Actually execute QuickSort or MergeSort
bench_rep_sort(_Reps, Goal, _, _):-
	once(Goal),
% 	reset_all_wams,
% 	eng_release_all_for_unwinding,
 %% 	pause(2),
	fail.
bench_rep_sort(Reps, Goal, GC, Name/Arity):- 
        between(1, Reps, _),
	release_all_for_unwinding,
        statistics(walltime, [T1,_]),
        once(Goal),
        statistics(walltime, [T2,_]),
	arg(1,Goal,OrigGoal),
	arg(1,OrigGoal,InputVector),
	( GC == gc -> arg(4,OrigGoal,SortedVector)
	; arg(2,OrigGoal,SortedVector)
	),
	length(InputVector,Length),
	ordered(SortedVector,Length),
        Delta is T2 - T1,
	asserta_fact(time(Delta)),
        format("-- ~w/~w in ~f ms.~n", [Name, Arity, Delta]),
%       reset_all_wams,
% 	eng_release_all_for_unwinding,
 %% 	pause(2),
        fail.
bench_rep_sort(_, _, _, _) :-
	findall(Time,(time(Time),retract_fact(time(Time))),L),
	delete_min_max(L,L2),
	average_list(L2,Average),
	format("     Time Average     = ~f ms.~n", [Average]),
	(
	    current_fact(seq_average(_)) ->
	    seq_average(SeqAverage),
	    SpeedUp is SeqAverage / Average,
	    format("     SpeedUp obtained = ~f~n", [SpeedUp])
	;
	    asserta_fact(seq_average(Average))
	).


:- meta_predicate once(goal).
once(G):- call(G), !.

:- meta_predicate test_driver(?, goal).

test_driver(NAgents, Goal):-
        set_prolog_flag(gc, off),
	( NAgents > 1 -> ensure_agents(NAgents) ; true ),
        call(Goal).

:- meta_predicate increasing_agents(?, ?, goal).

increasing_agents(Min, Max, Goal):-
        between(Min, Max, Agents),
        format("                    ~d agents~n", [Agents]),
        ensure_agents(Agents),
 %%         pause(2),
        call(Goal),
        format("~n++++++++++++++++++++++++++++++++++++++++++++++++++~n", []),
        fail.

% %reset_all_wams :- eng_reset_all_wams.
% reset_all_wams:- eng_reset_all_wams, pause(2).
%         % N > 0,
% 	% N1 is N - 1,
% 	% eng_reset_all_wams(N1).


% ordered/2: List is sorted in increasing order and has length L

ordered([], 0).
ordered([A|L], N) :-
	N >0,
	ordered_lagging(L, A, N).

ordered_lagging([], _A, 1).
ordered_lagging([B|L], A, N) :-
	N > 1,
	A =< B,
	N1 is N - 1,
	ordered_lagging(L, B, N1).

