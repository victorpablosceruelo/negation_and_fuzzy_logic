:- module(apll_nd,
	[
	    start_thread/1,
	    number_agents/1,
	    push_goal/1,
	    push_goal/3,
	    find_goal/1,
	    find_det_goal/1,
	    goal_available/1,
 %% 	    cancellation/1,
  	    reclaim_handler/1,
	    retrieve_goal/2,
	    goal_det/1,
	    set_goal_det/1,
	    set_goal_nondet/1,

	    goal_not_executed/1,
	    set_goal_not_executed/1,
	    goal_rem_executing/1,
	    set_goal_rem_executing/1,
	    goal_finished/1,
	    set_goal_finished/1,
	    goal_tobacktrack/1,
	    set_goal_tobacktrack/1,
	    goal_toreexecute/1,
	    set_goal_toreexecute/1,
	    goal_failed/1,
	    set_goal_failed/1,
	    goal_cancelled/1,
	    set_goal_cancelled/1,

	    get_dep_info/2,
	    set_dep_info/2,

	    send_event/1,
	    read_event/1,
	    save_init_execution/1,
	    save_end_execution/0,
	    move_execution_top/1,

	    waiting/1,
	    suspend/0,
	    release/1,
	    release_remote/1,
	    release_some_suspended_thread/1,
	    release_all_for_unwinding/0,
	    enter_mutex/1,
	    enter_mutex_self/0,
	    enter_mutex_remote/1,
	    exit_mutex/1,
	    exit_mutex_self/0,
	    exit_mutex_remote/1,

	    reset_stats/0,
	    show_memory_usage/0,
	    get_stats/3
        ],
	[assertions, isomodes, foreign_interface]).


:- comment(nodoc, assertions).

:- comment(filetype, package).

:- comment(title, "Low-level concurrency primitives for and-parallelism
                   support").

:- comment(author, "Amadeo Casas").
:- comment(author, "@tt{http://www.ece.unm.edu/~amadeo}").
:- comment(author, "University of New Mexico").

:- comment(module, "This module provides basic mechanisms to start threads,
                    wait for their completion, push goals, search for
                    goals, access to locks, etc. Most of these primitives
                    need to refer to an explicit goal and need to use some
                    information related to its state, stored in the data
                    structure @var{Handler}.

                    This primitives allow to efficiently implement at a
                    higher-level different approaches to exploiting
                    independent and-parallelism.").


 %% :- extra_compiler_opts(['-g -DDEBUG_TRAPPED -DDEP_TRAPPED -DSTATS']).
 %% :- extra_compiler_opts(['-g -DDEP_TRAPPED']).
:- extra_compiler_opts(['-g']).
:- extra_linker_opts(['-lpthread -lm']).

:- use_module(engine(internals), [term_to_meta/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOW-LEVEL SUPPORT PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- true pred initial
   + foreign_low(init).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOW-LEVEL SUPPORT PRIMITIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- true pred '$start_thread'(+int)
   + foreign_low(apll_nd_start_thread).
:- true pred '$number_agents'(-int)
   + foreign_low(apll_nd_number_agents).
% :- true pred '$push_goal'(+int)
%    + foreign_low(apll_nd_push_goal).
:- true pred '$push_goal'(+int,+int,?int)
   + foreign_low(apll_nd_push_goal).
:- true pred '$find_goal'(+int,-int)
   + foreign_low(apll_nd_find_goal).
:- true pred '$goal_available'(+int)
   + foreign_low(apll_nd_goal_available).
 %% :- true pred '$cancellation'(+int)
 %%    + foreign_low(apll_nd_cancellation).
:- true pred '$reclaim_handler'(+int)
	+ foreign_low(apll_nd_reclaim_handler).
:- true pred '$retrieve_goal'(+int,-int)
   + foreign_low(apll_nd_retrieve_goal).
:- true pred '$goal_det'(+int)
   + foreign_low(apll_nd_goal_det).
:- true pred '$set_goal_det'(+int)
   + foreign_low(apll_nd_set_goal_det).
:- true pred '$set_goal_nondet'(+int)
   + foreign_low(apll_nd_set_goal_nondet).
:- true pred '$goal_not_executed'(+int)
   + foreign_low(apll_nd_goal_not_executed).
:- true pred '$set_goal_not_executed'(+int)
   + foreign_low(apll_nd_set_goal_not_executed).
:- true pred '$goal_rem_executing'(+int)
   + foreign_low(apll_nd_goal_rem_executing).
:- true pred '$set_goal_rem_executing'(+int)
   + foreign_low(apll_nd_set_goal_rem_executing).
:- true pred '$goal_finished'(+int)
   + foreign_low(apll_nd_goal_finished).
:- true pred '$set_goal_finished'(+int)
   + foreign_low(apll_nd_set_goal_finished).
:- true pred '$goal_tobacktrack'(+int)
   + foreign_low(apll_nd_goal_tobacktrack).
:- true pred '$set_goal_tobacktrack'(+int)
   + foreign_low(apll_nd_set_goal_tobacktrack).
:- true pred '$goal_toreexecute'(+int)
   + foreign_low(apll_nd_goal_toreexecute).
:- true pred '$set_goal_toreexecute'(+int)
   + foreign_low(apll_nd_set_goal_toreexecute).
:- true pred '$goal_failed'(+int)
   + foreign_low(apll_nd_goal_failed).
:- true pred '$set_goal_failed'(+int)
   + foreign_low(apll_nd_set_goal_failed).
:- true pred '$goal_cancelled'(+int)
   + foreign_low(apll_nd_goal_cancelled).
:- true pred '$set_goal_cancelled'(+int)
   + foreign_low(apll_nd_set_goal_cancelled).

:- true pred '$get_dep_info'(+int,+int)
   + foreign_low(apll_nd_get_dep_info).
:- true pred '$set_dep_info'(+int,+int)
   + foreign_low(apll_nd_set_dep_info).

:- true pred '$send_event'(+int)
   + foreign_low(apll_nd_send_event).
:- true pred '$read_event'(-int)
   + foreign_low(apll_nd_read_event).
:- true pred '$save_init_execution'(+int)
   + foreign_low(apll_nd_save_init_execution).
:- true pred '$save_end_execution'
   + foreign_low(apll_nd_save_end_execution).
:- true pred '$move_execution_top'(+int)
   + foreign_low(apll_nd_move_execution_top).

:- true pred '$waiting'(+int)
   + foreign_low(apll_nd_waiting).
:- true pred '$suspend'
   + foreign_low(apll_nd_suspend).
:- true pred '$release'(+int)
   + foreign_low(apll_nd_release).
:- true pred '$release_remote'(+int)
   + foreign_low(apll_nd_release_remote).
:- true pred '$release_some_suspended_thread'(+int)
   + foreign_low(apll_nd_release_some_suspended_thread).
:- true pred '$release_all_for_unwinding'
   + foreign_low(apll_nd_release_all_for_unwinding).
:- true pred '$enter_mutex'(+int)
   + foreign_low(apll_nd_enter_mutex).
:- true pred '$enter_mutex_self'
   + foreign_low(apll_nd_enter_mutex_self).
:- true pred '$enter_mutex_remote'(+int)
   + foreign_low(apll_nd_enter_mutex_remote).
:- true pred '$exit_mutex'(+int)
   + foreign_low(apll_nd_exit_mutex).
:- true pred '$exit_mutex_self'
   + foreign_low(apll_nd_exit_mutex_self).
:- true pred '$exit_mutex_remote'(+int)
   + foreign_low(apll_nd_exit_mutex_remote).

:- true pred '$reset_stats'
   + foreign_low(apll_nd_reset_stats).
:- true pred '$show_memory_usage'
   + foreign_low(apll_nd_show_memory_usage).
:- true pred '$get_stats'(-int,-int,-int)
   + foreign_low(apll_nd_get_stats).

:- meta_predicate(start_thread(goal)).
:- meta_predicate(push_goal(goal,?,?)).

:- pred start_thread(+Goal) : callable # "Executes @var{Goal} in a new
   stack set, using a new thread. Used herein to create a number of
   parallel agents.".

start_thread(Goal):-
        term_to_meta(MetaGoal,Goal),
        '$start_thread'(MetaGoal).

:- pred number_agents(-N) : int # "Returns in @var{N} the number of
   agents in the system.".

number_agents(N):-
	'$number_agents'(N).

:- pred push_goal(+Goal,+Det,?Handler) : int * int * int # "Atomically
   creates the handler (an arbitrary structure in the heap) associated
   to @var{Goal}, described as deterministic or not by @var{Det}, and
   adds to the goal list a pointer to @var{Handler}. @var{Det} states
   whether the goal is deterministic or not.".

push_goal(Goal,Det,Handler):-
        '$push_goal'(Goal,Det,Handler).

:- pred push_goal(+Handler) : int # "Atomically adds a pointer to the
   particular handler @var{Handler} to the goal list in order to
   reexecute the goal associated to it.".

push_goal(Handler):-
        '$push_goal'(_,_,Handler).

:- pred find_goal(-Handler) : int # "Searches for a goal
   handler @var{Handler} published in some goal list and succeeds if
   one is found, failing otherwise. The access to each goal list is
   made atomically.".

find_goal(Handler):-
	'$find_goal'(any,Handler).

:- pred find_det_goal(-Handler) : int # "Searches for a
   deterministic goal associated to @var{Handler} and succeeds if one
   is found, failing otherwise. The access to each goal list is made
   atomically.".

find_det_goal(Handler):-
	'$find_goal'(det,Handler).

:- pred goal_available(+Handler) : int # "Succeeds if @var{Handler} is
   still in the goal list, and fails otherwise.".

goal_available(Handler):- 
        '$goal_available'(Handler).

 %% :- pred cancellation(+Handler) :
 %%    int
 %%      # "Frees the memory used by @var{Handler}.".
 %% 
 %% cancellation(Handler):-
 %% 	'$cancellation'(Handler).

:- pred reclaim_handler(+Handler) : int # "Frees the memory used by
	@var{Handler}.".

reclaim_handler(Handler):-
	'$reclaim_handler'(Handler).

:- pred retrieve_goal(+Handler,-Goal) : int * int # "Returns in
	@var{Goal} the parallel goal associated to @var{Handler}.".

retrieve_goal(Handler,Goal):-
	'$retrieve_goal'(Handler,Goal).

:- pred goal_det(+Handler) : int # "Succeeds if the the goal
   associated to @var{Handler} is deterministic, and fails
   otherwise.".

goal_det(Handler):-
	'$goal_det'(Handler).

:- pred set_goal_det(+Handler) : int # "Marks the goal associated to
   @var{Handler} as deterministic.".

set_goal_det(Handler):-
	'$set_goal_det'(Handler).

:- pred set_goal_nondet(+Handler) : int # "Marks the goal associated
   to @var{Handler} as non-deterministic.".

set_goal_nondet(Handler):-
	'$set_goal_nondet'(Handler).

:- pred goal_not_executed(+Handler) : int # "Succeeds if the goal
   associated to @var{Handler} has not been executed yet, and fails
   otherwise.".

goal_not_executed(Handler):-
	'$goal_not_executed'(Handler).

:- pred set_goal_not_executed(+Handler) : int # "Sets the execution of
   the goal associated to @var{Handler} to never executed.".

set_goal_not_executed(Handler):-
	'$set_goal_not_executed'(Handler).

:- pred goal_rem_executing(+Handler) : int # "Succeeds if the goal
   associated to @var{Handler} is remotely executing, and fails
   otherwise.".

goal_rem_executing(Handler):-
	'$goal_rem_executing'(Handler).

:- pred set_goal_rem_executing(+Handler) : int # "Sets the execution
   of the goal associated to @var{Handler} to being remotely
   executing.".

set_goal_rem_executing(Handler):-
	'$set_goal_rem_executing'(Handler).

:- pred goal_finished(+Handler) : int # "Succeeds if the execution of
   the goal associated to @var{Handler} has finished, and fails
   otherwise.".

goal_finished(Handler):-
	'$goal_finished'(Handler).

:- pred set_goal_finished(+Handler) : int # "Sets the execution of the
   goal associated to @var{Handler} to finished.".

set_goal_finished(Handler):-
	'$set_goal_finished'(Handler).

:- pred goal_tobacktrack(+Handler) : int # "Succeeds if the execution
   of the goal associated to @var{Handler} has to backtrack, and fails
   otherwise.".

goal_tobacktrack(Handler):-
	'$goal_tobacktrack'(Handler).

:- pred set_goal_tobacktrack(+Handler) : int # "Sets the execution of
   the goal associated to @var{Handler} to backtrack.".

set_goal_tobacktrack(Handler):-
	'$set_goal_tobacktrack'(Handler).

:- pred goal_toreexecute(+Handler) : int # "Succeeds if the execution
   of the goal associated to @var{Handler} has to backtrack, and fails
   otherwise.".

goal_toreexecute(Handler):-
	'$goal_toreexecute'(Handler).

:- pred set_goal_toreexecute(+Handler) : int # "Sets the execution of
   the goal associated to @var{Handler} to be reexecuted.".

set_goal_toreexecute(Handler):-
	'$set_goal_toreexecute'(Handler).

:- pred goal_failed(+Handler) : int # "Succeeds if the execution of
   the goal associated to @var{Handler} has failed, and fails
   otherwise.".

goal_failed(Handler):-
	'$goal_failed'(Handler).

:- pred set_goal_failed(+Handler) : int # "Sets the execution of the
   goal associated to @var{Handler} to failed.".

set_goal_failed(Handler):-
	'$set_goal_failed'(Handler).

:- pred goal_cancelled(+Handler) : int # "Succeeds if the execution of
   the goal associated to @var{Handler} has cancelled, and fails
   otherwise.".

goal_cancelled(Handler):-
	'$goal_cancelled'(Handler).

:- pred set_goal_cancelled(+Handler) : int # "Sets the execution of
   the goal associated to @var{Handler} to cancelled.".

set_goal_cancelled(Handler):-
	'$set_goal_cancelled'(Handler).

:- pred get_dep_info(-Dep_Size, -Dep_ID) : int * int # "Gets the
   dependence info to avoid trapped goals.".

get_dep_info(Dep_Size, Dep_ID):-
	'$get_dep_info'(Dep_Size, Dep_ID).

:- pred set_dep_info(+Dep_Size, +Dep_ID) : int * int # "Sets the
   dependence info to avoid trapped goals.".

set_dep_info(Dep_Size, Dep_ID):-
	'$set_dep_info'(Dep_Size, Dep_ID).

:- pred send_event(+Handler) : int # "Sends the handler to the agent
   that picked up the goal associated to the handler @var{Handler} in
   order to perform backtracking over it.".

send_event(Handler):-
	'$send_event'(Handler).

:- pred read_event(-Handler) : int # "Succeeds if the event queue of
   the agent is not empty and unifies @var{Handler} with the handler
   associated to the goal to backtrack over.".

read_event(Handler):-
	'$read_event'(Handler).

:- pred save_init_execution(+Handler) : int # "Saves the choice point
   that marks the starting point of the execution of the parallel goal
   associated to the handler @var{Handler}.".

save_init_execution(Handler):-
	'$save_init_execution'(Handler).

:- pred save_end_execution # "Saves the choice point that marks the
   final point of the execution of the parallel goal associated to the
   handler @var{Handler}.".

save_end_execution:-
	'$save_end_execution'.

:- pred move_execution_top(+Handler) : int # "Moves the choice point
   of the goal saved into the handler @var{Handler} to the top of the
   stack.".

move_execution_top(Handler):-
	'$move_execution_top'(Handler).

:- pred waiting(+Handler) : int # "Succeeds when the execution of the
   publishing agent associated to @var{Handler} is suspended, and
   fails otherwise.".

waiting(Handler):-
	'$waiting'(Handler).

:- pred suspend # "Suspends the execution of the current thread.".

suspend:-
	'$suspend'.

:- pred release(+Handler) : int # "Releases the execution of the
   publishing agent associated to @var{Handler}.".

release(Handler):-
	'$release'(Handler).

:- pred release_remote(+Handler) : int # "Releases the execution of
   the agent that picked up the goal associated to @var{Handler}.".

release_remote(Handler):-
	'$release_remote'(Handler).

:- pred release_some_suspended_thread(+Handler) # "Selects one out of
any suspended threads and sends it a signal to resume its execution.".

release_some_suspended_thread(Handler) :-
	'$release_some_suspended_thread'(Handler).

:- pred release_all_for_unwinding # "Releases the execution of all
agents to perform stack unwinding.".

release_all_for_unwinding:-
	'$release_all_for_unwinding'.

:- pred enter_mutex(+Handler) : int # "Attemps to enter into a mutual
   exclusion to access shared variables of the publishing agent
   associated to @var{Handler}.".

enter_mutex(Handler):-
	'$enter_mutex'(Handler).

:- pred enter_mutex_self # "The mutual exclusion is to access the
shared variables of the calling thread.".

enter_mutex_self:-
	'$enter_mutex_self'.

:- pred enter_mutex_remote(+Handler) : int # "The mutual exclusion is
   to access the shared variables of the remote thread associated to
   @var{Handler}.".

enter_mutex_remote(Handler):-
	'$enter_mutex_remote'(Handler).

:- pred exit_mutex(+Handler) : int # "Exits from the mutual exclusion
   of the publishing agent associated to @var{Handler}.".

exit_mutex(Handler):-
	'$exit_mutex'(Handler).

:- pred exit_mutex_self # "Exits from the local mutual exclusion.".

exit_mutex_self:-
	'$exit_mutex_self'.

:- pred exit_mutex_remote(+Handler) : int # "Exits from the mutual
   exclusion of the remote agent associated to @var{Handler}.".

exit_mutex_remote(Handler):-
	'$exit_mutex_remote'(Handler).

:- pred reset_stats # "Restarts the statistical measures for
nondeterministic parallel programs.".

reset_stats:-
	'$reset_stats'.

:- pred show_memory_usage # "Show the memory usage of all the agents.".

show_memory_usage:-
	'$show_memory_usage'.

:- pred get_stats(-T, -Tt, -Tft) : int * int *int # "Gets current statistics of
nondeterministic parallel programs".

get_stats(T,Tt,Tft):-
	'$get_stats'(T,Tt,Tft).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_foreign_source(apll_nd).
:- initialization(initial).


