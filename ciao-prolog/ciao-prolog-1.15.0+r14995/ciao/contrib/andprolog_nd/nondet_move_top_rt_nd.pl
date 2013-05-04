:- module(
	nondet_move_top_rt_nd,
        [
	    '&'/2,
	    '&>'/2,
	    '<&'/1,
	    '&'/1,
	    '&&'/2,
	    '&&'/1,
	    '&&>'/2,
	    '<&&'/1
	],
	[assertions, isomodes]
	 ).

:- include(library(andprolog_nd(andprolog_nd_ops))).
:- use_module(library(andprolog_nd(andprolog_nd_sched))).
:- use_module(library(andprolog_nd(andprolog_nd_props))).
:- use_module(library(andprolog_nd(agents_rt_nd))).
:- use_module(library(andprolog_nd(apll_nd))).

:- use_module(library(odd)).


%%**********************************************************************
%% HIGH-LEVEL PRIMITIVES
%%**********************************************************************


:- pred '&>'(+callable,-handler).

:- comment('&>'(Goal,Handler), "Sends out the nondeterministic goal
   @var{Goal} to be executed potentially by another agent, returning
   in @var{Handler} a handler of the goal sent. It performs cleaning
   in backtracking.").


:- meta_predicate((goal&>_)).
Goal &> Handler :-
	(true; fail), %avoid environment trimming -> push_choice_pt cancellation!
	push_goal(Goal,nondet,Handler),
 %% 	undo(decrease_dep_info_size),
	release_some_suspended_thread(Handler),
	sending_event(Handler).
 %% 	undo(cancellation(Handler)).

decrease_dep_info_size :-
	get_dep_info(DepSize,DepId),
	NewDepSize is DepSize - 1,
	set_dep_info(NewDepSize,DepId).

 %% cancellation(Handler) :-
 %% 	enter_mutex_self,
 %% 	wait_until_finish(Handler),
 %% 	exit_mutex_self,
 %% 	reclaim_handler(Handler).
	


:- pred '&&>'(+callable,-handler).

:- comment('&&>'(Goal,Handler), "Fair version of the &>/2 operator. If
   there is no idle agent, one is created to execute the goal
   @var{Goal}. This way, fairness among concurrent agents is
   ensured.").

:- meta_predicate((goal&&>_)).
Goal &&> Handler :-
	create_agent,
	Goal &> Handler.


:- pred '&'(+callable).

:- comment('&'(Goal), "Sends out the nondeterministic goal @var{Goal}
   to be executed potentially by another agent. No waiting for its
   return is performed.").

:- meta_predicate((goal&)).
Goal & :-
	Goal &> _.


:- pred '&&'(+callable).

:- comment('&&'(Goal), "Fair version of the '&'/1 operator.").

:- meta_predicate((goal&&)).
Goal && :-
	Goal &&> _.


:- pred '<&'(+handler).

:- comment('<&'(Handler), "Reads the bindings made to the output
   variables of the goal associated to @var{Handler}, or executes it
   if that goal has not been executed yet. Backtracking over the goal
   will be performed at this point.").

Handler <& :-
	Handler <&& .


:- pred '<&&'(+handler).

:- comment('<&&'(Handler), "Fair version of the <&/1 operator.").

Handler <&& :-
	(
	    goal_available(Handler) ->
 %% 	    get_dep_info(Dep_Size,Dep_Id),
 %% 	    undo(set_dep_info(Dep_Size,Dep_Id)),
	    retrieve_goal(Handler,Goal),
	    call(Goal)
	;
	    enter_mutex_self,
	    wait_until_finish(Handler)
	).


:- pred '&'(+callable,+callable).

:- comment('&'(Goal1,Goal2), "Performs a parallel fork of the goals
   @var{Goal1} and @var{Goal2} involved and waits for the execution of
   both to finish. If some of the goals has not been picked up by
   another agent then it will be executed by the publishing agent.").

:- meta_predicate((goal&goal)).
GoalA & GoalB :-
	GoalA &> H,
	GoalB,
	H <& .


:- pred '&&'(+callable,+callable).

:- comment('&&'(Goal1,Goal2), "Fair version of the '&'/2 operator.").

:- meta_predicate((goal&&goal)).
GoalA && GoalB :-
	GoalA &&> H,
	GoalB,
	H <&& .


%%**********************************************************************
%% AUXILIARY DEFINITIONS
%%**********************************************************************


:- pred sending_event(+handler).

:- comment(sending_event(+Handler), "Sends event to the agent that
   picked up the goal associated to @var{Handler} in order to perform
   backwards execution over it.").

sending_event(_).
sending_event(Handler) :-
	enter_mutex_self,
	(
	    goal_finished(Handler) ->
	    exit_mutex_self,
	    set_goal_tobacktrack(Handler),
	    send_event(Handler),
	    enter_mutex_remote(Handler),
	    release_remote(Handler),
	    exit_mutex_remote(Handler),
	    enter_mutex_self,
	    perform_some_other_work(Handler)
	;
	    exit_mutex_self,
	    fail
	).



:- pred perform_some_other_work(+handler).

:- comment(perform_some_other_work(+Handler), "Checks whether the
   execution of the goal associated to the handler @var{Handler},
   which has been picked up by another agent, has already finished or
   failed.").

perform_some_other_work(Handler) :-
	(
	    goal_finished(Handler) ->
	    exit_mutex_self,
	    sending_event(Handler)
	;
	    (
		goal_failed(Handler) ->
		%% I could make pruning here!
 %%   		set_goal_toreexecute(Handler),
		%%Should send an event to recompute.
		exit_mutex_self,
		fail
	    ;
		suspend,
		perform_some_other_work(Handler)
	    )
	).

wait_until_finish(Handler) :-
	(
	    goal_finished(Handler) ->
	    exit_mutex_self
	;
	    (
		goal_failed(Handler) ->
		exit_mutex_self,
		fail
	    ;
		suspend,
		wait_until_finish(Handler)
	    )
	).


