:- module(
	nondet_move_top_rt,
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

:- use_module(library(system)).
:- include(library(andprolog(andprolog_ops))).
:- use_module(library(andprolog(andprolog_sched))).
:- use_module(library(andprolog(andprolog_props))).

:- use_module(library(andprolog(callh_rt))).
:- use_module(library(andprolog(agents_rt))).

:- use_module(library(apll)).

:- use_module(library(odd)).


%%**********************************************************************
%% HIGH-LEVEL PRIMITIVES
%%**********************************************************************


:- pred '&>'(+callable,-handler).

:- doc('&>'(Goal,Handler), "Sends out the nondeterministic goal
   @var{Goal} to be executed potentially by another agent, returning
   in @var{Handler} a handler of the goal sent. It performs cleaning
   in backtracking.").

:- meta_predicate((goal&>_)).
Goal &> Handler :-
	push_goal(Goal,nondet,Handler),
 %%    	display(publishing(Goal)),nl,
% 	undo(cancel(Handler)),
	release_some_suspended_thread.
 %% 	display(libero_some),nl.


 %% cancel(Handler) :-
 %% 	cancellation(Handler),
 %% 	enter_mutex(Handler),
 %% 	show_handler(Handler),
 %% 	(
 %% 	    goal_cancelled(Handler) ->
 %% 	    exit_mutex(Handler)
 %% 	;
 %% 	    exit_mutex(Handler),
 %% 	    enter_mutex_self,
 %% 	    suspend
 %% 	),
 %% 	show_handler(Handler).


:- pred '&&>'(+callable,-handler).

:- doc('&&>'(Goal,Handler), "Fair version of the &>/2 operator. If
   there is no idle agent, one is created to execute the goal
   @var{Goal}. This way, fairness among concurrent agents is
   ensured.").

:- meta_predicate((goal&&>_)).
Goal &&> Handler :-
	create_agent,
	Goal &> Handler.


:- pred '&'(+callable).

:- doc('&'(Goal), "Sends out the nondeterministic goal @var{Goal}
   to be executed potentially by another agent. No waiting for its
   return is performed.").

:- meta_predicate((goal&)).
Goal & :-
	Goal &> _.


:- pred '&&'(+callable).

:- doc('&&'(Goal), "Fair version of the '&'/1 operator.").

:- meta_predicate((goal&&)).
Goal && :-
	Goal &&> _.


:- pred '<&'(+handler).

:- doc('<&'(Handler), "Reads the bindings made to the output
   variables of the goal associated to @var{Handler}, or executes it
   if that goal has not been executed yet. Backtracking over the goal
   will be performed at this point.").

Handler <& :-
	Handler <&& .


:- pred '<&&'(+handler).

:- doc('<&&'(Handler), "Fair version of the <&/1 operator.").

Handler <&& :-
 %%    	display(checking_state),nl,
	enter_mutex_self,
	(
	    goal_available(Handler) ->
 %%         display(is_available),nl,
	    exit_mutex_self,
	    retrieve_goal(Handler,Goal),
 %%         display(exec_local(Goal)),nl,
	    call(Goal)
 %%     	    display(exec_local_answer1(Goal)),nl
	;
 %%    	    display(not_available),nl,
	    (
		goal_toreexecute(Handler) ->
		exit_mutex_self,
		retrieve_goal(Handler,Goal),
 %%     		display(reexecuting(Goal)),nl,
 %%   		display(exec_local(Goal)),nl,
		call(Goal)
 %%   		display(exec_local_answer2(Goal)),nl
 %% 		pause(1)
 %% 		enter_mutex_remote(Handler),
 %% 		send_event(Handler),
 %% 		exit_mutex_remote(Handler),
 %% 		release_remote(Handler),
 %% 		enter_mutex_self
	    ;
 %%     		display(not_to_reexecte),nl,
		perform_some_other_work(Handler)
	    )
	).


:- pred '&'(+callable,+callable).

:- doc('&'(Goal1,Goal2), "Performs a parallel fork of the goals
   @var{Goal1} and @var{Goal2} involved and waits for the execution of
   both to finish. If some of the goals has not been picked up by
   another agent then it will be executed by the publishing agent.").

:- meta_predicate((goal&goal)).
GoalA & GoalB :-
	GoalA &> H,
 %%   	display(exec_local(GoalB)),nl,
	GoalB,
 %%   	display(exec_local_answer3(GoalB)),nl,
	H <& .


:- pred '&&'(+callable,+callable).

:- doc('&&'(Goal1,Goal2), "Fair version of the '&'/2 operator.").

:- meta_predicate((goal&&goal)).
GoalA && GoalB :-
	GoalA &&> H,
	GoalB,
	H <&& .


%%**********************************************************************
%% AUXILIARY DEFINITIONS
%%**********************************************************************


:- pred sending_event(+handler).

B:- doc(sending_event(+Handler), "Sends event to the agent that
   picked up the goal associated to @var{Handler} in order to perform
   backwards execution over it.").

sending_event(_).
sending_event(Handler) :-
 %% 	display(pasa_sending),nl,
	enter_mutex_self,
	set_goal_tobacktrack(Handler),
	exit_mutex_self,
	enter_mutex_remote(Handler),
 %%   	display(sending_back(Handler)),nl,
	send_event(Handler),
	release_remote(Handler),
	exit_mutex_remote(Handler),
	enter_mutex_self,
	perform_some_other_work(Handler).


:- pred perform_some_other_work(+handler).

:- doc(perform_some_other_work(+Handler), "Checks whether the
   execution of the goal associated to the handler @var{Handler},
   which has been picked up by another agent, has already finished or
   failed.").

perform_some_other_work(Handler) :-
 %% 	display(pasa_performing),nl,
	goal_scheduling(GS),
	!,
	perform_some_other_work_(Handler,GS).
perform_some_other_work_(Handler,GS) :-
	(
	    goal_finished(Handler) ->
 %%     	    display(is_finished),nl,
	    exit_mutex_self,
 	    sending_event(Handler)
	;
	    (
		goal_failed(Handler) ->
 %%     		display(is_failed),nl,
		set_goal_toreexecute(Handler),
		exit_mutex_self,
		fail
	    ;
 %%     		display(to_suspend_local),nl,
		suspend,
 %%     		display(after_suspend_local),nl,
		perform_some_other_work_(Handler,GS)
	    )
	).
