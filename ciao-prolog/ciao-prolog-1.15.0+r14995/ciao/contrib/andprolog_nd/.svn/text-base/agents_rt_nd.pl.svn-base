:- module(
        agents_rt_nd,
        [
	    create_agent/0,
	    create_agents/1,
	    ensure_agents/1
	],
	[assertions, isomodes]
	 ).

:- include(library(andprolog_nd(andprolog_nd_ops))).
:- use_module(library(andprolog_nd(andprolog_nd_sched))).
%:- include(andprolog_nd_props).

:- use_module(library(andprolog_nd(callh_rt_nd))).

:- use_module(library(andprolog_nd(apll_nd))).


%%**********************************************************************
%% AGENTS CREATION
%%**********************************************************************


:- pred create_agents(+int).

:- comment(create_agents(N), "Adds @var{N} new agents to the
   system.").

create_agents(0) :- !.
create_agents(N) :-
        N > 0,
        create_agent,
        N1 is N - 1,
        create_agents(N1).


:- pred ensure_agents(+int).

:- comment(ensure_agents(N), "Creates as many agents as necessary to
   ensure @var{N} agents in the system.").

ensure_agents(N) :-
	number_agents(N1),
	(
	    N1 < N ->
	    N2 is N - N1
	;
	    N2 is 0
	),
	create_agents(N2).


:- pred create_agent # "Creates a new agent.".

create_agent :-
        start_thread(agent).


:- pred agent.

:- comment(agent, "The agent performs some work indefinitely,
   based on the goal scheduling defined by @var{GS}.").

agent :-
	repeat,
	'$metachoice'(MC),
	agent_(MC).

agent_(MC) :-
	save_end_execution,
	enter_mutex_self,
	work(MC),
	agent_(MC).

agent_(MC) :-
	agent_(MC).


:- pred work(+int).

:- comment(work(+MC), "Checks whether there is some backtracking
   to perform over a particular goal or searches for a new parallel
   goal in the system to be executed, based on the goal scheduling
   defined by @var{GS}. Otherwise, the agent will suspend.").

work(MC) :-
	(
	    read_event(Handler) ->
 %% 	    (
 %% 		goal_toreexecute(Handler) ->
 %% 		exit_mutex_self,
 %% 		save_init_execution(Handler),
 %% 		call_handler(Handler)
 %% 	    ;
 %% 		(
 %% 		    \+goal_cancelled(Handler) ->
 %% 		    (
 %% 			more_solutions(Handler) ->
	    exit_mutex_self,
	    move_execution_top(Handler),
	    fail
 %% 		    ;
 %% 			move_pointers_down(Handler)
 %% 		    )
 %% 		;
 %% 		    display(cancelled),nl,
 %% 		    exit_mutex_self
 %% 		),
 %% 		    fail
 %% 	    )
	;
	    (
		find_goal(H) ->
		(
		    goal_det(H) ->
		    exit_mutex_self,
		    call_det_handler(H),
		    enter_mutex_self,
		    work(MC)
		;
 		    exit_mutex_self,
 		    save_init_execution(H),
 		    call_handler(H)
		)
	    ;
		(
  		    suspend ->
		    work(MC)
		;
		    exit_mutex_self,
  		    '$metacut'(MC),
		    fail
		)
	    )
        ).

