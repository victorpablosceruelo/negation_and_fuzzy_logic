:- module(callh_rt_nd,
	[
	    call_handler/1,
	    call_det_handler/1
	],
	[assertions, isomodes]).

:- include(library(andprolog_nd(andprolog_nd_ops))).
:- use_module(library(andprolog_nd(andprolog_nd_props))).

:- use_module(library(andprolog_nd(apll_nd))).


%%**********************************************************************
%% CALL_HANDLER/1
%%**********************************************************************


:- pred call_handler(+handler).

:- comment(call_handler(+Handler), "Calls the parallel goal associated
   to @var{Handler}.").

call_handler(Handler) :-
	retrieve_goal(Handler,Goal),
	call(Goal),
	set_goal_finished(Handler), %safe: noone else is writting
	enter_mutex(Handler),
	release(Handler),
	exit_mutex(Handler).
call_handler(Handler) :-
	set_goal_failed(Handler), %safe: noone else is writting
	enter_mutex(Handler),
	release(Handler),
 %% 	metacut_garbage_slots(Handler),
	exit_mutex(Handler),
	fail.


:- pred call_det_handler(+handler).

:- comment(call_det_handler(+Handler), "Calls the deterministic
   parallel goal associated to @var{Handler}.").

call_det_handler(Handler) :-
	retrieve_goal(Handler,Goal),
	(
	    call(Goal) ->
	    set_goal_finished(Handler), %safe: noone else is writting
	    enter_mutex(Handler),
	    release(Handler),
	    exit_mutex(Handler)
	;
	    set_goal_failed(Handler), %safe: noone else is writting
	    enter_mutex(Handler),
	    release(Handler),
	    exit_mutex(Handler),
	    fail
	).

