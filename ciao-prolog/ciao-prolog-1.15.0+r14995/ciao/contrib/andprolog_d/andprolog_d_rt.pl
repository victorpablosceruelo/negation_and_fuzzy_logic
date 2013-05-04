:- module(
        andprolog_d_rt,
        [
	    '&'/2,
	    '&!'/2,
	    '&>'/2,
	    '&!>'/2,
	    '<&'/1,
	    '<&!'/1,
	    '&'/1,
	    '&!'/1,
	    '&&'/2,
	    '&&!'/2,
	    '&&'/1,
	    '&&!'/1,
	    '&&>'/2,
	    '&&!>'/2,
	    '<&&'/1,
	    '<&&!'/1
	],
	[assertions, isomodes]
	 ).

:- include(andprolog_d_ops).

:- use_module(library(odd), [undo/1]).
:- use_module(library(prolog_sys), [new_atom/1]).
:- use_module(agents_d).
:- use_module(common).

:- doc(title,  "And-parallel execution").
:- doc(author, "Amadeo Casas"||
                   " (@tt{http://www.cs.unm.edu/~amadeo},"||
                   " University of New Mexico)").
:- doc(usage,  "This library allows and-parallel execution of goals in
                    (Herbrand-)independent fashion. It resembles the
                    execution rules of &-Prolog.").


%%***************************************************************************


:- pred '&>'(+callable,-handler).

:- doc('&>'(X, H), "Sends out goal @var{X}, to be executed potentially by
   another worker of the team, returning in @var{H} a handler of the goal
   sent.").

:- meta_predicate((goal&>_)).
Goal &> goal_info(TaskId, Goal) :-
	new_atom(TaskId),
	publish_goal(TaskId, Goal, nondet).

:- pred '&!>'(+callable,-handler).

:- doc('&!>'(X, H), "Sends out the deterministic goal @var{X}, to be
   executed potentially by another worker of the team, returning in @var{H} a
   handler of the goal sent.").

:- meta_predicate((goal'&!>'_)).
Goal '&!>' goal_info(TaskId, Goal) :-
	new_atom(TaskId),
	publish_goal(TaskId, Goal, det).

:- pred '&&>'(+callable,-handler).

:- doc('&&>'(X, H), "Fair version of the &>/2 operator. If there is no
   idle worker, create one to execute goal @var{X}. This way, fairness among
   concurrent threads is ensured.").

:- meta_predicate((goal&&>_)).
Goal &&> goal_info(TaskId, Goal) :-
	Goal &> goal_info(TaskId, Goal).

:- pred '&&!>'(+callable,-handler).

:- doc('&&!>'(X, H), "Fair version of the '&!>'/2 operator. If there is no
   idle worker, create one to execute goal @var{X}.").

:- meta_predicate((goal'&&!>'_)).
Goal '&&!>' goal_info(TaskId, Goal) :-
	Goal '&!>' goal_info(TaskId, Goal).

:- pred '<&'(+handler).

:- doc('<&'(H), "Gets the result of the goal pointed to by @var{H}, or
   executes it if it has not been executed yet. Backtracking of the goal will
   be done at this point.").

goal_info(TaskId, Goal) <& :-
	goal_info(TaskId, Goal) <&& .

:- pred '<&!'(+handler).

:- doc('<&!'(H), "Gets the result of the deterministic goal pointed to by
   @var{H}, or executes it if it has not been executed yet.").

goal_info(TaskId, Goal) '<&!' :-
	goal_info(TaskId, Goal) '<&&!' .

:- pred '<&&'(+handler).

:- doc('<&&'(H), "Fair version of the <&/1 operator.").

goal_info(TaskId, Goal) <&& :-
	retrieve_goal(TaskId, Goal, nondet),
	!,
	undo(publish_goal(TaskId, Goal, nondet)),
	call(Goal).
goal_info(TaskId, Goal) <&& :-
	retract_fact(goal_solution(TaskId, Solution, H, P, _)),
	!,
	(
	    Solution == 'end' ->
	    publish_goal(TaskId, Goal, nondet),
	    fail
	;
	    processing_generated_solution(Solution, Goal, H, P, TaskId)
	).

:- pred '<&&!'(+handler).

:- doc('<&&!'(H), "Fair version of the '<&!'/1 operator.").

goal_info(TaskId, Goal) '<&&!' :-
	retrieve_goal(TaskId, Goal, det),
	!,
	call(Goal).
goal_info(TaskId, Goal) '<&&!' :-
	retract_fact(goal_solution(TaskId, Goal, _, _, _)),
	!.

:- pred processing_generated_solution(+callable,+callable,+wam_id,+port_id,
	+task_id).

:- doc(processing_generated_solution(Solution, Goal, WamId, Port), "Uses
   the obtained solution @var{Solution} of the goal @var{Goal} or asks for a
   new solution of it.").

processing_generated_solution(S, S, _, _, _).
processing_generated_solution(_, _, Host, Port, TaskId) :-
	backtrack_goal(TaskId, Host, Port),
	fail.

:- pred '&'(+callable,+callable).

:- doc('&'(X, Y), "Performs a parallel fork of the two goals @var{X} and
   @var{Y} involved and waits for the execution of both to finish. If no
   workers are idle then the two goals may be executed by the same worker and
   sequentially, i.e., one after the other.").

:- meta_predicate((goal&goal)).
GoalA & GoalB :-
	GoalB &> H,
	GoalA,
	H <& .

:- pred '&!'(+callable,+callable).

:- doc('&!'(X, Y), "Performs a parallel fork of the two deterministic
   goals @var{X} and @var{Y} involved and waits for the execution of both to
   finish.").

:- meta_predicate((goal'&!'goal)).
GoalA '&!' GoalB :-
	GoalB '&!>' H,
	GoalA,
	H '<&!' .

:- pred '&&'(+callable,+callable).

:- doc('&&'(X, Y), "Fair version of the '&'/2 operator.").

:- meta_predicate((goal&&goal)).
GoalA && GoalB :-
	GoalB &&> H,
	GoalA,
	H <&& .

:- pred '&&!'(+callable,+callable).

:- doc('&&!'(X, Y), "Fair version of the '&!'/2 operator.").

:- meta_predicate((goal'&&!'goal)).
GoalA '&&!' GoalB :-
	GoalB '&&!>' H,
	GoalA,
	H '<&&!' .

:- pred '&'(+callable).

:- doc('&'(X), "Sends out goal @var{X} to be executed potentially by
   another worker of the team. No waiting for its return is performed. Updates
   on the variables of @var{X} will be exported to other workers sharing
   them.").

:- meta_predicate((goal&)).
Goal & :-
	Goal &> _ .

:- pred '&!'(+callable).

:- doc('&!'(X), "Sends out deterministic goal @var{X} to be executed
   potentially by another worker of the team. No waiting for its return is
   performed. Updates on the variables of @var{X} will be exported to other
   workers sharing them.").

:- meta_predicate((goal'&!')).
Goal '&!' :-
	Goal '&!>' _ .

:- pred '&&'(+callable).

:- doc('&&'(X), "Fair version of the '&'/2 operator.").

:- meta_predicate((goal&&)).
Goal && :-
	Goal &&> _ .

:- pred '&&!'(+callable).

:- doc('&&!'(X), "Fair version of the '&!'/2 operator.").

:- meta_predicate((goal'&&!')).
Goal '&&!' :-
	Goal '&&!>' _ .


