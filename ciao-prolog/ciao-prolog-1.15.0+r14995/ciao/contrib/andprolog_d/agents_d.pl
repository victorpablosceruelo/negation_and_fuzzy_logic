:- module(
        agents_d,
        [
	    publish_goal/3,
	    backtrack_goal/3,
	    retrieve_goal/3,
	    goal_thread/0,
	    main/0
	],
	[assertions, isomodes]
	 ).

:- use_module(library(concurrency), 
	[
	    eng_call/4,
	    eng_goal_id/1
	]).
:- use_module(library(sockets), 
	[
	    connect_to_socket/3,
	    bind_socket/3,
	    socket_accept/2
	]).
:- use_module(library(system), [current_host/1]).
:- use_module(library(remote(read_and_write))).
:- use_module(common).

:- concurrent goal_backtrack/2.
:- concurrent connection/1.

:- data port/1.
% :- data code/0.


%%***************************************************************************


:- pred main # "Throws a new thread that obtains a new goal to execute (or to
   backtrack over) and then returns a solution for it. When it completes these
   operations the thread waits again until it obtains a new goal to execute.".

main :-
	goal_thread,
	repeat,
	get_goal_to_execute,
	fail.

:- pred goal_thread # "Throws a new thread that will be listening to new
   connections in an specific port in the machine.".

goal_thread :-
	current_host(Host),
	central_thread_socket_port(PortSocket),
	connect_to_socket(Host, PortSocket, Stream),
	remote_write(Stream, port),
	remote_read(Stream, Port),
	close(Stream),
	assertz_fact(port(Port)),
	debug_display("Port: ~w~n", [Port]),
	eng_call(handle(Port), create, create, _).

handle(Port) :-
	eng_call(handler, create, create, _),
	bind_socket(Port, 100, Socket),
        repeat,
        socket_accept(Socket, Stream),
	assertz_fact(connection(Stream)),
        fail.

handler :-
	retract_fact(connection(Stream)),
	handle_stream(Stream),
	fail.

handle_stream(Stream) :-
	remote_read(Stream, Value),
        debug_display("Received: ~w~n", [Value]),
	handle_stream_(Value, Stream),
	close(Stream).

handle_stream_(goal, Stream) :-
	(
	    retract_fact_nb(goal_stack(TaskId, Goal, Det)) ->
	    inform_no_more_goals,
	    remote_write(Stream, yes),
	    GS =.. [goal_stack, TaskId, Goal, Det],
	    remote_write(Stream, GS),
	    debug_display("Goal ~w sent~n", [TaskId])
	;
	    inform_no_more_goals,
	    remote_write(Stream, no)
	),
	!.
handle_stream_(goal_solution, Stream) :-
	remote_read(Stream, Solution),
	Solution =.. [goal_solution, TaskId|_],
	assertz_fact(Solution),
	debug_display("Solution for TaskId ~w received~n", [TaskId]),
	!.
handle_stream_(backtrack, Stream) :-
	remote_read(Stream, TaskId),
	assertz_fact(goal_backtrack(TaskId, _)),
	debug_display("TaskId ~w to backtrack~n", [TaskId]),
	!.
handle_stream_(code, _Stream) :-
	true,
	!.

:- pred publish_goal(+callable,+task_id,+det).

:- doc(publish_goal(Goal, TaskId, Det), "Publishes a new goal @var{Goal}
   with id @var{TaskId}. @var{Det} determines whether the goal is
   deterministic or not.").

publish_goal(TaskId, Goal, Det) :-
	inform_new_goals,
	assertz_fact(goal_stack(TaskId, Goal, Det)),
	!.

:- pred retrieve_goal(+task_id,+callable,+det).

:- doc(retrieve_goal(TaskId, Goal, Det), "Retrieves a new goal @var{Goal}
   with id @var{TaskId} that has not been executed yet.").

retrieve_goal(TaskId, Goal, Det) :-
	retract_fact_nb(goal_stack(TaskId, Goal, Det)),
	inform_no_more_goals.

:- pred inform_new_goals.

:- doc(inform_new_goals, "Inform the server that new goals are ready to be
   executed.").

inform_new_goals :-
	(
	    current_fact_nb(goal_stack(_, _, _)) ->
	    true
	;
	    current_server_host(HostServer),
	    central_thread_socket_port(PortSocket),
	    connect_to_socket(HostServer, PortSocket, StreamServer),
	    remote_write(StreamServer, new),
	    current_host(Host),
	    remote_write(StreamServer, Host),
	    port(Port),
	    remote_write(StreamServer, Port),
	    close(StreamServer),
	    debug_display("New goals ready.~n", [])
	).

:- pred inform_no_more_goals.

:- doc(inform_no_more_goals, "Inform the server that no more goals are
   ready to be executed.").

inform_no_more_goals :-
	(
	    current_fact_nb(goal_stack(_, _, _)) -> true
	;
	    current_server_host(HostServer),
	    central_thread_socket_port(PortSocket),
	    connect_to_socket(HostServer, PortSocket, StreamServer),
	    remote_write(StreamServer, nomore),
	    current_host(Host),
	    remote_write(StreamServer, Host),
	    port(Port),
	    remote_write(StreamServer, Port),
	    close(StreamServer),
	    debug_display("No more goals ready.~n", [])
	).

:- pred get_goal_to_execute # "Asks the server whether there is another
   process with some new goals to be executed.".

get_goal_to_execute :-
	current_server_host(HostServer),
	central_thread_socket_port(PortSocket),
	connect_to_socket(HostServer, PortSocket, StreamServer),
	debug_display("Asking for goal...~n", []),
	remote_write(StreamServer, get),
	remote_read(StreamServer, Host),
	remote_read(StreamServer, Port),
	close(StreamServer),
	debug_display("Thread to ask: ~w, port ~w~n", [Host, Port]),
	!,
	connect_to_socket(Host, Port, Stream),
	remote_write(Stream, goal),
	remote_read(Stream, X),
	(
	    X == no -> !, fail
	;
	    remote_read(Stream, GS),
	    get_source_code(Stream),
	    GS =.. [goal_stack, TaskId, Goal, Det],
	    debug_display("Goal ~w obtained!~n", [TaskId]),
	    action(Det, Goal, _, TaskId, Host, Port)
	),
	close(Stream),
	!.

get_source_code(_Stream) :-
% 	(
% 	    code -> true
% 	;
% 	    get_and_assert_code(Stream, Goal),
% 	    assertz_fact(code)
% 	),
	true.

% get_and_assert_code(_Stream, Goal) :-
% 	debug_display("Code obtained and asserted for goal ~w~n", [Goal]).

% read_module(Program, Code) :-
%         open(Program, read, Stream),
%         read_module_(Stream, Code),
%         close(Stream).
% read_module_(Stream, [Term|Codes]) :-
%         read(Stream, Term),
%         (
%             (Term = end_of_file) -> (Codes = [])
%         ;
%             (read_module_(Stream, Codes))
%         ).

% send_code([end_of_file], Stream) :-
%         write(Stream, end_of_file),
%         write(Stream, '.'),
%         nl(Stream).
% send_code([Code|T], Stream) :-
%         write(Stream, Code),
%         write(Stream, '.'),
%         nl(Stream),
%         send_code(T, Stream).

% receive_code(Stream, FileName) :-
%         open(FileName, write, FileStream),
%         receive_code_(Stream, FileStream),
%         close(FileStream).
% receive_code_(Stream, FileStream) :-
%         read_term(Stream, Code, []),
%         (
%             (Code = end_of_file) -> ( nl(FileStream) )
%         ;
%             (
%                 print(FileStream, Code),
%                 print(FileStream, '.'),
%                 nl(FileStream),
%                 receive_code_(Stream, FileStream)
%             )
%         ).

:- pred send_solution(+task_id,+host_id,+port_id,+callable,+wam_id).

:- doc(send_solution(TaskId, Host, Port, Goal, WamId), "Sends the solution
   @var{Goal} for the goal with id @var{TaskId}, and the id @var{WamId} of the
   wam where it was executed, to the port @var{Port}.").

send_solution(TaskId, Host, Port, Sol, WamId) :-
	connect_to_socket(Host, Port, Stream),
	remote_write(Stream, goal_solution),
	current_host(H),
	port(P),
	GS =.. [goal_solution, TaskId, Sol, H, P, WamId],
	remote_write(Stream, GS),
	close(Stream),
	debug_display("Solution sent for goal ~w~n", [TaskId]).

:- pred action(+action,+callable,+wam_id,+task_id,+host_id,+port_id).

:- doc(action(Det, Goal, WamId, TaskId, Host, Port), "A thread obtains a
   new solution of a goal @var{Goal}, which can be the first solution of the
   goal or a new solution after backtracking over the goal.").

action(det, Goal, _, TaskId, Host, Port) :-
	debug_display("Goal to execute: ~w~n", [TaskId]),
	call(Goal),
	debug_display("Goal executed: ~w~n", [TaskId]),
	send_solution(TaskId, Host, Port, Goal, _),
	!.
action(nondet, Goal, WamId, TaskId, H, P) :-
	eng_call(wrapper(Goal, WamId, TaskId, H, P), create, self, WamId).
% 	retract_fact(goal_backtrack(TaskId, End)),
% 	(
% 	    End == 'end' -> !, fail
% 	;
% 	    eng_backtrack(WamId, self)
% 	),
% 	fail.
action(nondet, _, _, _, _, _).

:- pred wrapper(+callable,+wam_id,+task_id,+host_id,+port_id).

:- doc(wrapper(Goal, WamId, TaskId, Host, Port), "Wraps the execution of
   the goal and sends the solution.").

wrapper(Goal, WamId, TaskId, Host, Port) :-
	debug_display("Goal to execute: ~w~n", [TaskId]),
	call(Goal),
	debug_display("Goal executed: ~w~n", [TaskId]),
	eng_goal_id(WamId),
	send_solution(TaskId, Host, Port, Goal, WamId).
wrapper(_, WamId, TaskId, Host, Port) :-
	eng_goal_id(WamId),
	assertz_fact(goal_backtrack(TaskId, 'end')),
	send_solution(TaskId, Host, Port, end, WamId).

:- pred backtrack_goal(+task_id,+host_id,+port_id).

:- doc(backtrack_goal(TaskId, Host, Port), "Backtracks in the goal with id
   @var{TaskId}. @var{Host} and @var{Port} specify where the WAM of the goal
   is.").

backtrack_goal(TaskId, Host, Port) :-
	connect_to_socket(Host, Port, Stream),
	remote_write(Stream, backtrack),
	remote_write(Stream, TaskId),
	close(Stream),
	debug_display("TaskId to backtrack: ~w~n", [TaskId]),
	!.


