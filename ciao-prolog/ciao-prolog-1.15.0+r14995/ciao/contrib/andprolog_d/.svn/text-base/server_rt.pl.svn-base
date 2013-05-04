:- module(server_rt, [main/0], [assertions, isomodes]).

:- use_module(library(concurrency), 
	[
	    lock_atom/1,
	    unlock_atom/1,
	    eng_call/4
	]).
:- use_module(library(sockets), [bind_socket/3, socket_accept/2]).
:- use_module(library(remote(read_and_write))).
:- use_module(common).

:- concurrent process/2.
:- concurrent connection/1.

:- data port_mut_ex/0.
:- data max_port/1.

max_port(3855).


%%***************************************************************************


:- pred main # "Throws a thread which will behave as a blackboard for the
   goals published to be executed in parallel.".

main :-
	eng_call(handler, create, create, _),
	central_thread_socket_port(Port),
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

handle_stream_(port, Stream) :-
	lock_atom(port_mut_ex),
	retract_fact(max_port(Port)),
	Port1 is Port + 1,
	assertz_fact(max_port(Port1)),
	unlock_atom(port_mut_ex),
	remote_write(Stream, Port1),
	debug_display("   Port sent: ~w~n", [Port1]),
	eng_call(handler, create, create, _),
	!.
handle_stream_(new, Stream) :-
	remote_read(Stream, Host),
	remote_read(Stream, P),
	(
	    current_fact_nb(process(Host, P)) -> true
	;
	    assertz_fact(process(Host, P)),
	    debug_display("   New host ~w, port ~w with goals~n", [Host, P])
	),
	!.
handle_stream_(nomore, Stream) :-
	remote_read(Stream, Host),
	remote_read(Stream, P),
	(
	    retract_fact_nb(process(Host, P)) ->
	    debug_display("   Host ~w, port ~w with no goals~n", [Host, P])
	;
	    true
	),
	!.
handle_stream_(get, Stream) :-
	current_fact(process(Host, Port)),
	remote_write(Stream, Host),
	remote_write(Stream, Port),
	debug_display("   Host ~w, port ~w sent ~n", [Host, Port]),
	!.


