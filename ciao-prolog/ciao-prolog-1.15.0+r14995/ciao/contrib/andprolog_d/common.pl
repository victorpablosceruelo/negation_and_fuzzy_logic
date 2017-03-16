:- module(common, _, [assertions]).

:- reexport(library(assertions(native_props)), [indep/1,indep/2]).

:- include(andprolog_d_ops).

:- use_module(library(concurrency), [lock_atom/1, unlock_atom/1]).
:- use_module(library(format), [format/2]).

:- set_prolog_flag(multi_arity_warnings, off).

:- data display_mut_ex/0.
:- data debug/0.

:- concurrent goal_stack/3.
:- concurrent goal_solution/5.

debug.

%current_server_host('blade001.js20.unm.edu').
current_server_host('localhost').
central_thread_socket_port(3855).

debug_display(Text, Args) :-
	debug,
	!,
	lock_atom(display_mut_ex),
	format(Text, Args),
	unlock_atom(display_mut_ex).
debug_display(_,_).


%%***************************************************************************


:- prop andcallable/1.

:- doc(andcallable(Exp), "@var{Exp} is of the form @tt{goal&goal}.").

andcallable(A&B):- callable(A), callable(B).

:- prop task_id/1.

:- doc(task_id(X), "@var{X} is the id of the goal published to be
   executed concurrently.").

task_id(X) :- atom(X).

:- doc(wam_id(Id), "@var{Id} is the id of the WAM where the goal is being
   or has been executed.").

:- prop wam_id/1.

wam_id(Id) :- Id = '$goal_id'(_,_).

:- prop host_id/1.

:- doc(host_id(X), "@var{X} is the name of a host.").

host_id(X) :- atom(X).

:- doc(port_id(X), "@var{X} is the port where a thread is listening to.").

:- prop port_id/1.

port_id(X) :- number(X).

:- prop handler/1.

:- doc(handler(H), "@var{H} is a variable which stores a handler of a
   goal sent to be executed by other worker.").

handler(H) :-
	H = goal_info(TaskId, Goal),
	task_id(TaskId),
	callable(Goal).

:- prop action/1.

:- doc(action(Act), "@var{Act} specifies if the goal has to be executed
   or backtracked over.").

action(Act) :- (Act = call ; Act = backtrack).

:- prop det/1.

:- doc(det(D), "@var{D} shows whether the goal to be executed is
   deterministic or not.").

det(D) :- (D = det ; D = nondet).


