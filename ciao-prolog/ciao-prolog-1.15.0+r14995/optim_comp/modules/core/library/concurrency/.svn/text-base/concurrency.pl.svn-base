:- module(concurrency, [], [pure, assertions, isomodes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(arithmetic)).
:- use_module(engine(data_facts)).
:- use_module(engine(exceptions)).

:- doc(title,"Low-level concurrency/multithreading primitives").

:- doc(author,"Manuel Carro").

:- doc(module, "This module provides basic mechanisms for using
concurrency and implementing multi-goal
applications.@cindex{concurrency} It provides a means for arbitrary
goals to be specified to be run in a separate stack set; in that case,
they are assigned a goal identifier with which further accesses (e.g.,
asking for more solutions) to the goal can be made. Additionally, in
some architectures, these goals can be assigned an O.S. thread,
separate from the one which made the initial call, thus providing
concurrency and, in multiprocessors, parallelism capabilities.

As for now, the memory space of the threads (c.f., stack sets) is
separate in the sense that goals are copied to the new stack set, and
bindings of variables are not seen among stack sets which allows
forward and backward execution to proceed independently in each stack
set, at the cost of the initial goal copy.  However, the program space
(including, specially, the @concept{concurrent predicates}) are shared
and seen by all the goals and threads, and should be used as the
primary means of communication and synchronization.  Higer level
libraries can be built using these basic blocks.

Additionally, a small set of lock primitives are provided.  Locks are
associated with atom names. Whereas the concurrent database facilities
are enough to implement locks, semaphores, messages, etc., the
predicates implementing atom-based locks are faster than the ones
accessing the concurrent database (but they are less powerful).

").

% :- doc(bug, "@bf{This library is being rewritten at the moment.
% The Prolog interface herein described may not correspond exactly with
% the existing implementation. This note will be removed when the
% documentation corresponds to the implementation.}").

:- doc(bug, "Available only for Windows 32 environments and for
architectures implementing POSIX threads.").

:- doc(bug, "Some implementation of threads have a limit on the
total number of threads that can be created by a process.  Thread
creation, in this case, just hangs. A better solution is planned for
the future.").

:- doc(bug, "Creating many concurrent facts may fill up the atom
   table, causing Ciao Prolog to abort.").

% :- doc(bug, "The thread-related part of the library is not yet
% final, an some characteristics may be revised in a near future.  A
% higher level library for concurrency and distribution based on the
% low-level primitives in this module is being implemented.  ").

:- '$native_include_c_source'(.(concurrency)).

% ---------------------------------------------------------------------------
:- export(eng_call/4).
:- pred eng_call(+Goal, +EngineCreation, +ThreadCreation, -GoalId) :
callable * atm * atm * int # "Calls @var{Goal} in a new engine (stack
set), possibly using a new thread, and returns a @var{GoalId} to
designate this new goal henceforth.  @var{EngineCreation} can be
either @tt{wait} or @tt{create}; the distinction is not yet
meaningful. @var{ThreadCreation} can be one of @tt{self}, @tt{wait},
or @tt{create}.  In the first case the creating thread is used to
execute @var{Goal}, and thus it has to wait until its first result or
failure.  The call will fail if @var{Goal} fails, and succeed
otherwise.  However, the call will always suceed when a remote thread
is started.  The space and identifiers reclaimed for the thread must
be explicitly deallocated by calling @pred{eng_release/1}.
@var{GoalId}s are unique in each execution of a Ciao Prolog program.".

:- meta_predicate(eng_call(primitive(goal), ?, ?, ?)).
eng_call(Goal, Eng, Thr, Id):- 
        var(Id),
        Id = '$goal_id'(GoalDescriptorId, _UniqueId),
%        Id = GoalDescriptorId,
        '$eng_call'(Goal, Eng, Thr, GoalDescriptorId, _UniqueId, true).

% :- meta_predicate('$eng_call'(primitive(goal), ?, ?, ?, ?, ?)).
:- '$props'('$eng_call'/6, [impnat=cbool(prolog_eng_call)]).

% ---------------------------------------------------------------------------
:- export(eng_call/3).
:- pred eng_call(+Goal, +EngineCreation, +ThreadCreation) : callable *
atm * atm # "Similar to @pred{eng_call/4}, but the thread (if
created) and stack areas are automatically released upon success or
failure of the goal.  No @var{GoalId} is provided for further
interaction with the goal.".

:- meta_predicate(eng_call(primitive(goal), ?, ?)).
eng_call(Goal, Eng, Thr):- 
        '$eng_call'(Goal, Eng, Thr, _Id, _UniqueId, fail).

% ---------------------------------------------------------------------------
:- export(eng_backtrack/2).
:- pred eng_backtrack(+GoalId, +ThreadCreation): int * atm # "Performs
backtracking on the goal designed by @var{GoalId}.  A new thread can
be used to perform backtracking, according to @var{ThreadCreation}
(same as in @pred{eng_call/4}).  Fails if the goal is backtracked over
by the local thread, and there are no more solutions.  Always succeeds
if executed by a remote thread. The engine is @bf{not} automatically
released up upon failure: @pred{eng_release/1} must be called to that
end.".

eng_backtrack(GoalId, ThreadCreation):-
        GoalId = '$goal_id'(GoalDesc, _UniqueId),
%        GoalId = GoalDesc,
        '$eng_backtrack'(GoalDesc, ThreadCreation).

:- '$props'('$eng_backtrack'/2, [impnat=cbool(prolog_eng_backtrack)]).

% ---------------------------------------------------------------------------
:- export(eng_cut/1).
:- pred eng_cut(+GoalId) : int # "Performs a @em{cut} in the execution
of the goal @var{GoalId}.  The next call to @pred{eng_backtrack/2}
will therefore backtrack all the way and fail.".

eng_cut(GoalId):-
        GoalId = '$goal_id'(GoalDesc, _UniqueId),
%        GoalId = GoalDesc,
        '$eng_cut'(GoalDesc).

:- '$props'('$eng_cut'/1, [impnat=cbool(prolog_eng_cut)]).

% ---------------------------------------------------------------------------
:- export(eng_release/1).
:- pred eng_release(+GoalId): int # "Cleans up and releases the engine
executing the goal designed by @var{GoalId}. The engine must be idle,
i.e., currently not exedcuting any goal.  @pred{eng_wait/1} can be
used to ensure this.".

eng_release(GoalId):-
        GoalId = '$goal_id'(GoalDesc, _),
%        GoalId = GoalDesc,
        '$eng_release'(GoalDesc).

:- '$props'('$eng_release'/1, [impnat=cbool(prolog_eng_release)]).

% ---------------------------------------------------------------------------
:- export(eng_wait/1).
:- pred eng_wait(+GoalId) : int # "Waits for the engine executing the
goal denoted by @var{GoalId} to finish the computation (i.e., it has
finished searching for a solution, either with success or failure).".

eng_wait(GoalId):-
        GoalId = '$goal_id'(GoalDesc, _),
%        GoalDesc = GoalId,
        '$eng_wait'(GoalDesc).

:- '$props'('$eng_wait'/1, [impnat=cbool(prolog_eng_wait)]).

% ---------------------------------------------------------------------------
:- export(eng_kill/1).
:- pred eng_kill(+GoalId): int # "Kills the thread executing
@var{GoalId} (if any), and frees the memory used up by the stack set.
Usually one should wait (@pred{eng_wait/1}) for a goal, and then
release it, but killing the thread explicitly allows recovering from
error states.  A goal cannot kill itself.  This feature should be used
with caution, because there are situations where killing a thread
might render the system in an unstable state.  Threads should
cooperate in their killing, but if the killed thread is blocked in a
I/O operation, or inside an internal critical region, this cooperation
is not possible and the system, although stopped, might very well end
up in a incosistent state.".

eng_kill(GoalId):-
        GoalId = '$goal_id'(GoalDesc, _),
%        GoalId = GoalDesc,
        '$eng_kill'(GoalDesc).

:- '$props'('$eng_kill'/1, [impnat=cbool(prolog_eng_kill)]).

% ---------------------------------------------------------------------------
:- export(eng_killothers/0).
:- pred eng_killothers # "Kills threads and releases stack sets of all
active goals, but the one calling @pred{eng_killothers}.  Again, a
safety measure.  The same cautions as with @pred{eng_kill/1} should be
taken.".

eng_killothers:- '$eng_killothers'.

:- '$props'('$eng_killothers'/0, [impnat=cbool(prolog_eng_killothers)]).

% ---------------------------------------------------------------------------
:- export(eng_status/0).
:- pred eng_status #"Prints to standard output the current status of
the stack sets.".

eng_status :- '$eng_status'.

:- '$props'('$eng_status'/0, [impnat=cbool(prolog_eng_status)]).

% ---------------------------------------------------------------------------
% :- export(eng_status1/1).
% :- '$props'('$eng_status1'/1, [impnat=cbool(prolog_eng_status1)]).

% ---------------------------------------------------------------------------
:- export(eng_goal_id/1).
:- pred eng_goal_id(?GoalId) : int #"@var{GoalId} is unified with the
identifier of the goal within which @pred{eng_goal_id/1} is executed.".

eng_goal_id(GoalId):- 
        GoalId = '$goal_id'(GoalDesc, _UniqueId),
%        GoalId = GoalDesc,
       '$eng_self'(GoalDesc, _UniqueId).

:- '$props'('$eng_self'/2, [impnat=cbool(prolog_eng_self)]).

 %% goal_id(UniqueId):-
 %%         GoalId = '$goal_id'(GoalDesc, UniqueId),
 %% %        GoalId = UniqueId,
 %%        '$eng_self'(_GoalDesc, UniqueId).

%% ---------------------------------------------------------------------------
% :- export(eng_clean/0).
% :- pred eng_clean # "Removes the stack sets not in use at this moment.
% This will probably be left to an extended garbage collection in a
% future.".
% :- '$props'(eng_clean/0, [impnat=cbool(prolog_eng_clean)]).

% ---------------------------------------------------------------------------
:- export(lock_atom/1).
:- pred lock_atom(+Atom) : atm #"The @concept{semaphore} associated to
@var{Atom} is accessed; if its value is nonzero, it is atomically
decremented and the execution of this thread proceeds.  Otherwise, the
goal waits until a nonzero value is reached.  The semaphore is then
atomically decremented and the execution of this thread proceeds.".

:- '$props'(lock_atom/1, [impnat=cbool(prolog_lock_atom)]).

% ---------------------------------------------------------------------------
:- export(unlock_atom/1).
:- pred unlock_atom(+Atom) : atm #"The @concept{semaphore} associated
to @var{Atom} is atomically incremented.".

:- '$props'(unlock_atom/1, [impnat=cbool(prolog_unlock_atom)]).

% ---------------------------------------------------------------------------
:- export(atom_lock_state/2).
:- pred atom_lock_state(+Atom, +Value) : atm * int #"Sets the
semaphore associated to @var{Atom} to @var{Value}.  This is usually
done at the beginning of the execution, but can be executed at any
time.  If not called, semaphore associated to atoms are by default
inited to 1.  It should be used with caution: arbitrary use can
transform programs using locks in a mess of internal relations.  The
change of a semaphore value in a place other than the initialization
stage of a program is @bf{not} among the allowed operations as defined
by Dijkstra @cite{dijkstra-semaphores,ben-ari}.".

:- pred atom_lock_state(+Atom, -Value) : atm * int #"Consults the
@var{Value} of the semaphore associated to @var{Atom}.  Use sparingly
and mainly as a medium to check state correctness.  Not among the
operations on semaphore by Djikstra.".

:- '$props'(atom_lock_state/2, [impnat=cbool(prolog_lock_atom_state)]).

% ---------------------------------------------------------------------------
% TODO: does not work! documenting a predicate defined in other module...
/*
:- pred concurrent(+Spec).
:- doc(concurrent(F/A), "The predicate named @var{F} with arity
@var{A} is made @concept{concurrent} in the current module at runtime
(useful for predicate names generated on-the-fly). This difficults a
better compile-time analysis, but in turn offers more flexibility to
applications.  It is also faster for some applications: if several
agents have to share data in a stuctured fashion (e.g., the generator
knows and wants to restrict the data generated to a set of other
threads), a possibility is to use the same concurrent fact and emply a
field within the fact to distinguish the receiver/sender.  This can
cause many threads to access and wait on the same fact, which in turns
can create contention problems.  It is much better to create a new
concurrent fact and to use that new name as a channel to communicate
the different threads.  @pred{concurrent/1} can either be given a
@concept{predicate spec} in the form @var{Name/Arity}, with @var{Name}
and @var{Arity} bound, or to give a value only to @var{Arity}, and let
the system choose a new, unused @var{Name} for the fact.").
:- doc(doinclude, concurrent/1).
*/
:- reexport(engine(rt_exp), [concurrent/1]).
