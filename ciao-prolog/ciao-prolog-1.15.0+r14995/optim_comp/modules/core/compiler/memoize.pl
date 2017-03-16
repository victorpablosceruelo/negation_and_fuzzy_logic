:- class(memoize, [], [pure, compiler(complang)]).

% Function memoization with persistence of results (saved/restored
% from disk) and incremental evaluation.

% TODO: better name?
% TODO: define hooks to get element date
% TODO: rename element by action result hmmm

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(exceptions)).
:- use_module(engine(arithmetic)).
:- use_module(engine(basic_props)).

:- use_module(compiler(errlog)).
:- '$trust_statemodel'(errlog, single).

:- attr errs :: errlog.

% ---------------------------------------------------------------------------
% Create, delete or clear a memoize object

% Constructor
:- constructor new_/1.
new_(Errs) :- ~errs = Errs, add(working(0)).

:- public delete/0.
delete :-
	memoize:clear,
	'$inst_destroy'(~self).

% Clean the make session (fast way to reuse memoize objects)
:- public clear/0.
clear :-
	errs.clear,
	del(status(_, _)),
	del(working(_)), add(working(0)).

:- pred working(Depth).
:- data working/1.
:- public enter/0.
enter :-
	( working(N0) -> true ),
	N is N0 + 1,
	del(working(_)), add(working(N)). % TODO: implement set
:- public leave/0.
leave :-
	( working(N0) -> true ),
	N is N0 - 1,
	del(working(_)), add(working(N)),
	( N = 0 ->
	    errs.summary,
	    memoize:clear
	; true
	).

% ---------------------------------------------------------------------------

% TODO: implement garbage collection of objects

% Bind wanted with ret elements (and memoize results)
bind_ret(Wanted, Ret) :-
	ret(Ret, Wanted), % TODO: may fail... (i.e. no free space) 
	get_wanted(Wanted).

% set the return value of Action to Ref
ret([Action=Ref|Xs], Wanted) :- !,
	ret__1(Action, Ref, Wanted),
	ret(Xs, Wanted).
ret([], _).

ret__1(Action, Ref, Wanted) :-
	action__save(Action, Ref),
	( bind_wanted(Wanted, Action, Ref) ->
	    true
	; '$inst_destroy'(Ref)
	).

get_wanted([Action=Ref]) :- var(Ref), !,
	last_value(Action, Ref).
get_wanted(_).

bind_wanted([Action=Ref], Action, Ref) :- !.

% Notify the finalization of a given element (used by the element writers)
% Automatically sets creation time and updated.
% TODO: this predicate can also be used to touch frozen elements and force the update of dependant files (without really modifying the filesystem)
:- public fake_updated/1.
fake_updated(Action) :-
	action__key(Action, Key),
	set_status(Key, done).

% ---------------------------------------------------------------------------

:- public eval/2.
% Update and get the value of Action
% note: fails on error
% note: only valid for actions that returns something
eval(Action, Ref) :-
	do(Action, [Action=Ref]).

:- public eval0/1.
% Update and get the value of Action
% note: fails on error
eval0(Action) :-
	do(Action, []). % TODO: obtain here the Ref... (do not reload)

% ---------------------------------------------------------------------------

:- public last_value/2.
% Reference an element (without updating).
% note: fails if last value is unknown
% note: only valid for actions that returns something
last_value(Action, Ref) :-
	action__restore(Action, Ref).

% ---------------------------------------------------------------------------
% Incremental evaluation of action

% Status of each Action
:- pred status(Key, Status).
:- data status/2.

% set the status
set_status(Key, Status) :-
	del(status(Key, _)),
	add(status(Key, Status)).

% Ensure that an element is updated.
% (Valid for in-memory and in-disk objects).

% Precondition: element terminal inputs must not be modified until the session ends, because elements that are finished within the current session are considered updated (fresh).

% TODO: see if this case fails...
%   1) a is generated from b and depends on b and e
%   2) e depends on c
%   3) b source changes and now, a depends on d instead of e, but remove c
%   4) try to update a:
%         the old dependency says that a depends on b and e,
%         the system checks if any input is outdated or inconsistent
%         4.1) - b does not needs updates
%         4.2) - e 'ghost' update failed

do(Action, Wanted) :-
	( action__output(Action, Action2) ->
	    true
	; Action = Action2
	),
	mark_status(Action2, Status),
	do__2(Status, Action2, Ret),
	bind_ret(Wanted, Ret).

% TODO: pass what you want to do/1 and obtain it... do not
%   destroy it in ret, destroy only unwanted things...
%   then the next step would be ...

do__2(done, _, []) :- !.
do__2(updated(_), _, []) :- !.
do__2(undefined, Action, _) :- !,
	errs.compiler_error(not_found(Action)),
	fail.
do__2(pending, Action, _) :- !,
	% TODO: if this is not a bug, write a good explanation
	errs.compiler_error(bug(loop(Action))),
	fail.
do__2(error, _, _) :- !,
	errs.add_module_error, % silently remember that we had an error
	fail.
do__2(outdated, Action, Ret) :- !,
	action__key(Action, Key),
	set_status(Key, pending),
	Self = ~self,
	memo :: memoize <- Self,
	( errs.protect(Action, action__do(Action, Ret)) ->
%	    '$show_mempred',
%	    statistics,
	    set_status(Key, done)
	; set_status(Key, error),
	  fail
	).

% TODO: active PROFILE__MEMPRED in basiccontrol and study where the memory goes... also implement spec->speckey in C to avoid unnecessary atoms...
%:- use_module(library(prolog_sys), [statistics/0]).
%:- '$props'('$show_mempred'/0, [impnat=cbool(show_mempred)]).

% obtain the status of the action and all required actions
mark_status(Action, Status2) :-
	action__frozen(Action),
	!,
	Status2 = updated(1). % bottom time
mark_status(Action, Status2) :-
	action__key(Action, Key),
	( status(Key, Status2) ->
	    true
	; ( action__timestamp(Action, Time) ->
	      mark_status__old(Time, Action, Status2)
	  ; mark_status__missing(Action, Status2)
	  ),
	  set_status(Key, Status2)
	).

mark_status__missing(Action, Status2) :- !,
	( action__terminal(Action) ->
	    Status2 = undefined
	; Status2 = outdated
	).

% TODO: check and document: it is not necessary to set the status to pending in mark_status__old, because that predicate checks the status of a existing build and that build cannot have loops 
mark_status__old(Time, Action, Status2) :-
	action__terminal(Action),
	!,
	Status2 = updated(Time).
mark_status__old(Time, Action, Status2) :-
	% check for updates
	Memoize = ~self,
	memo :: memoize <- Memoize,
	( action__input(Action, Used) ->
	    ( ( member(Action0, Used),
	         mark_status(Action0, Status0),
	        ( Status0 = done
	        ; Status0 = outdated
	        ; Status0 = updated(Time0),
	          Time0 > Time
	        )
	      % yes, at least one input needs update, is erroneous, or is newer
	      ) ->
	        Status2 = outdated
	    ; % mark as updated
	      Status2 = updated(Time)
	    )
	; % action__input failed
          Status2 = outdated
	).

% TODO: find a way to specify action parameters + recompilation 
%       (may be a .conf file and an extra input dependency?)
% TODO: find a way to specify more than one callback definition
:- include(.(memoize__callback)).
