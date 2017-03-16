:- module(basiccontrol, [], [pure, assertions, isomodes]).

:- use_module(engine(basic_props)). % TODO: only for assertions

:- doc(title,"Control constructs/predicates").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(module,"This module contains the set of basic control
   predicates, except the predicates dealing with exceptions, which are
   in @ref{Exception handling}.").

:- doc(usage, "These predicates/constructs are builtin in Ciao, so
   nothing special has to be done to use them.  In fact, as they are
   hardwired in some parts of the system, most of them cannot be redefined.").

% TODO: DOCUMENT! Indeed, this is the starting point for the emulator definition
:- '$native_include_c_header'(.(basiccontrol)).
:- '$native_weak_inline'(include('engine/basiccontrol.h')).

%:- use_module(engine(interpreter), [interpret_body/1]).
:- import(interpreter, [interpret_body/1]).

:- export(','/2).
:- doc(','(P,Q), "Conjunction (@var{P} @em{and} @var{Q}).").
:- true pred ','(+callable,+callable) + iso.
:- '$allow_def'(','/2).
:- meta_predicate(','(primitive(goal), primitive(goal))).
(X, Y) :- interpret_body('basiccontrol:,'(X, Y)).
%(X, Y) :- '$meta_call'(X), '$meta_call'(Y).

:- export(';'/2).
:- doc(';'(P,Q), "Disjunction (@var{P} @em{or} @var{Q}).").
:- true pred ';'(+callable,+callable) + iso.
:- meta_predicate(';'(primitive(goal), primitive(goal))).
:- '$allow_def'((;)/2).
(X;Y) :- interpret_body('basiccontrol:;'(X, Y)).
%(X;_) :- '$meta_call'(X).
%(_;Y) :- '$meta_call'(Y).

:- doc('|'/2, "An alias for disjunction (when appearing outside a
   list). The alias is performed when terms are read in.").

:- doc(doinclude, '|'/2). %% Implemented in reader.

:- export('->'/2).
:- doc('->'(P,Q), "If @var{P} then @var{Q} else fail, using first
   solution of @var{P} only.  Also, @tt{(}@var{P} @tt{->} @var{Q}
   @tt{;} @var{R}@tt{)}, if @var{P} then @var{Q} else @var{R}, using
   first solution of @var{P} only.  No cuts are allowed in @var{P}.").
:- true pred '->'(+callable,+callable) + iso.
:- meta_predicate('->'(primitive(goal), primitive(goal))).
:- '$allow_def'((->)/2).
(X->Y) :- interpret_body('basiccontrol:->'(X, Y)).

:- export(!/0).
:- true pred '!'/0 + iso
   # "Commit to any choices taken in the current predicate.". 
!. % TODO: interpreter hook, use a proper decl like inlinepred

:- export((\+)/1).
:- doc(\+(P), "Goal @var{P} is not provable (negation by failure).
   Fails if @var{P} has a solution, and succeeds otherwise.  No cuts are
   allowed in @var{P}.").
:- true pred \+(+callable) + ( iso, native(not(X)) ).
:- meta_predicate('\\+'(primitive(goal))).
:- '$allow_def'((\+)/1).
\+ X :- interpret_body('basiccontrol:\\+'(X)).

:- export(if/3).
:- doc(if(P,Q,R), "If @var{P} then @var{Q} else @var{R}, exploring
   all solutions of @var{P}.  No cuts are allowed in @var{P}."). 
:- true pred if(+callable,+callable,+callable).
:- meta_predicate(if(primitive(goal),primitive(goal),primitive(goal))).
:- '$allow_def'(if/3).
if(P, Q, R) :- interpret_body('basiccontrol:if'(P,Q,R)).

:- export(true/0).
:- true pred true/0 + ( iso, native ) # "Succeed (noop).".
:- '$allow_def'(true/0).
true. % TODO: interpreter hook, use a proper decl like inlinepred

:- export(otherwise/0).
:- '$props'(otherwise/0, [impnat=intrinsic]).

:- export(fail/0).
:- true pred fail/0 + ( iso, native ) # "Fail, backtrack immediately.".
:- '$allow_def'(fail/0).
fail :- fail. % TODO: interpreter hook, use a proper decl like inlinepred

:- export(false/0).
:- '$props'(false/0, [impnat=intrinsic]).

:- export(repeat/0).
:- true pred repeat/0 + ( iso, native ) # "Generates an infinite sequence of
   backtracking choices.".
:- '$props'(repeat/0, [impnat=cbool(prolog_repeat)]).

:- export('$metachoice'/1).
:- '$props'('$metachoice'/1, [impnat=cbool(metachoice)]).

:- export('$metacut'/1).
:- '$props'('$metacut'/1, [impnat=cbool(metacut)]).

% Built-ins handled directly by the compiler
:- export('$caller_choice'/1).
:- '$props'('$caller_choice'/1, [impnat=ptoc_builtin]).
:- export('$cut'/1).
:- '$props'('$cut'/1, [impnat=ptoc_builtin]).

% TODO: export?
:- export('IF BUILTIN'/1).
:- '$props'('IF BUILTIN'/1, [impnat=cblt(bu1_if,0)]).

% ----------------------------------------------------------------------------
% Bytecode disassembler 

:- export('$disasm'/1).
:- '$props'('$disasm'/1, [impnat=cbool(disasm)]).

:- export('$dectok'/1).
:- '$props'('$dectok'/1, [impnat=cbool(dectok)]).

% ----------------------------------------------------------------------------
% The emulator

% TODO: DOCUMENT! Indeed, this is the starting point for the emulator definition
:- '$pragma'('$emit_emulator').
