:- package(prelude).

:- '$props'('$initialization'/1, [impnat=bytecode]).
:- multifile '$initialization'/1.
:- '$props'('$on_abort'/1, [impnat=bytecode]).
:- multifile '$on_abort'/1.

:- '$forbid_def'(fail/0).
:- '$forbid_def'(true/0).
:- '$forbid_def'(','/2).
:- '$forbid_def'((;)/2).
:- '$forbid_def'((->)/2).
:- '$forbid_def'((\+)/1).
:- '$forbid_def'(if/3).
:- '$forbid_def'((^)/2).
:- '$forbid_def'((=)/2).

% TODO: include in every module?
% definitions for ptoc and ptoc__analysis
:- '$native_weak_inline'(include('engine/basiccontrol.native.h')).
:- include(engine(ptoc__prelude_types)).
:- include(engine(ptoc__prelude)).
%:- '$pragma'(analyze_all).

:- '$pragma'(analyze_idet).



