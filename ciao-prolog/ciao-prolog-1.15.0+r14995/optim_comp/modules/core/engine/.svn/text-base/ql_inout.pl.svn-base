:- module(ql_inout, [], [pure]).

:- '$native_include_c_source'(.(ql_inout)).

% TODO: note: read only supports abscurr
:- export('$qread_begin'/1).
:- '$props'('$qread_begin'/1, [impnat=cbool(prolog_qread_begin)]).
:- export('$qread_end'/0).
:- '$props'('$qread_end'/0, [impnat=cbool(qread_end)]).
:- export('$qread'/1).
:- '$props'('$qread'/1, [impnat=cbool(prolog_qread)]).

% TODO: first argument is the absmach (0: abscurr, 1: absnext)
:- export('$qwrite_begin'/2).
:- '$props'('$qwrite_begin'/2, [impnat=cbool(qwrite_begin)]).
:- export('$qwrite_end'/0).
:- '$props'('$qwrite_end'/0, [impnat=cbool(qwrite_end)]).
:- export('$qwrite'/1).
:- '$props'('$qwrite'/1, [impnat=cbool(qwrite)]).
:- export('$qwrite_b'/1).
:- '$props'('$qwrite_b'/1, [impnat=cbool(qwrite_b)]). % writes a bytecode clause

% TODO: move to compiler/absmach.pl?
:- export('$absnext__set'/8).
:- '$props'('$absnext__set'/8, [impnat=cbool(absnext__set)]).

