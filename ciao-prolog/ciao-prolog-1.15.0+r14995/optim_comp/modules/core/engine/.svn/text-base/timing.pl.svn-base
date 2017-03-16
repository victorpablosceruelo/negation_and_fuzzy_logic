:- module(timing, _, [pure]).

:- use_module(engine(basiccontrol)).

:- '$native_include_c_source'(.(timing)).

:- export('$runtime'/1).
:- '$props'('$runtime'/1, [impnat=cbool(prolog_runtime)]).
:- export('$usertime'/1).
:- '$props'('$usertime'/1, [impnat=cbool(prolog_usertime)]).
:- export('$systemtime'/1).
:- '$props'('$systemtime'/1, [impnat=cbool(prolog_systemtime)]).
:- export('$walltime'/1).
:- '$props'('$walltime'/1, [impnat=cbool(prolog_walltime)]).
:- export('$runclick'/1).
:- '$props'('$runclick'/1, [impnat=cbool(prolog_runclick)]).
:- export('$userclick'/1).
:- '$props'('$userclick'/1, [impnat=cbool(prolog_userclick)]).
:- export('$systemclick'/1).
:- '$props'('$systemclick'/1, [impnat=cbool(prolog_systemclick)]).
:- export('$wallclick'/1).
:- '$props'('$wallclick'/1, [impnat=cbool(prolog_wallclick)]).
:- export('$userclockfreq'/1).
:- '$props'('$userclockfreq'/1, [impnat=cbool(prolog_userclockfreq)]).
:- export('$systemclockfreq'/1).
:- '$props'('$systemclockfreq'/1, [impnat=cbool(prolog_systemclockfreq)]).
:- export('$wallclockfreq'/1).
:- '$props'('$wallclockfreq'/1, [impnat=cbool(prolog_wallclockfreq)]).
