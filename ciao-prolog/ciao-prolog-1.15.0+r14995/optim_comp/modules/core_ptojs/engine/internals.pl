:- module(internals, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Internals").
:- doc(author, "Jose F. Morales").

:- doc(bug, "[Incomplete version of engine(internals) for pl2js]").

:- if(( target_platform(v8) ; target_platform(nodejs) )).
:- '$pragma'(shell_exec).
:- endif.

:- export('__boot__'/0).
:- pred '__boot__'/0 + det.
'__boot__' :-
	% TODO: implement in other way
	'$boot'.
%%	':'('\6\root', static_noself_new__),
%%	':'('\6\root', '__call_main__'). % TODO: check for main/1 too
%	':'('\6\root', main). % TODO: check for main/1 too

