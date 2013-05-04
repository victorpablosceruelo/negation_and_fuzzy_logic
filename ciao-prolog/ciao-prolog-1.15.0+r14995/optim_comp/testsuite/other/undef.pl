:- module(_, _, []).

:- impl_defined(a/0).
:- impl_defined(b/0).

test1 :-
	a.

test2 :-
	call((a,b)).

:- use_module(engine(hiord_rt)).

test3 :-
	'$meta_call'('basiccontrol:,'('undef:c','undef:b')).

c.
