:- module(_, _, []).

:- use_module(library(prolog_sys)).

% a test to show local_top grown when
% EXECUTE instruction is used...  (which do not invalidate the local_top)
% ENVIRONMENT TRIMMING MAKES THIS TEST WORK!!!!

t :-
	X = 1,
	true,
	X = 1,
	t.
g.
h(_).
