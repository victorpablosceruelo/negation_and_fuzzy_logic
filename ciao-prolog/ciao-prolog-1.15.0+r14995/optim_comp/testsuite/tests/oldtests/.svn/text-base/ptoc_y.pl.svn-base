:- module(_, _, []).

:- '$pragma'('$ptocdump').

:- '$preddef'(tak/4, ptoc).
:- '$ptoc_prop'('ptoc_y:tak'/4, [call_types=[smallint, smallint, smallint, var], exit_types=[smallint, smallint, smallint, smallint]]).
% :- '$ptoc_prop'('ptoc_y:tak'/4, [argmodes=[in,in,in,out]]).
%:- '$ptoc_prop'('ptoc_y:tak'/4, [argmems=[cvar,cvar,cvar,cvar]]).
%:- '$ptoc_prop'('ptoc_y:tak'/4, [argderefs=[true,true,true,true]]).
:- '$ptoc_prop'('ptoc_y:tak'/4, [imp=det]).
%:- '$ptoc_prop'('ptoc_y:tak'/4, [should_trim_frame=no]).
:- '$ptoc_prop'('ptoc_y:tak'/4, [register=true]).
:- '$ptoc_prop'('ptoc_y:tak'/4, [indexed=false]).


tak(X,Y,Z,A) :-
	X =< Y, !,
%        display(tak_1(X,Y,Z,A)), nl,
	Z = A.
tak(X,Y,Z,A) :-
%        display(tak(X,Y,Z,A)), nl,
	% X > Y,
	X1 is X - 1,
	tak(X1,Y,Z,A1),
	Y1 is Y - 1,
	tak(Y1,Z,X,A2),
	Z1 is Z - 1,
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).
