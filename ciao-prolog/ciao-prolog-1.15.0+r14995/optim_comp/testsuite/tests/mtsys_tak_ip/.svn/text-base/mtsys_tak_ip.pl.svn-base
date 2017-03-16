#include "../mtsys_common.pl"

#if !defined(CIAO3)
#error "tak_ip test requires ciao/optim_comp"
#endif

:- include(engine(spec_arithmetic)).

:- op(50, fx, [(~)]).
:- op(50, fx, [(@)]).
:- op(40, yfx, [(.)]).
:- set_prolog_flag(read_infix_dot, on).
:- op(700, xfx, [(<-)]).
:- op(1100, xfy, [('|')]).
:- op(1150, xfx, [(:=)]).
:- op(1150, fx, [(pred)]).

benchmark_data(tak_ip, 100, triple(18, 12, 6)).

benchmark(triple(X, Y, Z), Out) :-
	tak(X, Y, Z, Out).

:- '$trust_entry'(tak/4, sht, [smallint, smallint, smallint, var]).
:- '$props'(tak/4, [argmodes=[in,in,in,out]]).
% :- '$props'(tak/4, [argmems=[cvar,cvar,cvar,cvar]]).
% :- '$props'(tak/4, [argderefs=[true,true,true,true]]).
tak(X, Y, Z, Out) :-
	tak1(X, Y, Z, Out).

% A predicate where the code is specified as ImProlog and unfolded where it is called.
:- '$props'(tak1/4, [
	imp = det,
	argmodes = [in, in, in, out],
	argderefs = [false, false, false, false],
	argmems = [cvar, cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, smallint, var], [smallint, smallint, smallint, smallint]),
	argunboxs = [true, true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z, W],
	  (tak2(@X,@Y,@Z,T),W <- T)
	))]).

% TODO: probably faster C implementation: manage a stack by hand, use gcc regparams, ...
:- '$improlog_begin'.
:- pred tak2/4 + lowentryfun([intval,intval,intval], intval, 'tak2') + prop(no_worker).
tak2(X,Y,Z,A) :- X =< Y, !, A = Z.
tak2(X,Y,Z,A) :-
	X1 = X - 1,
	tak2(X1,Y,Z,A1),
	Y1 = Y - 1,
	tak2(Y1,Z,X,A2),
	Z1 = Z - 1,
	tak2(Z1,X,Y,A3),
	tak2(A1,A2,A3,A).
:- '$improlog_end'.
