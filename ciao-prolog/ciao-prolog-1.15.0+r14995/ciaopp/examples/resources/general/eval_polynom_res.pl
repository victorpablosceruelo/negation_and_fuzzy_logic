:- module(_, [call_fpu_time/2], [assertions, library(resdefs(resources_decl))]).
:- use_module(resources(resources_basic)).

call_fpu_time(LitInfo, Error) :-
	litinfo_get_lit(LitInfo, Head),
	call_fpu_time_(Head, Error).

call_fpu_time_(_A is B, Error) :-
	!,
	fpu_time(B, Error).
call_fpu_time_(_, 0).

fpu_time(A, 0) :-
	(var(A) ; num(A)),
	!.
fpu_time(A, Time) :-
	functor(A, OpName, N),
	A =.. [_|Args],
	op_time(OpName / N, Time0),
	fpu_times(Args, Time1),
	Time is Time0 + Time1.

fpu_times([],     0).
fpu_times([A|As], S) :-
	fpu_time(A, S0),
	fpu_times(As, S1),
	S is S0 + S1.

op_time(* /2, 1.1).
op_time(+ /2, 1.4).
op_time(- /2, 1.4).
op_time(/ /2, 2.5).
