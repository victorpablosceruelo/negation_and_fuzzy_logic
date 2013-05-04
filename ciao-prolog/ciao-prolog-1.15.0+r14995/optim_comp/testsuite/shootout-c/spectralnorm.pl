:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_math))).

% TODO: This benchmark runs around 2% slower in the ImProlog version than in C.
%   The use of 'alloca' does not seem to be the problem.

:- '$native_weak_inline'(include('engine/spectralnorm.native.h')).

:- '$improlog_begin'.

:- pred eval_A/3 + lowentryfun([intmach, intmach], flt64, 'eval_A').
eval_A(I, J, R) :-
	R = 1.0 / ~'$trust_typed'(((I + J) * (I + J + 1) / 2 + I + 1), flt64).

% TODO: add const and array? prototype was: void eval_A_times_u(int N, const double u[], double Au[])
:- pred eval_A_times_u/3 + lowentry(det, [intmach, ref1(array(ref0(mut(flt64)))), ref1(array(ref0(mut(flt64))))], 'eval_A_times_u').
eval_A_times_u(N, U, Au) :-
	'$for_each'(I, ~intrange(N), (
           Au[@I] <- 0.0,
	   '$for_each'(J, ~intrange(N), (
	      eval_A(@I, @J, V),
	      X = Au[@I],
	      X <- @X + V * @U[@J]
%	      Au[@I] <- @Au[@I] + V * @U[@J]
	   ))
        )).				

% TODO: add const and array? prototype was: void eval_At_times_u(int N, const double u[], double Au[])
:- pred eval_At_times_u/3 + lowentry(det, [intmach, ref1(array(ref0(mut(flt64)))), ref1(array(ref0(mut(flt64))))], 'eval_At_times_u').
eval_At_times_u(N, U, Au) :-
	'$for_each'(I, ~intrange(N), (
           Au[@I] <- 0.0,
	   '$for_each'(J, ~intrange(N), (
	      eval_A(@J, @I, V),
	      X = Au[@I],
	      X <- @X + V * @U[@J]
%	      Au[@I] <- @Au[@I] + V * @U[@J]
	   ))
        )).				

% TODO: add const and array? prototype was: void eval_AtA_times_u(int N, const double u[], double AtAu[])
:- pred eval_AtA_times_u/3 + lowentry(det, [intmach, ref1(array(ref0(mut(flt64)))), ref1(array(ref0(mut(flt64))))], 'eval_AtA_times_u').
eval_AtA_times_u(N, U, AtAu) :-
	V = ~'$alloc'(alloca, array(ref0(mut(flt64)), N)),
	eval_A_times_u(N, U, V),
	eval_At_times_u(N, V, AtAu).

:- pred begin/1 + lowentry(det, [intmach], 'begin') + prop(foreign__static).
begin(N) :-
	U = ~'$alloc'(alloca, array(ref0(mut(flt64)), N)),
	V = ~'$alloc'(alloca, array(ref0(mut(flt64)), N)),
	'$for_each'(I, ~intrange(N), (U[@I] <- 1.0)),
	% TODO: use I when scope is correct
	'$for_each'(I1, ~intrange(10), (
          eval_AtA_times_u(N,U,V),
	  eval_AtA_times_u(N,V,U)
	)),
	VV = ~initmut(flt64, 0.0),
	VBV = ~initmut(flt64, 0.0),
	% TODO: use I when scope is correct
	'$for_each'(I2, ~intrange(N), (
	  VBV <- @VBV + @U[@I2] * @V[@I2],
	  VV <- @VV + @V[@I2] * @V[@I2]
	)),
	printf2("%0.9f\n", ~sqrt(@VBV / @VV)).

:- '$improlog_end'.
