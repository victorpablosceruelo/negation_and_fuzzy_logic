:- module(_, _, [assertions, inliner]).

:- load_compilation_module(profcost(calibuil(calibuil_tr))).
% TODO: uncertain priority: custom expansion
:- add_sentence_trans(builtin_def/3, 9110).

:- use_module(library(profiler(profiler_extra)), [time_option/2]).
:- inline_module(library(profiler(profiler_extra)), [measure_0/3]).

:- discontiguous bench_test_pred/3.
:- discontiguous bench_test_arit/3.
:- discontiguous calibrated_builtin_pred/1.
:- discontiguous calibrated_builtin_arit/1.

% Warning!!! the next clauses are expanded using calibuil_tr:
bench_buil(pred, atom/1,    '_bench_atom_',    500, X= aaa) :- atom(X).
bench_buil(pred, atomic/1,  '_bench_atomic_',  500, X= aaa) :- atomic(X).
bench_buil(pred, float/1,   '_bench_float_',   500, X=1.3) :- float(X).
bench_buil(pred, integer/1, '_bench_integer_', 500, X=3) :- integer(X).
bench_buil(pred, nonvar/1,  '_bench_nonvar_',  500, X= aaa) :- nonvar(X).
bench_buil(pred, number/1,  '_bench_number_',  500, X=1.4) :- number(X).
bench_buil(pred, var/1,     '_bench_var_',     500, X=_) :- var(X).
%
%
% Note: these are operations between numeric constants,
% not bt. arithmetic expresions:
%bench_buil(pred,is/2,     '_bench_is_',    500,(X is 2.2)):- _ is X.
bench_buil(pred, =:= /2, '_bench_=:=_', 500, (X is 2.3, Y is 2.3)) :-
	X =:= Y.
bench_buil(pred, =\= /2, '_bench_=\\=_', 500, (X is 2.3, Y is 2.5)) :-
	X =\= Y.
bench_buil(pred, < /2,  '_bench_<_',  500, (X=2, Y=3)) :- X < Y.
bench_buil(pred, >= /2, '_bench_>=_', 500, (X=3, Y=2)) :- X >= Y.
bench_buil(pred, > / 2, '_bench_>_',  500, (X=3, Y=2)) :- X > Y.
bench_buil(pred, =< /2, '_bench_=<_', 500, (X=2, Y=3)) :- X =< Y.
%
%
bench_buil(pred, arg/3, '_bench_arg_', 500, (F= a(f, g(h), i, kl), N=3)) :-
	arg(N, F, _).
%
bench_buil(pred, functor/3, '_bench_functor_', 500, F= a(f, g(h), i, kl)) :-
	functor(F, _, _).
%
%

bench_buil(pred, true/0, '_bench_true_', 500, _) :- true.
% bench_buil(pred,!/0,      '_cut_',         500, _) :- !.

% The next are operands in arithmetic expressions:

bench_buil(arit, (-) /1,    '_bench_ae_-_',       500, X is 2.7) :- _ is -(X).
bench_buil(arit, (+) /1,    '_bench_ae_+/1_',     500, X is 2.2) :- _ is +(X).
bench_buil(arit, (--) /1,   '_bench_ae_--_',      500, X is 2.3) :- _ is X-1.
bench_buil(arit, (++) /1,   '_bench_ae_++_',      500, X is 2.2) :- _ is X+1.
bench_buil(arit, integer/1, '_bench_ae_integer_', 500, X is 2.1) :-
	_ is integer(X).
bench_buil(arit, truncate/1, '_bench_ae_truncate_', 500, X is 2.233) :-
	_ is truncate(X).
bench_buil(arit, float/1, '_bench_ae_float_', 500, X is 2.3) :- _ is float(X).
bench_buil(arit, (\) /1,  '_bench_ae_\_',     500, X is 2.2) :- _ is \ X.
bench_buil(arit, abs/1,   '_bench_ae_abs_',   500, X is 2.2) :- _ is abs(X).
bench_buil(arit, sign/1,  '_bench_ae_sign_',  500, X is -1.2) :- _ is sign(X).
bench_buil(arit, float_integer_part/1, '_bench_ae_float_integer_part_', 500,
	    X is 2.4) :-
	_ is float_integer_part(X).
bench_buil(arit, float_fractional_part/1, '_bench_ae_float_fractional_part_',
	    500, X is 2.4) :-
	_ is float_fractional_part(X).
bench_buil(arit, floor/1, '_bench_ae_floor_', 500, X is -1.24) :-
	_ is floor(X).
bench_buil(arit, round/1, '_bench_ae_round_', 500, X is -1.23) :-
	_ is round(X).
bench_buil(arit, ceiling/1, '_bench_ae_ceiling_', 500, X is -1.23) :-
	_ is ceiling(X).
bench_buil(arit, (+) /2, '_bench_ae_+/2_', 500, (X is 5.1, Y is 2.1)) :-
	_ is X+Y.
bench_buil(arit, (-) /2, '_bench_ae_-/2_', 500, (X is 5.1, Y is 2.1)) :-
	_ is X-Y.
bench_buil(arit, * /2, '_bench_ae_*/2_', 500, (X is 5.1, Y is 2.1)) :-
	_ is X*Y.
bench_buil(arit, / /2, '_bench_ae_/ /2_', 500, (X is 5.1, Y is 2.1)) :-
	_ is X/Y.
bench_buil(arit, // /2, '_bench_ae_// /2-_', 500, (X is 5, Y is 2)) :-
	_ is X//Y.
bench_buil(arit, rem/2, '_bench_ae_rem_', 500, (X is 5, Y is 2)) :-
	_ is X rem Y.
bench_buil(arit, # /2, '_bench_ae_#_', 500, (X is 5, Y is 2)) :-
	_ is X#Y.
bench_buil(arit, /\ /2, '_bench_ae_/\\_', 500, (X is 5, Y is 2)) :-
	_ is X/\Y.
bench_buil(arit, \/ /2, '_bench_ae_\\/_', 500, (X is 5, Y is 2)) :-
	_ is X\/Y.
bench_buil(arit, << /2, '_bench_ae_<<_', 500, (X is 5, Y is 2)) :-
	_ is X<<Y.
bench_buil(arit, >> /2, '_bench_ae_>>_', 500, (X is 5, Y is 2)) :-
	_ is X>>Y.
bench_buil(arit, mod/2, '_bench_ae_mod_', 500, (X is 5, Y is 2)) :-
	_ is X mod Y.
bench_buil(arit, ** /2, '_bench_ae_**_', 500, (X is 5.5, Y is 3.22)) :-
	_ is X ** Y.
bench_buil(arit, exp/1,  '_bench_ae_exp_',  500, (X is 0.567)) :- _ is exp(X).
bench_buil(arit, log/1,  '_bench_ae_log_',  500, (X is 0.456)) :- _ is log(X).
bench_buil(arit, sqrt/1, '_bench_ae_sqrt_', 500, (X is 13.45)) :- _ is sqrt(X).
bench_buil(arit, cos/1,  '_bench_ae_cos_',  500, (X is 1.234)) :- _ is cos(X).
bench_buil(arit, atan/1, '_bench_ae_atan_', 500, (X is -0.24)) :- _ is atan(X).
bench_buil(arit, gcd/2,  '_bench_ae_gcd_',  500, (X is 755, Y is 235)) :-
	_ is gcd(X, Y).
