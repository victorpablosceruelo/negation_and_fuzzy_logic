:- module(_, [], [compiler(complang)]).

% Generation of gluecode to foreign functions, processing of ttr tables

:- use_module(library(lists), [select/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(compiler(errlog)).
:- use_module(compiler(module_pli)).
:- use_module(compiler(module_exp)).
:- use_module(compiler(memoize)).

% TODO: use ptoc__impcomp!!! (first with inline, C code, then try to make it more abstract)
% TODO: try to use lower level C code
:- public foreign_prototype/2.
foreign_prototype(Desc) := Proto :-
	Desc = foreign(_, ForeignName, Arguments, ResVar, NeedsState),
	( ResVar = [ResN], select(arg(ResN, ResTTr, _, _), Arguments, Arguments1) ->
	    ResCType = ~ttr_ctype_res(ResTTr)
	; ResCType = void, Arguments1 = Arguments
	),
	Args0 = ~foreign_prototype_args(Arguments1),
	( NeedsState = yes -> Args = [ciao_state|Args0] ; Args = Args0 ),
	Proto = declare(ForeignName, function(Args, ResCType)).

foreign_prototype_args([]) := [] :- !.
foreign_prototype_args([A|As]) := [~foreign_prototype_arg(A)|~foreign_prototype_args(As)] :- !.

foreign_prototype_arg(arg(_, TTr, _, _)) := X :- X = ~ttr_ctype_call(TTr), !.
foreign_prototype_arg(_) := ciao_term.

% -----------------------------------------------------------------------------

:- public interface_function/3.
interface_function(PredName, Desc) := GlueCode :-
	code :: accum(GlueCode),
	interface_function__body(PredName, Desc).

{
:- fluid code :: accum.
interface_function__body(MF/A, Desc) :-
	Desc = foreign(_, ForeignName, Arguments, ResVar, NeedsState),
	code.add(call('ERR__FUNCTOR', [string(~atom_codes(MF)), A])),
	% variable declaration 
	params_apply(Arguments, t_decl),
	params_apply(Arguments, v_decl),
	params_apply(Arguments, u_decl),
	% variable initialization
	code.add('DECL_STATE'),
	code.add('INIT_STATE'),
	code.add(call(ciao_frame_begin_s, [state])),
	params_apply(Arguments, ref),
	% Convert data from Prolog to C
	params_apply(~filter_single(Arguments), check),
	params_apply(~filter_compound(Arguments), check),
	params_apply(~filter_single(Arguments), to_c),
	params_apply(~filter_compound(Arguments), to_c),
	% Call foreign function
	do_call(ForeignName, Arguments, ResVar, NeedsState), 
	% Convert data from C to Prolog
	params_apply(Arguments, from_c),
	params_apply(Arguments, free),
	params_apply(Arguments, unify),
	code.add(call(ciao_frame_end_s, [state])),
	code.add('CBOOL__PROCEED').

:- '$ctxprj'(filter_single/2, []).
filter_single([]) := [] :- !.
filter_single([X|Xs]) := [X|~filter_single(Xs)] :- X = arg(_, _, compound(_), _), !.
filter_single([_|Xs]) := ~filter_single(Xs) :- !.

:- '$ctxprj'(filter_compound/2, []).
filter_compound([]) := [] :- !.
filter_compound([X|Xs]) := [X|~filter_compound(Xs)] :- X = arg(_, _, single, _), !.
filter_compound([_|Xs]) := ~filter_compound(Xs) :- !.

params_apply([], _).
params_apply([X|Xs], Action) :- param_apply(Action, X), params_apply(Xs, Action).

param_apply(t_decl, X) :- !, param_apply_t_decl(X).
param_apply(v_decl, X) :- !, param_apply_v_decl(X).
param_apply(u_decl, X) :- !, param_apply_u_decl(X).
param_apply(ref, X) :- !, param_apply_ref(X).
param_apply(check, X) :- !, param_apply_check(X).
param_apply(to_c, X) :- !, param_apply_to_c(X).
param_apply(from_c, X) :- !, param_apply_from_c(X).
param_apply(free, X) :- !, param_apply_free(X).
param_apply(unify, X) :- !, param_apply_unify(X).

param_apply_v_decl(arg(N, TTr, _, _)) :- CType = ~ttr_ctype_decl(TTr), !,
	code.add(declare(~c(N), CType)).
param_apply_v_decl(_).

param_apply_u_decl(arg(N, TTr, _, _)) :- _ = ~ttr_from_c(TTr), !,
	code.add(declare(~u(N), ciao_term)).
param_apply_u_decl(_).

param_apply_t_decl(arg(N, _, _, _)) :- !,
	code.add(declare(~t(N), ciao_term)).

param_apply_ref(arg(N, _, _, _)) :- code.add(~t(N) = call(ciao_ref, [state, ~x(N)])).

param_apply_check(X) :- Check = ~check_code(X), !,
	code.add(if(logical_not(Check), ~exception_code(X))). 
param_apply_check(_).

:- '$ctxprj'(check_code/2, []).
check_code(arg(N, TTr, _, _)) := call(Check, [state, ~t(N)]) :- Check = ~ttr_check(TTr).

:- '$ctxprj'(exception_code/2, []).
exception_code(arg(N, TTr, _, _)) := X :- X = ~exception_code_2(N, ~ttr_exception(TTr)), !.
exception_code(_) := 'CBOOL__FAIL' :- !.

:- '$ctxprj'(exception_code_2/3, []).
exception_code_2(N, error_in_arg(Type)) := call('ERROR_IN_ARG', [~x(N), N + 1, Type]) :- !.
exception_code_2(_, usage_fault(Msg)) := call('USAGE_FAULT', [string(Msg)]) :- !.

param_apply_to_c(arg(N, TTr, single, _)) :- ToC = ~ttr_to_c(TTr), !,
	code.add(~c(N) = call(ToC, [state, ~t(N)])).
param_apply_to_c(arg(N, TTr, compound(_), _)) :- ToC = ~ttr_to_c(TTr), !,
	code.add(~c(N) = call(ToC, [state, ~t(N)])).
param_apply_to_c(_).

param_apply_from_c(arg(N, TTr, XN, _)) :- FromC = ~ttr_from_c(TTr), !,
	code.add(~u(N) = ~from_c_code(FromC, N, XN)).
param_apply_from_c(_).

:- '$ctxprj'(from_c_code/4, []).
from_c_code('=', N, single) := ~c(N) :- !.
from_c_code(FromC, N, single) := call(FromC, [state, ~c(N)]) :- !.
from_c_code(FromC, N, compound(LengthN)) := call(FromC, [state, ~c(N), ~c(LengthN)]) :- !.

param_apply_free(arg(N, TTr, _, no)) :- Free = ~ttr_free(TTr), !,
	code.add(call(Free, [~c(N)])).
param_apply_free(_).

param_apply_unify(arg(N, TTr, _, _)) :- _ = ~ttr_from_c(TTr), !,
	code.add(if(logical_not(call(ciao_unify_s, [state, ~u(N), ~t(N)])), 'CBOOL__FAIL')).
param_apply_unify(_).

do_call(ForeignName, Arguments, ResVar, NeedsState) :-
	( ResVar = [ResN], select(arg(ResN, _, _, _), Arguments, Arguments1) -> true ; Arguments1 = Arguments ),
	Args0 = ~call_args(Arguments1),
	( NeedsState = yes -> Args = [state|Args0] ; Args = Args0 ),
	( NeedsState = no -> code.add('IMPLICIT_STATE') ; true ),
	( ResVar = [N] -> Call = (~c(N) = call(ForeignName, Args)) ; Call = (call(ForeignName, Args)) ),
	code.add(call('GLUECODE_TRY', [Call])).
}.

call_args([]) := [] :- !.
call_args([X|Xs]) := [~call_arg(X)|~call_args(Xs)] :- !.

call_arg(arg(N, TTr, _, _)) := address(~call_arg_v(N, TTr)) :- _ = ~ttr_call_cref(TTr), !.
call_arg(arg(N, TTr, _, _)) := ~call_arg_v(N, TTr) :- !.

call_arg_v(N, TTr) := ~t(N) :- \+ _ = ~ttr_ctype_decl(TTr), !.
call_arg_v(N, _) := ~c(N) :- !.

c(N) := ~atom_plus_number('c', N).
u(N) := ~atom_plus_number('u', N).
t(N) := ~atom_plus_number('t', N).
x(N) := call('X', [N]).

atom_plus_number(X, N, XN) :-
	number_codes(N, C),
	atom_codes(N2, C),
	atom_concat(X, N2, XN).

% -----------------------------------------------------------------------------
% TODO: move to other module...
% Table of type translations

%% exports
:- public data ttr_ctype_res/2.
:- data ttr_ctype_call/2.
:- data ttr_ctype_decl/2.
:- data ttr_check/2.
:- data ttr_exception/2.
:- data ttr_to_c/2.
:- public data ttr_compound/2.
:- data ttr_call_cref/2.
:- data ttr_from_c/2.
:- data ttr_free/2.

%% export

% TODO: find a better way to do this (e.g. export ttr defs from a module; reuse the solution to import ptoc types?)	
:- public load_ttr_from_pli/1.
load_ttr_from_pli(Pli) :-
	trust(Pli instance_of module_pli),
	Pli.pragma(ttr_def(X, Ys)),
	  assert_ttr_def(Ys, X),
	  fail.
load_ttr_from_pli(_).

% TODO: find a better way to do this (e.g. export ttr defs from a module; reuse the solution to import ptoc types?)	
:- public load_ttr_from_exp/1.
load_ttr_from_exp(Exp) :-
	trust(Exp instance_of module_exp),
	Exp.pragma(ttr_def(X, Ys)),
	  assert_ttr_def(Ys, X),
	  fail.
load_ttr_from_exp(_).

assert_ttr_def([(Y = V)|Ys], X) :- !,
	assert_ttr_def_2(Y, X, V), assert_ttr_def(Ys, X).
assert_ttr_def([], _) :- !.

assert_ttr_def_2(match, X, (D,C,A)) :- !, asserta_fact(ttr_match_0(D, C, A, X)).
assert_ttr_def_2(ctype_res, X, V) :- !, asserta_fact(ttr_ctype_res(X, V)).
assert_ttr_def_2(ctype_call, X, V) :- !, asserta_fact(ttr_ctype_call(X, V)).
assert_ttr_def_2(ctype_decl, X, V) :- !, asserta_fact(ttr_ctype_decl(X, V)).
assert_ttr_def_2(check, X, V) :- !, asserta_fact(ttr_check(X, V)).
assert_ttr_def_2(exception, X, V) :- !, asserta_fact(ttr_exception(X, V)).
assert_ttr_def_2(to_c, X, V) :- !, asserta_fact(ttr_to_c(X, V)).
assert_ttr_def_2(compound, X, V) :- !, asserta_fact(ttr_compound(X, V)).
assert_ttr_def_2(call_cref, X, V) :- !, asserta_fact(ttr_call_cref(X, V)).
assert_ttr_def_2(from_c, X, V) :- !, asserta_fact(ttr_from_c(X, V)).
assert_ttr_def_2(free, X, V) :- !, asserta_fact(ttr_free(X, V)).

:- data ttr_match_0/4.

:- public ttr_match/4.
ttr_match(D, C, A) := TTr :- TTr = ~ttr_match_0(D, C, A), !.
ttr_match(_, _, _) := '$$any_term$$' :- !.

:- public clean_ttr/0.
clean_ttr :-
	retractall_fact(ttr_match_0(_, _, _, _)),
	retractall_fact(ttr_ctype_res(_, _)),
	retractall_fact(ttr_ctype_call(_, _)),
	retractall_fact(ttr_ctype_decl(_, _)),
	retractall_fact(ttr_check(_, _)),
	retractall_fact(ttr_exception(_, _)),
	retractall_fact(ttr_to_c(_, _)),
	retractall_fact(ttr_compound(_, _)),
	retractall_fact(ttr_call_cref(_, _)),
	retractall_fact(ttr_from_c(_, _)),
	retractall_fact(ttr_free(_, _)).

