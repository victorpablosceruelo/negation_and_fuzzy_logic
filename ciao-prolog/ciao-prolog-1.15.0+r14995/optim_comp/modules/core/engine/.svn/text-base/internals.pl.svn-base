:- module(internals, [], [pure, assertions]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(arithmetic)).
:- use_module(engine(attributes)).
:- use_module(engine(exceptions)).
:- use_module(engine(term_compare)).
:- use_module(engine(data_facts)).
:- use_module(engine(io_basic)).
:- use_module(engine(io_aux)).

:- doc(title,"Engine Internal Predicates").  

:- doc(module,"

This library lists a set of internal predicates (written in C) used by
the system code. They should not be used in user code. The file itself
provides handles for the module system into the internal definitions.

").

% ===========================================================================

:- '$native_include_c_header'(engine(engine__ciao_prolog)).
:- '$native_include_c_header'(engine(engine__ciao_gluecode)).
:- '$native_include_c_header'(engine(engine__os)).
:- '$native_include_c_header'(engine(engine__own_mmap)).

% core definitions
:- '$native_include_c_source'(engine(engine__alloc)).
:- '$native_include_c_source'(engine(engine__bignum)).
:- '$native_include_c_source'(engine(engine__ciao_prolog)).
:- '$native_include_c_source'(engine(engine__debug)).
:- '$native_include_c_source'(engine(engine__interrupt)).
:- '$native_include_c_source'(engine(engine__own_mmap)).
:- '$native_include_c_source'(engine(engine__own_malloc)).
:- '$native_include_c_source'(engine(engine__profile)).
:- '$native_include_c_source'(engine(engine__threads)).
:- '$native_include_c_source'(engine(engine__resources)).
:- '$native_include_c_source'(engine(engine__main)).
:- '$native_include_c_source'(engine(engine__float_tostr)).
:- '$native_include_c_source'(engine(engine__registers)).
:- '$native_include_c_source'(engine(engine__gc)).

% ---------------------------------------------------------------------------
:- '$native_include_c_source'(.(internals)).
:- export('$atom_mode'/2).
:- '$props'('$atom_mode'/2, [impnat=cbool(prolog_atom_mode)]).
:- export('$termheap_usage'/1).
:- '$props'('$termheap_usage'/1, [impnat=cbool(termheap_usage)]).
:- export('$envstack_usage'/1).
:- '$props'('$envstack_usage'/1, [impnat=cbool(envstack_usage)]).
:- export('$trail_usage'/1).
:- '$props'('$trail_usage'/1, [impnat=cbool(trail_usage)]).
:- export('$choice_usage'/1).
:- '$props'('$choice_usage'/1, [impnat=cbool(choice_usage)]).
:- export('$stack_shift_usage'/1).
:- '$props'('$stack_shift_usage'/1, [impnat=cbool(stack_shift_usage)]).

% ---------------------------------------------------------------------------
:- export('$purge'/1).
% TODO: used?
:- '$props'('$purge'/1, [impnat=cbool(prolog_purge)]).
:- export('$current_clauses'/2).
:- '$props'('$current_clauses'/2, [impnat=cbool(current_clauses)]).

:- export('$asserta_root'/3).
:- '$props'('$asserta_root'/3, [impnat=cbool(prolog_asserta_root)]).
:- export('$asserta'/2).
:- '$props'('$asserta'/2, [impnat=cbool(prolog_asserta)]).
:- export('$asserta_ref'/3).
:- '$props'('$asserta_ref'/3, [impnat=cbool(prolog_asserta_ref)]).
:- export('$assertz_root'/3).
:- '$props'('$assertz_root'/3, [impnat=cbool(prolog_assertz_root)]).
:- export('$assertz'/2).
:- '$props'('$assertz'/2, [impnat=cbool(prolog_assertz)]).
:- export('$assertz_ref'/3).
:- '$props'('$assertz_ref'/3, [impnat=cbool(prolog_assertz_ref)]).
:- export('$erase'/2).
:- '$props'('$erase'/2, [impnat=cinsnp(prolog_erase)]).
:- export('$erase_nb_root'/3).
:- '$props'('$erase_nb_root'/3, [impnat=cinsnp(prolog_erase_nb_root)]).
:- export('$erase_nb'/2).
:- '$props'('$erase_nb'/2, [impnat=cinsnp(prolog_erase_nb)]).
:- export('$erase_ref'/1).
:- '$props'('$erase_ref'/1, [impnat=cbool(prolog_erase_ref)]).
:- export('$current'/2).
:- '$props'('$current'/2, [impnat=cinsnp(prolog_current)]).
:- export('$current_nb_root'/3).
:- '$props'('$current_nb_root'/3, [impnat=cinsnp(prolog_current_nb_root)]).
:- export('$current_nb'/2).
:- '$props'('$current_nb'/2, [impnat=cinsnp(prolog_current_nb)]).
:- export('$current_nb_ref'/3).
:- '$props'('$current_nb_ref'/3, [impnat=cinsnp(prolog_current_nb_ref)]).
:- export('$open_pred'/1).
:- '$props'('$open_pred'/1, [impnat=cbool(prolog_open_pred)]).
:- export('$close_pred'/1).
:- '$props'('$close_pred'/1, [impnat=cbool(prolog_close_pred)]).

% ---------------------------------------------------------------------------

:- export('$abolish'/1).
:- '$props'('$abolish'/1, [impnat=cbool(prolog_abolish)]).
:- export('$erase_clause'/1).
:- '$props'('$erase_clause'/1, [impnat=cbool(erase_clause)]).
:- export('$empty_gcdef_bin'/0).
:- '$props'('$empty_gcdef_bin'/0, [impnat=cbool(empty_gcdef_bin)]).
:- export('$program_usage'/1).
:- '$props'('$program_usage'/1, [impnat=cbool(program_usage)]).
:- export('$internal_symbol_usage'/1).
:- '$props'('$internal_symbol_usage'/1, [impnat=cbool(internal_symbol_usage)]).
:- export('$total_usage'/1).
:- '$props'('$total_usage'/1, [impnat=cbool(total_usage)]).
:- export('$ddt'/1).
:- '$props'('$ddt'/1, [impnat=cbool(set_predtrace)]).
% #if defined(ATOMGC)
% :- export('$erase_atom'/1).
% :- '$props'('$erase_atom'/1, [impnat=cbool(prolog_erase_atom)]).
% #endif
:- export('$prompt'/2).
:- '$props'('$prompt'/2, [impnat=cbool(prompt)]).
:- export('$frozen'/2).
:- '$props'('$frozen'/2, [impnat=cbool(frozen)]).
:- export('$defrost'/2).
:- '$props'('$defrost'/2, [impnat=cbool(defrost)]).
:- export('$unknown'/2).
:- '$props'('$unknown'/2, [impnat=cbool(unknown)]).
:- export('$ferror_flag'/2).
:- '$props'('$ferror_flag'/2, [impnat=cbool(ferror_flag)]).
:- export('$quiet_flag'/2).
:- '$props'('$quiet_flag'/2, [impnat=cbool(quiet_flag)]).
% :- export('$prolog_radix'/2).
% :- '$props'('$prolog_radix'/2, [impnat=cbool(prolog_radix)]).
:- export('$constraint_list'/2).
:- '$props'('$constraint_list'/2, [impnat=cbool(constraint_list)]).
:- export('$eq'/2).
:- '$props'('$eq'/2, [impnat=cbool(prolog_eq)]).
%:- export('$blob_data'/3).
%:- '$props'('$blob_data'/3, [impnat=cbool(blob_data)]).
:- export('$first_instance'/2).
:- '$props'('$first_instance'/2, [impnat=cbool(first_instance)]).
:- export('$unix_argv'/1).
:- '$props'('$unix_argv'/1, [impnat=cbool(prolog_unix_argv)]).
:- export('$ciao_version'/1).
:- '$props'('$ciao_version'/1, [impnat=cbool(prolog_version)]).
:- export('$bootversion'/0).
:- '$props'('$bootversion'/0, [impnat=cbool(prolog_print_emulator_version)]).
:- export('$gc_mode'/2).
:- '$props'('$gc_mode'/2, [impnat=cbool(gc_mode)]).
:- export('$gc_trace'/2).
:- '$props'('$gc_trace'/2, [impnat=cbool(gc_trace)]).
:- export('$gc_margin'/2).
:- '$props'('$gc_margin'/2, [impnat=cbool(gc_margin)]).
:- export('$gc_usage'/1).
:- '$props'('$gc_usage'/1, [impnat=cbool(gc_usage)]).

% ===========================================================================

% force dependency...
% TODO: polish, some executables may not require them!
:- use_module(engine(ql_inout)).
:- use_module(library(concurrency)).
:- use_module(engine(dynlink)).

% ===========================================================================

% TODO: move to terms_basic?
:- export('$setarg'/4).
:- '$props'('$setarg'/4, [impnat=cbool(setarg)]).
% TODO: move to basiccontrol?
:- export('$undo_goal'/1).
:- '$props'('$undo_goal'/1, [impnat=cbool(undo)]).

% ===========================================================================
% Low level global variables
%
% Currently, the global variables are reserved statically:
%
%   [optim_comp only]
%   1 - debugger
%   2 - compiler/frontend.pl ('compile' part)
%   4 - compiler/frontend.pl ('split' part)
%   3 - Errs object during analysis
%   5 - unused, see compiler/dynload.pl
%   6 - absmach
%
%   [rest of Ciao]
%   10 - CHR package (chr/hrpolog.pl)
%   11 - global_vars module
%
% TODO: move to a different file and share
%       (ciao/lib/engine/internals.pl and
%        optim_comp/modules/core/engine/internals.pl)

% global variables based on setarg
:- export('$global_vars_set'/2).
'$global_vars_set'(I, X) :-
	'$global_vars_get_root'(R),
	'$setarg'(I, R, X, on).
:- export('$global_vars_get'/2).
'$global_vars_get'(I, X) :-
	'$global_vars_get_root'(R),
	arg(I, R, X).

% naive implementation of mutable variables based on setarg
% TODO: I could place mutable type info here
% :- export('$mutvar_init'/2).
% '$mutvar_init'(Val, X) :-
% 	X = '$mut'(Val).
% :- export('$mutvar_assign'/2).
% '$mutvar_assign'(X, Val) :- nonvar(X), X = '$mut'(_), 
% 	'$setarg'(1, X, Val, on).
% :- export('$mutvar_get'/2).
% '$mutvar_get'(X, Val) :- nonvar(X), X = '$mut'(Val).

% TODO: Bad implementation! This creates long dereference chains
%       Use update_attribute instead of deteach/attach sequences
%       (detected in the main branch by Remy & Jose)
%
% TODO: it may worth implementing the new attributes
/*
% global variables based on cva variables
:- use_module(engine(attributes)).
:- use_module(engine(term_typing)).
:- export('$global_vars_set'/2).
'$global_vars_set'(I, X) :-
	'$global_vars_get_root'(R),
	arg(I, R, V),
	( type(V, attv) ->
	    detach_attribute(V)
	; true
	),
	attach_attribute(V, v(X)).
:- export('$global_vars_get'/2).
'$global_vars_get'(I, X) :-
	'$global_vars_get_root'(R),
	arg(I, R, V),
	( type(V, attv) ->
	    get_attribute(V, v(X))
	; attach_attribute(V, v(X))
	).
*/

%:- use_module(engine(io_basic)).

:- '$props'('$global_vars_get_root'/1, [impnat=cbool(prolog_global_vars_get_root)]).

% note: This has to be done before any choicepoint is created
'$global_vars_init' :-
% for cva version	
%	F = '$glb'(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,
%	           _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), % 32 global vars
% for setarg version	
	F = '$glb'(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), % 32 global vars
	'$global_vars_set_root'(F).

:- '$props'('$global_vars_set_root'/1, [impnat=cbool(prolog_global_vars_set_root)]).

% ===========================================================================

:- initialization(fail).
:- on_abort(fail).

control_c_handler :- throw(control_c).

% ===========================================================================
% Attributed variables

% (entry information)
% :- if(defined(optim_comp)).
% TODO: better solution? the compiler can remove them if they are not exported
:- export(uvc/2).
:- export(ucc/2).
:- export(pending_unifications/1).
% :- else.
% :- trust pred verify_attribute(A,B).
% :- trust pred combine_attributes(A,B).
% :- entry uvc/2.
% :- entry ucc/2.
% :- entry pending_unifications/1.
% :- endif.
%% :- include(library(engine(attributed_variables))).
:- include('.'(attributed_variables)).

% ===========================================================================
%------ internal builtin errors ------%
% TODO: this predicate is called from basiccontrol.c:wam and is used
%       to decode the C exception and throw an error <- too complicated... partially recode in C? does it belong to any standard?

:- export(error/5). % TODO: NOT A EXPORT BUT A INTERNAL ENTRY
error(Type, PredName, PredArity, Arg, Culprit) :-
%        display('In Error'(Type, Culprit)), nl,
        error_term(Type, Culprit, Error_Term),
%        display(error_term_is(Error_Term)), nl,
        where_term(Arg, PredName, PredArity, Where_Error),
        throw(error(Error_Term, Where_Error)).

in_range(Type, Code, WhichWithinType):-
        range_per_error(Range),
        error_start(Type, Section),
        Start is Section * Range,
        Code >= Start,
        Code < Start + Range,
        WhichWithinType is Code - Start.

error_term(  1, _, instantiation_error) :- !.
error_term(Code, _, system_error) :-   in_range(system, Code, _), !.
error_term(Code, _, syntax_error) :-   in_range(syntax, Code, _), !.
error_term(Code, _, resource_error) :- in_range(res,    Code, _), !.
error_term(Code, _, user_error) :-     in_range(user,   Code, _), !.
error_term(N, Culprit, evaluation_error(Type, Culprit)) :-
        in_range(eval, N, Code), !,
        evaluation_code(Code, Type).
error_term(N, Culprit, representation_error(Type, Culprit)) :-
        in_range(repres, N, Code), !,
        representation_code(Code, Type).
error_term(N, Culprit, type_error(Type, Culprit)) :-
        in_range(type, N, Code), !,
        type_code(Code, Type).
error_term(N, Culprit, domain_error(Type, Culprit)) :-
        in_range(dom, N, Code), !,
        domain_code(Code, Type).
error_term(N, Culprit, existence_error(Type, Culprit)) :-
        in_range(exist, N, Code), !,
        existence_code(Code, Type).
error_term(N, Culprit, permission_error(Object, Permission, Culprit)) :-
        in_range(perm, N, Code), !,
        get_obj_perm(Code,Obj,Per),
        permission_type_code(Per, Permission),
        permission_object_code(Obj, Object).


%% Check error type and return get Code for every class of error.  This should
%% be made more modularly (i.e., with an C interface - but is it worth?)

 %% is_evaluation_error(N,Code) :-     N>120, N<126, Code is N-121.
 %% 
 %% is_representation_error(N,Code) :- N>114, N<121, Code is N-115.
 %% 
 %% is_type_error(N,Code) :-           N>1, N<15, Code is N-2.
 %% 
 %% is_domain_error(N,Code) :-         N>14, N<32, Code is N-15.
 %% 
 %% is_existence_error(N,Code) :-      N>31, N<35, Code is N-32.
 %% 
 %% is_permission_error(N,Code) :-     N>34, N<115, Code is N-35.

get_obj_perm(Code, Obj, Perm) :-
        Obj is Code mod 10,
        Perm is Code // 10.


 %% culprit_stream([], S) :- !, current_input(S).
 %% culprit_stream(S,S).

%% This is the Prolog counterpart of the definitions in support.h.  Please 
%% have a look there!

range_per_error(100).

error_start(inst,   0).
error_start(type,   1).
error_start(dom,    2).
error_start(exist,  3).
error_start(perm,   4).
error_start(repres, 5).
error_start(eval,   6).
error_start(res,    7).
error_start(syntax, 8).
error_start(system, 9).
error_start(user,   10).

type_code(0, atom).
type_code(1, atomic).
type_code(2, byte).
type_code(3, character).
type_code(4, compound).
type_code(5, evaluable).
type_code(6, in_byte).
type_code(7, integer).
type_code(8, list).
type_code(9, number).
type_code(10, predicate_indicator).
type_code(11, variable).
type_code(12, callable).

domain_code(0, character_code_list).
domain_code(1, source_sink).
domain_code(2, stream).
domain_code(3, io_mode).
domain_code(4, not_empty_list).
domain_code(5, not_less_than_zero).
domain_code(6, operator_priority).
domain_code(7, prolog_flag).
domain_code(8, read_option).
domain_code(9, flag_value).
domain_code(10, close_option).
domain_code(11, stream_option).
domain_code(12, stream_or_alias).
domain_code(13, stream_position).
domain_code(14, stream_property).
domain_code(15, write_option).
domain_code(16, operator_specifier).

existence_code(0, procedure).
existence_code(1, source_sink).
existence_code(2, stream).

permission_type_code(0, access).
permission_type_code(1, creation).
permission_type_code(2, input).
permission_type_code(3, modification).
permission_type_code(4, opening).
permission_type_code(5, output).
permission_type_code(6, reposition).

permission_object_code(0, binary_stream).
permission_object_code(1, source_sink).
permission_object_code(2, stream).
permission_object_code(3, text_stream).
permission_object_code(4, flag).
permission_object_code(5, operator).
permission_object_code(6, past_end_of_stream).
permission_object_code(7, private_procedure).
permission_object_code(8, static_procedure).

representation_code(0, character_code_list).
representation_code(1, in_character_code).
representation_code(2, max_arity).
representation_code(3, character).
representation_code(4, max_integer).
representation_code(5, min_integer).
representation_code(6, character_code).

evaluation_code(0, float_overflow).
evaluation_code(1, int_overflow).
evaluation_code(2, undefined).
evaluation_code(3, underflow).
evaluation_code(4, zero_divisor).

where_term(0, PredName, PredArity, PredName/PredArity) :- !.
where_term(Arg, PredName, PredArity, PredName/PredArity-Arg).

% ===========================================================================
% Boot the machine

:- export(boot/0). % TODO: NOT A EXPORT BUT A INTERNAL ENTRY
boot :-
	init,
	run_main_entry.

% TODO: recode in C?
% High level part of the initialization
:- data init_ok/0.
init :-
	prepare_stacks,
	init_hooks, !,
	asserta_fact(init_ok).
init :-
	message(error, '{Internal initialization failed}'),
	halt(1).

reboot :-
	init_ok, % stop if initialization did not succeed
	prepare_stacks,
	abort_hooks,
	!.

% High level part of the WAM stack preparation
prepare_stacks :-
	'$global_vars_init'.

:- use_module(engine(rt_exp), [do_initialization/1]).

init_hooks :-
	( '$main_module'(MainModule),
	  rt_exp:do_initialization(MainModule),
	  fail
	; true
	).
	
:- import(user, [aborting/0]).
abort_hooks :-
	( '$on_abort'(_), % module's abort hook
	  fail
	; aborting % user's abort hook
	).

% Run the main entry
:- import(internal_init, ['$main_module'/1]).
:- import(internal_init, ['$main_entry'/1]).
run_main_entry :-
	( '$unix_argv'(Args), '$main_entry'(Args) ->
	    true
	; message(error, '{Program ended with failure}'),
	  halt(2)
	).
