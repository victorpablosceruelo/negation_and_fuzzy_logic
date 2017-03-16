:- module(exported_preds,
	    [
		init_imported_preds/3,
		save_external_pred/2,
		debug_external_pred/0
	    ],
	    [assertions, regtypes, isomodes]).

:- doc(author, "Jorge Navas").
:- doc(module, "This module saves/loads measure, mode, size, and
	resource information for exported predicates.").


:- use_module(library(lists), [append/3]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(messages)).
:- use_module(spec(s_simpspec), 
	    [make_atom/2]).
:- use_module(infer(infer_db), 
	    [inferred/3]).
:- use_module(infer(infer), 
	    [get_info/5, type2measure/3]).
:- use_module(infer(gather_modes_basic), 
	    [translate_to_modes/2]).
:- use_module(infer(gather_modes), 
	    [vartypes_to_modes/2]).

:- use_module(resources(res_assrt_defs(size_trust)), 
	    [apply_trusted_size0/5, apply_trusted_size/6]).
:- use_module(resources(res_assrt_defs(resources_trust)), 
	    [apply_resource_assertion/6]).
:- use_module(resources(res_assrt_defs(resources_lib)), 
	    [get_measures_assrt/2, get_modes_assrt/2]).

:- use_module(resources(init_res(symtable_res)), 
	    [insert_symbol_field/4]).
:- use_module(ciaopp(preprocess_flags), 
	    [current_pp_flag/2]).

:- use_module(program(p_abs), 
	    [get_imported_modules/0, get_imported_calls/1, cleanup_p_abs/0]).

:- doc(bug, "Size and Resource information should be saved in .reg
   files. Instead, they are saved into a intermediate database through
   save_external_pred/2.").

:- doc(bug, "Measure, mode, size, and resource information should be
   retrieved from .reg files rather than the PLAI database.").


:- pred save_external_pred(+Type, +Info) # "Stores dinamically size
relationships and resource functions for a particular @var{Pred}. It is
missing some extra logic to store ONLY external predicates.".

:- data resource_external/4.
:- data size_external/3.

save_external_pred(time, external(F/A, Approx, Resources, Costs)) :-
	current_fact(resource_external(F/A, Approx, Resources, Costs)), !.
save_external_pred(time, external(F/A, Approx, Resources, Costs)) :- !,
	asserta_fact(resource_external(F/A, Approx, Resources, Costs)).
save_external_pred(size, external(F/A, Approx, Size)) :-
	current_fact(size_external(F/A, Approx, Size)), !.
save_external_pred(size, external(F/A, Approx, Size)) :- !,
	asserta_fact(size_external(F/A, Approx, Size)).

:- pred init_imported_preds(+Ap, +Resources, ?ST) # "Insert all information
(modes, measures, sizes, and resource functions) related to an external
predicate.".

:- data imported_pred/1.
:- data imported_pred_obtained/0.

init_imported_preds(Ap, Resources, ST) :-
	current_pp_flag(intermod, on), !,
	retractall_fact(imported_pred(_)),
	cleanup_p_abs,
	get_imported_modules,
	get_imported_calls(Ps),
	asserta_fact(imported_pred_obtained),
	insert_symtable(Ps, Ap, Resources, ST).
init_imported_preds(_Ap, _Resources, _ST).

insert_symtable([],                   _,  _,         _) :- !.
insert_symtable([(_, _, Goal)|Preds], Ap, Resources, ST) :-
	functor(Goal, F, A),
	asserta_fact(imported_pred(F/A)),
	make_atom([F, A], PKey),
	get_measures(F/A, PKey, Measures),
	get_modes(F/A, PKey, Modes),
	get_sizes(F/A, Ap, Modes, Measures, Size),
	get_costs(Resources, Ap, F/A, Modes, Costs),
	insert_symbol_field(ST, F/A, mode,     Modes),
	insert_symbol_field(ST, F/A, measure,  Measures),
	insert_symbol_field(ST, F/A, det,      [1]),
	insert_symbol_field(ST, F/A, size,     Size),
	insert_symbol_field(ST, F/A, solution, inf),
	insert_symbol_field(ST, F/A, time,     [Costs]),
	insert_symtable(Preds, Ap, Resources, ST).

get_measures(F/A, _, Measures) :-
	get_measures_assrt(F/A, Measures), !.
get_measures(F/A, PKey, Measures) :-
	functor(Goal, F, A),
	get_info(regtypes, pred, PKey, Goal, (_Call, Succ_Type)),
	type2measure(Goal, Succ_Type, Measures), !.
get_measures(_, _PKey, _) :- !.
%	warning_message("No metrics assertions for ~q \n",[PKey]).

get_modes(F/A, _, Measures) :-
	get_modes_assrt(F/A, Measures), !.
get_modes(F/A, PKey, Modes) :-
	current_fact(inferred(modes, PKey, mode(F, A, Info)), _), !,
	translate_to_modes(Info, Modes), !.
get_modes(_, PKey, Modes) :-
	inferred(vartypes, PKey, Vartypes), !,
	vartypes_to_modes(Vartypes, Modes).
get_modes(_, _PKey, _) :- !.
%	warning_message("No mode assertions for ~q \n",[PKey]).


get_sizes(F/A, Ap, Modes, Measures, Sizes) :-
	current_fact(size_external(F/A, Ap, Sizes0)), !,
	apply_trusted_size(Ap, F/A, Modes, Measures, Sizes0, Sizes).
get_sizes(F/A, Ap, Modes, Measures, Sizes) :- !,
	apply_trusted_size0(Ap, F/A, Modes, Measures, Sizes).

:- use_module(resources(resources_basic), 
	    [get_litinfo/9]).

get_costs(Resources, Ap, F/A, _Modes, Costs) :-
	current_fact(resource_external(F/A, Ap, Resources, Costs0)), !,
	functor(Term, F, A),
	make_atom([F, A], Key),
	get_litinfo(Term, 0, Key, _BT, _ST, _ClausePPKey, _PPKey, Ap, LitInfo),
	apply_resource_assertion(Resources, cost, Ap, LitInfo, Costs0, Costs).
% get_costs(Resources,Ap,F/A,Modes,Costs):-!,
% 	get_costs_undefined(Resources,Ap,F/A,Modes,Costs).


:- pred debug_external_pred # "Just a predicate for debugging.".

debug_external_pred :-
	current_fact(resource_external(F/A, Approx, Resources, Costs)),
	simple_message("(~q,~q,~q) = ~q \n", [F/A, Approx, Resources, Costs]),
	fail.
debug_external_pred :-
	current_fact(size_external(F/A, Approx, Size)),
	simple_message("(~q,~q) = ~q \n", [F/A, Approx, Size]),
	fail.
debug_external_pred.
