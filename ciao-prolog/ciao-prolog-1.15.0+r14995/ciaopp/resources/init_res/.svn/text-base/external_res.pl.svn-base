:- module(external_res, [init_external_preds/4, get_modes/3, get_measures/3],
	    [assertions, isomodes, regtypes]).

:- doc(author, "Jorge Navas").

:- doc(module, "This module infers measure, mode, size, and resource
   information from assertions for those external predicates.").


:- use_module(library(lists), [append/3]).
:- use_module(library(sort),  [sort/2]).
:- use_module(library(messages)).
:- use_module(spec(s_simpspec),          [make_atom/2]).
:- use_module(infer(infer_db),           [inferred/3]).
:- use_module(infer(infer),              [get_info/5, type2measure/3]).
:- use_module(infer(gather_modes_basic), [translate_to_modes/2]).
:- use_module(infer(gather_modes),       [vartypes_to_modes/2]).
:- use_module(resources(res_assrt_defs(size_trust)),
	    [apply_trusted_size0/5]).
:- use_module(resources(res_assrt_defs(resources_lib)),
	    [get_measures_assrt/2, get_modes_assrt/2, trust_default/3]).
:- use_module(resources(init_res(symtable_res)), [insert_symbol_field/4]).
:- use_module(ciaopp(preprocess_flags),          [current_pp_flag/2]).
:- use_module(program(p_abs), [get_imported_modules/0, get_imported_calls/1]).

:- pred init_external_preds(+Clauses, +Ap, +Resources, -ST) # "Insert into
   symbol table @var{ST} those body literals without definition in the
   current module.".

:- data defined_pred/1.
:- data imported_pred_obtained/0.
:- data imported_pred/1.

init_external_preds(Clauses, Ap, Resources, ST) :-
	retractall_fact(defined_pred(_)),
	get_imported_preds,
	external_preds(Clauses, Ps), !,
	insert_symtable(Ps, Ap, Resources, ST).

get_imported_preds :-
	current_fact(imported_pred_obtained), !.
get_imported_preds :-
	get_imported_modules,
	get_imported_calls(Ps),
	get_imported_preds_(Ps),
	asserta_fact(imported_pred_obtained).

get_imported_preds_([]) :- !.
get_imported_preds_([(_, _, Goal)|Preds]) :-
	functor(Goal, F, A),
	asserta_fact(imported_pred(F/A)),
	get_imported_preds_(Preds).

:- doc(bug, "Exported predicates not defined are not considered.").
external_preds(Clauses, Exts) :-
	get_defined_preds(Clauses, Lits_u),
	sort(Lits_u, Lits),
	filter_literals(Lits, Exts).

get_defined_preds([],                          []) :- !.
get_defined_preds([clause(Head, Body) :_|Cls], Ps) :- !,
	functor(Head, F, A),
	asserta_fact(defined_pred(F/A)),
	proc_body(Body, Lits),
	get_defined_preds(Cls, Litss),
	append(Lits, Litss, Ps).
get_defined_preds([_|Cls], Ps) :- !,
	get_defined_preds(Cls, Ps).

proc_body((Lit, Lits), [F/A|Ps]) :-
	myfunctor(Lit, F, A),
	proc_body(Lits, Ps).
proc_body((Lit), [F/A]) :-
	myfunctor(Lit, F, A).
myfunctor(Lit:_, F, A) :- !,
	functor(Lit, F, A).
myfunctor(Lit, F, A) :- !,
	functor(Lit, F, A).

filter_literals([],       []).
filter_literals([F/A|Ps], Exts) :-
	( current_fact(defined_pred(F/A)) ;
	    current_fact(imported_pred(F/A)) ;
	    builtin(F/A) ), !,
	filter_literals(Ps, Exts).
filter_literals([F/A|Ps], [F/A|Exts]) :- !,
	debug_message("~q/~q is an external predicate\n", [F, A]),
	filter_literals(Ps, Exts).

:- use_module(resources(init_res(trusted_res)), [get_modes_measures_costs/6]).

insert_symtable([],          _Approx, _Resources, _ST) :- !.
insert_symtable([F/A|Preds], Approx,  Resources,  ST) :-
	make_atom([F, A], PKey),
	get_measures(F/A, PKey, Measures),
	get_modes(F/A, PKey, Modes),
	insert_symbol_field(ST, F/A, det,      [1]),
	insert_symbol_field(ST, F/A, relation, inf),
	( current_pp_flag(prog_lang, ciao) ->
	    functor(Pred, F, A),
	    get_modes_measures_costs(Resources, Pred, Approx, Modes, Measures,
		Costs),
	    fill_trust_default(Resources, Approx, Costs),
	    insert_symbol_field(ST, F/A, time, [Costs]),
	    get_sizes(F/A, Approx, Modes, Measures, Size),
	    insert_symbol_field(ST, F/A, size, Size)
	;
	    true
	),
	insert_symbol_field(ST, F/A, mode,    Modes),
	insert_symbol_field(ST, F/A, measure, Measures),
	insert_symtable(Preds, Approx, Resources, ST).

fill_trust_default([],                   _,      []).
fill_trust_default([Resource|Resources], Approx, [Cost|Costs]) :-
	(var(Cost), trust_default(Approx, Resource, Cost) -> true ; true),
	fill_trust_default(Resources, Approx, Costs).

get_measures(F/A, _, Measures) :-
	current_pp_flag(prog_lang, java),
	!,
	get_measures_assrt(F/A, Measures), !.
get_measures(F/A, PKey, Measures) :-
	functor(Goal, F, A),
	get_info(regtypes, pred, PKey, Goal, (_Call, Succ_Type)),
	type2measure(Goal, Succ_Type, Measures), !.
get_measures(_, _PKey, _) :- !.
%	warning_message("No metrics assertions for ~q \n",[PKey]).

get_modes(F/A, _, Measures) :-
	current_pp_flag(prog_lang, java),
	!,
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
	apply_trusted_size0(Ap, F/A, Modes, Measures, Sizes), !.

% get_costs(Resources, Ap, F/A, Modes, Costs) :-
% 	functor(Term, F, A),
% 	make_atom([F, A], Key),
% 	get_litinfo(Term, 0, Key, Modes, _Measures, LitInfo),
% 	apply_resource_assertion(Resources, cost, Ap, LitInfo, Costs, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TABLE of BUILTINS (generated from builtin_res.pl)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
builtin((\+) /1).
builtin('andprolog_rt:&'/2).
builtin('arithmetic:is'/2).
builtin('term_basic:='/2).
builtin('term_basic:functor'/3).
builtin('term_basic:arg'/3).
builtin('term_compare:=='/2).
builtin('term_compare:\\=='/2).
builtin('arithmetic:=:='/2).
builtin('arithmetic:=\\='/2).
builtin('arithmetic:<'/2).
builtin('arithmetic:>'/2).
builtin('arithmetic:=<'/2).
builtin('arithmetic:>='/2).
builtin('arithmetic:arithexpression'/1).
builtin('term_typing:atomic'/1).
builtin('lists:nth'/3).
builtin('term_typing:atom'/1).
builtin('term_typing:number'/1).
builtin('term_typing:integer'/1).
builtin('basic_props:atm'/1).
builtin('basic_props:num'/1).
builtin('basic_props:int'/1).
builtin('basic_props:gnd'/1).
builtin('term_typing:float'/1).
builtin('term_typing:var'/1).
builtin('term_typing:nonvar'/1).
builtin('write:write'/1).
builtin('io_basic:tab'/1).
builtin('io_basic:nl'/0).
builtin('!'/0).
builtin(fail/0).
builtin(true/0).
builtin('aggregates:findall'/3).
