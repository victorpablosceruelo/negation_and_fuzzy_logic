:- module(trusted_res, [
		translate_assrt_function_to_gen_form/4,
%		init_trusted_costs/3,
		get_modes/4,
		get_modes_measures_costs/6
	    ], [assertions, isomodes,
	        regtypes,
	        fsyntax,
		api(api_internal_dec),
		resources(inferres_decl), % equiv to regtypes + argnames
		library(resdefs(resources_decl))]).

:- use_module(library(messages)).
:- use_module(resources(resources_basic)).
:- use_module(library(keys), [key_lookup/4]).
:- use_module(infer(infer), [type2measure/3]).
:- use_module(infer(gather_modes), [vartypes_to_modes/2]).
:- use_module(library(resdefs(rescostfunc)), [compact_cf/3]).

:- use_module(program(assrt_db), [
		assertion_read/9,
		assertion_body/7]).

:- doc(author, "Edison Mera.").

:- doc(module, "This module contains predicates that returns
	relevant information for cost analysis that are present in
	trust assertions.").

% init_trusted_costs(Approx, Resources, ST) :-
% 	get_trusted_preds(Preds, Approx, Resources),
% 	insert_comps_in_symtable(Preds, Approx, Resources, ST).

% get_trusted_preds(Preds, Approx, Resources) :-
% 	sort(~findall(Pred, verify_pred_assertion(Pred, Approx,
% 		    Resources)), Preds).

% verify_pred_assertion(Pred, Approx, Resources) :-
% 	assertion_read(Pred, _M, trust, comp, Body, _Dic, _S, _LB, _LE),
% 	assertion_body(_, _Compat, _Call, _Succ, Comp, _Comm, Body),
% 	(member('resources_props:cost'(Pred, Approx, Resource, _), Comp),
% 	    member(Resource, Resources) -> true).

% get_costs([],                   _,    _,      []          ).
% get_costs([Resource|Resources], Pred, Approx, [Cost|Costs]) :-
% 	get_cost(Resource, Pred, Approx, Cost),
% 	get_costs(Resources, Pred, Approx, Costs).

% get_cost(Resource, Pred, Approx, Cost) :-
% 	get_cost_properties(Resource, Pred, Approx, _, _, Value, Loc) ->
% 	translate_assrt_function_to_gen_form(Pred, _,
% 	    resources(Value), resources(Cost))
%     ;
% 	warning_message(Loc, "No trust defined for ~w", [Resource]).

:- doc(bug, "Note that we are supposing that all resources have
	the same mode and measures.").

get_cost_properties(Resource, Pred, Approx, Call, Succ, CF, loc(S, LB, LE)) :-
	assertion_read(Pred, _, trust, comp, CBody, _, S, LB, LE),
	assertion_body(_, _, Call, _, Comp, _, CBody),
	(
	    assertion_read(Pred, _M, _, success, SBody, _Dic, _, _, _),
	    assertion_body(_, _, Call, Succ, _, _, SBody) ->
	    true
	;
	    Succ = []
	),
% 	get_assertion(Pred, as${status => trust, comp => Comp,
% 		call => Call, succ => Succ, locator => Loc}),
	member('resources_props:cost'(Pred, _, Approx, _, Resource, _, IF, CFN), Comp),
	compact_cf(CFN, IF, CF),
	!.

get_modes_measures_cost_each(Resource, Pred, Approx, Modes, Measures, Cost,
	    Loc) :-
	get_cost_properties(Resource, Pred, Approx, Call, Succ, Value, Loc),
	vartypes_to_modes(vartype(Pred, Call, Succ), Modes),
	type2measure(Pred, Succ, Measures),
	translate_assrt_function_to_gen_form(Pred, _, resources(Value),
	    resources(Cost)).

:- doc(bug, "The modes, measures and costs are strongly related,
	i. e., for different modes the cost could be different, that
	is why the following predicate is defined. --EMM").

:- doc(bug, "The cost also depends on the type of the arguments, a
	thing that has not been considered yet. --EMM").

:- pred get_modes_measures_costs/6 :: ( list(resource) * term * approx * list *
	    list * list ).

get_modes_measures_costs([],                   _,    _,      _,     _,
	    []).
get_modes_measures_costs([Resource|Resources], Pred, Approx, Modes, Measures,
	    [Value|Values]) :-
	(
	    get_modes_measures_cost_each(Resource, Pred, Approx, Modes1,
		Measures1, Value, Loc) ->
	    (
		Modes = Modes1 ->
		true
	    ;
		warning_message(Loc,
		    "In predicate ~w, using already defined mode ~w for "||
		    "resource ~w because it is different than mode ~w. Check "||
		    "that the modes in all resouces be the same.",
		    [Pred, Modes, Resource, Modes1])
	    ),
	    (
		Measures = Measures1 ->
		true
	    ;
		warning_message(Loc,
		    "In predicate ~w, Using already defined measure ~w for "||
		    "resource ~w because it is different than measure ~w. "||
		    "Check that the measures in all resouces be the same.",
		    [Pred, Measures, Resource, Measures1])
	    )
	;
	    true
	),
	get_modes_measures_costs(Resources, Pred, Approx, Modes, Measures,
	    Values).

get_modes([],                   _,    _,      _).
get_modes([Resource|Resources], Pred, Approx, Modes) :-
	(
	    get_cost_properties(Resource, Pred, Approx, Call, Succ, _, Loc) ->
	    vartypes_to_modes(vartype(Pred, Call, Succ), Modes1)
	;
	    true
	),
	(
	    Modes = Modes1 ->
	    true
	;
	    warning_message(Loc, "Using already defined mode ~w for "||
		"resource ~w because it is different than mode ~w. Check "||
		"that the modes in all resouces be the same.",
		[Modes, Resource, Modes1])
	),
	get_modes(Resources, Pred, Approx, Modes).

% get_measures([],                   _,    _,      _       ).
% get_measures([Resource|Resources], Pred, Approx, Measures) :-
% 	(
% 	    get_cost_properties(Resource, Pred, Approx, _Call, Succ, _, Loc) ->
% 	    type2measure(Pred, Succ, Measures1)
% 	;
% 	    true
% 	),
% 	(
% 	    Measures = Measures1 ->
% 	    true
% 	;
% 	    warning_message(Loc, "Using already defined measure ~w for "||
% 		"resource ~w because it is different than measure ~w. Check "||
% 		"that the measures in all resouces be the same.",
% 		[Measures, Resource, Measures1])
% 	),
% 	get_measures(Resources, Pred, Approx, Measures).

% insert_comps_in_symtable([],           _Approx, _Resources, _ST).
% insert_comps_in_symtable([Pred|Preds], Approx,  Resources,  ST ) :-
% 	get_modes(Resources, Pred, Approx, Modes),
% 	get_measures(Resources, Pred, Approx, Measures),
% 	get_costs(Resources, Pred, Approx, Costs),
% 	functor(Pred, F, A),
% 	insert_symbol_field(ST, F/A, mode,     Modes   ),
% 	insert_symbol_field(ST, F/A, measure,  Measures),
% 	insert_symbol_field(ST, F/A, det,      [1]     ),
% 	insert_symbol_field(ST, F/A, relation, inf     ),
% 	insert_symbol_field(ST, F/A, time,     [Costs] ),
% 	insert_comps_in_symtable(Preds, Approx, Resources, ST).

:- true pred translate_assrt_function_to_gen_form(+Goal, +Measure, +Assrt,
	    -Prop)
# "This predicate translates from an expression @var{Assrt} given by the
user corresponding to @var{Goal} to a general form expression @var{Prop}
understood by the analysis. If @var{Measure} is not var, then it contains
the a list of metrics corresponding to each @var{Goal} argument. In this
case, it is verified together with the translation that the metrics are
compatible. Otherwise, the previous verification is not performed.".

/* EXAMPLE:
	?- translate_assrt_function_to_gen_form( 'nrev_assrt:nrev'( A, B ),
	    steps_ub( 0.5 *exp( length( A ), 2 ) +1.5 *length( A ) +1 ),
	    GenFormProp ).

GenFormProp = steps_ub( 0.5 *exp( $( 0, 1 ), 2 ) +1.5 * $( 0, 1 ) +1 ) ?

	yes
	*/

translate_assrt_function_to_gen_form(Goal, Measure, AssrtCompProp,
	    GenFormProp) :-
	Goal=..[_|Args],
	AssrtCompProp=..[CompProp, AssrtExp],
	create_dict(Args, 0, Dic),
	rename_assrt_function(AssrtExp, Dic, Measure, GenFormExp),
	GenFormProp=..[CompProp, GenFormExp].

rename_assrt_function(AssrtExp, Dic, Measure, '$'(0, HeadArgNum)) :-
	functor(AssrtExp, F, A),
	valid_size_measure(F/A),
	!,
	compatible_measure(F, Measure),
	arg(1, AssrtExp, HeadVar),
	key_lookup(HeadVar, Dic, HeadArgNum, _).
rename_assrt_function([],                   _,   _,
	    []) :- !.
rename_assrt_function([AssrtExp|AssrtExps], Dic, M,
	    [GenFormExp|GenFormExps]) :-
	!,
	rename_assrt_function(AssrtExp,  Dic, M, GenFormExp),
	rename_assrt_function(AssrtExps, Dic, M, GenFormExps).
rename_assrt_function(AssrtExp, Dic, M, Term) :-
	AssrtExp=..[F|Args],
	rename_assrt_function(Args, Dic, M, GenFormExps),
	Term=..[F|GenFormExps].

create_dict([],       _,  []).
create_dict([A|Args], N0, [A=N|Dic]) :-
	N is N0 +1,
	create_dict(Args, N, Dic).
