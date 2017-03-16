:- module(_, _, [assertions]).

:- doc(author, "Edison Mera").

:- doc(module, "This module contains useful predicates to get
	resource information given by the analysis.").

:- use_module(library(terms)).
:- use_module(library(lists)).

:- use_module(infer(infer_db)).

:- use_module(resources(resources_basic)).
:- use_module(resources(resources_eval)).



verify_modes([],           []).
verify_modes([Mode|Modes], [Arg|Args]) :-
	verify_mode(Mode, Arg),
	verify_modes(Modes, Args).

verify_mode('+', Arg) :-
	ground(Arg).
verify_mode('-', Arg) :-
	var(Arg).

apply_cost_metrics([],               [],              [],             []).
apply_cost_metrics([Metric|Metrics], [N|UsedMetrics], [Value|Values], Datas) :-
	( N == 0 ->
	    Datas = Datas1
	; apply_cost_metric(Metric, Value, Data),
	    Datas = [Data|Datas1]
	),
	apply_cost_metrics(Metrics, UsedMetrics, Values, Datas1).

:- use_module(resources(size_res(ground_size_res)), [ground_size/2]).

apply_cost_metric(length, Value, Data) :-
	length(Value, Data),
	!.
apply_cost_metric(void, _Value, 0) :- !.
apply_cost_metric(size, Value,  Data) :-
	ground_size(Value, Data),
	!.
apply_cost_metric(_, Value, Value).

apply_datas_to_terms([],           _Terms, _UsedMetrics, []).
apply_datas_to_terms([Data|Datas], Terms,  UsedMetrics,  [Line|Lines]) :-
	!,
	apply_data_to_terms(Data, Terms, UsedMetrics, Line),
	apply_datas_to_terms(Datas, Terms, UsedMetrics, Lines).
apply_data_to_terms(Data, Terms, UsedMetrics, Line) :-
	TermData0 =.. [metrics|Data],
	TermData = data([], TermData0, UsedMetrics),
	eval_term(Terms, TermData, Line).

evaluate_resources(Pred, Approx, Resources, Values) :-
	Pred = Module: Predicate,
	functor(Predicate, Name, NumArgs),
	Predicate =.. [Name|Args],
	atom_concat([Module, ':', Name], ModName),
	functor(PredName, ModName, NumArgs),
	inferred(resources, PredName, complexity(_Call_VarType,
		_Succ_VarType, Modes, Metrics, _Mutex, _Solution_Det,
		_Out_Size, _Relation, Approx, Resources0,
		[CostFunctions0|_], _Domain)),
	map_resources(Resources0, CostFunctions0, Resources, CostFunctions),
	verify_modes(Modes, Args),
	mark_used_metrics(Metrics, CostFunctions, UsedMetrics, _),
	apply_cost_metrics(Metrics, UsedMetrics, Args, Sizes),
	apply_data_to_terms(Sizes, CostFunctions, UsedMetrics, Values).

map_resources([],                    [],
	    [],                   []).
map_resources([Resource|Resources0], [CostFunction|CostFunctions0],
	    [Resource|Resources], [CostFunction|CostFunctions]) :-
	!,
	map_resources(Resources0, CostFunctions0, Resources, CostFunctions).
map_resources([_Resource|Resources0], [_CostFunction|CostFunctions0],
	    Resources, CostFunctions) :-
	map_resources(Resources0, CostFunctions0, Resources, CostFunctions).
