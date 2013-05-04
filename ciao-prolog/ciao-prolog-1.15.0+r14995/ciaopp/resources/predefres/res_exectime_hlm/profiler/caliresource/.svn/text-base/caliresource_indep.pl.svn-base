:- module(caliresource_indep, _,
	    [fsyntax, assertions, unittestprops, regtypes]).

:- use_module(library(write)).
:- use_module(library(file_utils), [output_to_file/2]).
:- use_module(library(aggregates)).
:- use_module(ciaopp(driver), [module/1, analyze/1]).
:- use_module(infer(infer_db)).
:- use_module(program(p_unit), [entry_assertion/3]).
:- use_module(profcost(profconfig)).
:- use_module(resources(res_assrt_defs(resources_lib)), [get_all_resources/1]).
:- use_module(resources(resources_basic)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% plugin-like modules that define analyzers: -- EMM
:- use_module(ciaopp(resources), []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- doc(author, "Edison Mera").

:- doc(module, "This module do not depend of auto generated modules.").

analyze_calibration_tests :-
	setof(Module, module_calibrate(Module), ML),
	absolute_file_name(profcost(caliresource), File0),
	atom_concat(File1, '.pl',               File0),
	atom_concat(File1, '_cost_inferred.pl', File),
	output_to_file(calibration_tests_dump(ML), File).

calibration_tests_dump(ML) :-
	display_list([
		'%% Warning!!! This module was auto generated in ',
		~this_module, '\n',
		'%% Do not change it or the changes made by you will be lost\n'
	    ]),
	(
	    member(Module, ML),
	    proc_benchmarks(profcost(caliresource(Module))),
	    fail
	;
	    true
	),
	get_all_resources(A),
	portray_clause(valid_resources(A)).

proc_benchmarks(Module) :-
	output_to_file(apply_analysis(Module), '/dev/null'),
	write_bench_models,
	write_bench_metrics,
	!.

apply_analysis(Module) :-
	module(Module),
	analyze(eterms),
	analyze(shfr),
	analyze(nfg),
	analyze(resources).

write_bench_models :-
	entry_assertion(PredName, _, _),
	current_inferred(resources, PredName, _, ub, _, CostFunctions),
	portray_clause(bench_model(PredName, CostFunctions)),
	fail
    ;
	true.

write_bench_metrics :-
	entry_assertion(Pred, ArgTypesIn, _),
	Approx = ub,
	current_inferred(resources, Pred, Metrics, Approx, _, ModelUpper),
	mark_used_metrics(Metrics, ModelUpper, UsedMetrics, _),
	portray_clause(bench_metric(Pred, ArgTypesIn, Metrics, UsedMetrics)),
	fail
    ;
	true.

current_inferred(InferType, PredName, Metrics, Approx, Resources,
	    CostFunctions) :-
	inferred(InferType, PredName, complexity(_Call_VarType,
		_Succ_VarType, _Mode, Metrics, _Mutex, _Solution_Det,
		_Out_Size, _Relation, Approx, Resources, CostFunctions,
		_Domain)).

current_inferred_resource(PredName, Metrics, Approx, Resources,
	    CostFunctions) :-
	current_inferred(resources, PredName, Metrics, Approx, Resources,
	    CostFunctions),
%       Top and bottom must be ignored:
	\+ (vector_is_infinite(CostFunctions)). %; vector_is_zero(Model) ).

module_calibrate(Module) :-
	pred_calibrate(Module: _Name / _Arity).

% compose_pred(Module:Name/Arity, Pred) :-
% 	atom_concat([Module,':',Name],Functor),
% 	functor(Pred, Functor, Arity).
