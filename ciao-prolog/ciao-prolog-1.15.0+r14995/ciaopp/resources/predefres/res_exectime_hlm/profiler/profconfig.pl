:- module(profconfig, [confidence_interval/1, pred_calibrate/1,
		descriptive_calibrator_name/2], [fsyntax]).

%% The data are filtered in two steps using the confidence interval with an
%% optimal dispersion interval of level (1 - a) equal to [-Z, Z].

%% gauss(99%)   = 2.58
% confidence_interval(2.58).
%% gauss(99.9%) = 3.29
confidence_interval(3.29).

% list of pred in which resources for ub and lb are equals
pred_calibrate(PredDesc) :-
	descriptive_calibrator_name(PredDesc, _).

% descriptive_calibrator_name('caliresource_bench_auto:vart'(_,_,_,_,_,_,_,_),
% 	'var test').

descriptive_calibrator_name(caliresource_bench_auto:verify_list_type/1,
	    'Browse a list').
descriptive_calibrator_name(caliresource_bench_auto:unify_two_lists/2,
	    'Unify two lists element by element').
descriptive_calibrator_name(caliresource_bench_auto:list_many_args/9,
	    'Head with many arguments').
descriptive_calibrator_name(caliresource_bench_auto:list_var_input/2,
	    'Browse a list of terms with input vars').
descriptive_calibrator_name(caliresource_bench_auto:list_var_output/2,
	    'Browse a list of terms with output vars').
descriptive_calibrator_name(caliresource_bench_auto:list_deep_ground_input/2,
	    'Unify a list of input deep ground terms element by element').
descriptive_calibrator_name(caliresource_bench_auto:list_flat_ground_input/2,
	    'Unify a list of input flat ground terms element by element').
descriptive_calibrator_name(caliresource_bench_auto:list_flat_ground_output/2,
	    'Unify a list of output flat ground terms element by element').
descriptive_calibrator_name(caliresource_bench_auto:list_deep_ground_output/2,
	    'Unify a list of output deep ground terms element by element').
descriptive_calibrator_name(caliresource_bench_auto:list_no_last_call_opt/1,
	    'Browse a list avoiding last call optimization').
descriptive_calibrator_name(caliresource_bench_auto:lcall/0,
	    'Test of calls to predicates with no arguments').
descriptive_calibrator_name(caliresource_bench_auto:environment/0,
	    'Test of calls to predicates that creates environment').

%descriptive_calibrator_name('caliresource_bench_auto:nrev'(_,_),'nrev').
%descriptive_calibrator_name('caliresource_bench_auto:append'(_,_,_),'append').
%descriptive_calibrator_name('caliresource_bench_auto:env',      'environment test').

%descriptive_calibrator_name('caliresource_bench_auto:listenv'(_),'list (env)').
