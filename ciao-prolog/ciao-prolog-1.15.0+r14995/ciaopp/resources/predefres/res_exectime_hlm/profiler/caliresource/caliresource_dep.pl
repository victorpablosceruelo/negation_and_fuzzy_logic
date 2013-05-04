:- module(caliresource_dep,
	    [
		individual_model_params_dump/0, global_model_params_dump/0,
		fill_global_model_params/0, fill_all_model_params/0,
		get_model_params_data_filename/3],
	    [fsyntax, assertions, unittestprops, regtypes, hiord]).

:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(math)).
:- use_module(library(system_extra), [ls/3]).
:- use_module(profcost(profdb)).
:- use_module(profcost(profconfig)).
:- use_module(profcost(caliresource(caliresource_basic))).
:- use_module(profcost(caliresource(caliresource_params))).
:- use_module(profcost(caliresource(caliresource_cost)),
	    [valid_resources/1]).
:- use_module(profcost(caliresource(caliresource_gen_data))).
:- use_module(res_exectime_hlm(res_exectime_hlm_gen)).
:- use_module(res_exectime_hlm(res_exectime_hlm_dep)).
:- use_module(profcost(caliresource(caliresource_dep_utils))).

:- doc(author, "Edison Mera").

:- doc(module, "This module depends on auto generated modules.").

fill_all_model_params :-
	absolute_file_name(profcost(caliresource), FileName0),
	atom_concat(Dir, 'caliresource.pl', FileName0),
	ls(Dir, 'caliresource_*_data_auto.pl', FileBases),
	list(FileBases, gen_model_params(Dir)),
	res_exectime_hlm_gen_all.

gen_model_params(FileBase, Dir) :-
	atom_concat(Dir, FileBase, FileName),
	get_data_filename(Platform, FileName),
	note(['Generating cost models for ', Platform]),
	cleanup_data_table_db,
	load_data_table(FileName),
	fill_individual_model_params,
	fill_global_model_params,
	valid_resources(AllResources),
	gen_all_hlm_dep_files(Platform, AllResources).

:- meta_predicate get_model_params_data_filename(?, ?, pred(2)).
get_model_params_data_filename(FileName, AllResources, ResourcesSelector) :-
	cleanup_data_table_db,
	load_data_table(FileName),
	fill_individual_model_params,
	fill_model_params(AllResources, ResourcesSelector).

individual_model_params_dump :-
	display('\nConstants in the cost model for each test (ms)\n'),
	display('===============================\n'),
	display('Model: Time = K . Cost\n'),
	display('===============================\n'),
	model_params(Pred, Resources, Params),
	params_MRSS(Params, MRSS),
	params_Solution(Params, K),
	params_StdError(Params, StdError),
	params_TValue(Params, TValue),
	vector_constant_multiply(TValue, 100, Percent),
	functor(Pred, Name, Arity),
	display_list([
		'Pred     = \t', Name, '/', Arity, '\n',
		'Resources= \t', Resources, '\n',
		'K        = \t', K, '\n',
		'error(K) = \t', StdError, '\n',
		'  %      = \t', Percent, '\n',
		'MRSS     = \t', MRSS, '\n',
		'-------------------------------\n'
	    ]),
	fail
    ;
	true.

global_model_params_dump :-
	display('\nConstants in the cost model (ms)\n'),
	display('===============================\n'),
	display('Model: Time = K . Cost\n'),
	display('===============================\n'),
	global_model_params(Resources, Params),
	global_model_params_each_dump(Resources, Params),
	fail
    ;
	true.

global_model_params_each_dump(Resources, Params) :-
	params_Solution(Params, K),
	params_LowerBoundSolution(Params, KLower),
	params_UpperBoundSolution(Params, KUpper),
	params_StdError(Params, StdError),
	params_TValue(Params, TValue),
	vector_constant_multiply(TValue, 100, Percent),
	params_MRSS(Params, MRSS),
	display_list([
		'Model    = \t', Resources, '\n',
		'K        = \t', K, '\n',
		'lower(K) = \t', KLower, '\n',
		'upper(K) = \t', KUpper, '\n',
		'error(K) = \t', StdError, '\n',
		'  %      = \t', Percent, '\n',
		'MRSS     = \t', MRSS, '\n',
		'-------------------------------\n'
	    ]).

% models built using each calibration test, individual w.r.t. the
% test, not to the model.

valid_individual_model([A]) :-
	valid_resources(M),
	member(A, M).


fill_individual_model_params :-
	confidence_interval(Z),
	retractall_fact(model_params(_, _, _)),
	valid_resources(Resources),
	( data_table(Pred, _CA, _Values, AllDataMetrics, Times0),
%	    Type = 'nd',
	    valid_individual_model(IndividualModel),
	    vector_project_list(AllDataMetrics, Resources, IndividualModel,
		DataMetrics0),
	    preprocess_data(DataMetrics0, Times0, Z, Params),
	    assertz_fact(model_params(Pred, IndividualModel, Params)),
	    fail
	; true
	).

fill_global_model_params :-
	valid_resources(Resources),
	fill_model_params(Resources, selected_resources).

:- meta_predicate fill_model_params(?, pred(2)).

fill_model_params(Resources, ResourcesSelector) :-
	retractall_fact(global_model_params(_, _)),
	findall(TimesE,
	    data_table(_Pred, _CA, _Values, _DataMetricsE, TimesE), TimesList),
	list_concat(TimesList, Times0),
	(
	    ResourcesSelector(Resources, SelectedResources),
	    get_global_model_params(Resources, SelectedResources, _Z, Params,
		Times0),
	    assertz_fact(global_model_params(SelectedResources, Params)),
	    fail
	;
	    true
	).

get_global_model_params(AllResources, Resources, Z, Params, Times0) :-
	findall(DataMetricsE,
	    selected_data_metrics(AllResources, Resources, DataMetricsE),
	    DataMetricsList),
	list_concat(DataMetricsList, DataMetrics0),
	preprocess_data(DataMetrics0, Times0, Z, Params).

selected_data_metrics(AllResources, Resources, DataMetricsE) :-
	data_table(_Pred, _CA, _Values, AllDataMetrics, _TimesE),
	vector_project_list(AllDataMetrics, AllResources, Resources,
	    DataMetricsE).

preprocess_data(Data, Time, _Z, Params) :-
	data2params(Data, Time, Params).

:- test data2params(A, B, C) : (
	    A = [[1, 58, 111],
		[1, 84, 131],
		[1, 78, 158],
		[1, 81, 147],
		[1, 82, 121],
		[1, 102, 165],
		[1, 85, 174],
		[1, 102, 169]],
	    B = [64, 78, 83, 88, 89, 99, 101, 102])
	=> (
	    near(C, params(8, 3,
		    [[-2.82842712474619, -237.58787847868, -415.7787873376899],
			[0.0, -37.0675059857013, -45.02595887200545],
			[0.0, 0.0, -43.1122143673516]],
		    [9.053849885836959, 0.520278972183837, 0.2397463704130662],
		    176.345479271502), 1.0e-10
	    )) # "Example with a 3x8 data matrix.".

% Minimum square solver, this returns the required params to do
% statistical inferences

data2params(Data, Time, params(Rows, Cols, DDT, Solution, RSS)) :-
% The next operations are done here to improve performance:
	matrix_rows(Data, Rows),
	matrix_cols(Data, Cols),
	matrix_lssolve(Data, [Time], DDT, [Solution], [Residual]),
	rss(Residual, RSS).

% preprocess_data(Data, Z, Params, Accepted, Rejected) :-
% 	discrimine_data(Data, [], [], Z, Params, Accepted, Rejected).

% discrimine_data(Data0, Accepted0, Rejected0, Z, Params, Accepted, Rejected) :-
% 	data2params(Data0, Params0),
% 	filterdata(Data0, Z, Params0, Accepted0, Rejected0, Accepted,
% 	    Rejected),
% 	data2params(Accepted, Params),
% 	!.

% filterdata([],         _Z,_Params, Accepted , Rejected , Accepted, Rejected).
% filterdata([Data|Datas],Z, Params, Accepted0, Rejected0, Accepted, Rejected) :-
% 	Params = [_SumY, _SumX, _SumY2_X, K, Variance],
% 	Data = [Y,X],
% 	(   ((Y/X - K)/Z)**2 =< Variance ->
% 	    Accepted = [Data|Accepted1],
% 	    Rejected = Rejected1
% 	;   Accepted = Accepted1,
% 	    Rejected = [Data|Rejected1]
% 	),
% 	filterdata(Datas, Z, Params, Accepted0, Rejected0, Accepted1,
% 	    Rejected1).
