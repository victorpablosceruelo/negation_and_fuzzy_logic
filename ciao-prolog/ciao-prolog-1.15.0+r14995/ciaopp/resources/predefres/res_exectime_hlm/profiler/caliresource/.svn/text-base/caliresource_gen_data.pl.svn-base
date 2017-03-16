:- module(caliresource_gen_data, [get_data_filename/1, get_data_filename/2,
		generate_data_table/2, load_data_table/0, load_data_table/1],
	    [assertions]).

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(hiordlib),   [minimum/3, map/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(file_utils), [output_to_file/2]).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(resources(resources_tools), [apply_cost_metrics/4,
		apply_data_to_terms/4]).
:- use_module(profcost(profdb)).
:- use_module(profcost(simuldata),  [check_types/2]).
:- use_module(profcost(type2value), [type_value/2]).
:- use_module(profcost(caliresource(caliresource_cost)),
	    [bench_metric/4, bench_model/2]).
:- use_module(profcost(caliresource(caliresource_measure_auto)), [measure/3]).

get_data_filename(FileName) :-
	get_os(Os),
	get_arch(Arch),
	atom_concat(Os, Arch, Platform),
	get_data_filename(Platform, FileName).

get_data_filename(Platform, FileName) :-
	absolute_file_name(profcost(caliresource), FileName0),
	atom_concat(Dir, 'caliresource.pl', FileName0),
	atom_concat([Dir, 'caliresource_', Platform, '_data_auto.pl'],
	    FileName).

generate_data_table(N, TimeOption) :-
	get_data_filename(FileName),
	output_to_file(fill_data_table(N, TimeOption), FileName).

load_data_table :-
	get_data_filename(FileName),
	load_data_table(FileName).

load_data_table(FileName) :-
	open_input(FileName, SI),
	repeat,
	read(Term),
	(
	    Term = end_of_file ->
	    !
	;
	    (
		Term = data_table(Pred, CA, Values, DataMetrics, Times) ->
		assertz_fact(data_table(Pred, CA, Values, DataMetrics, Times))
	    ;
		true
	    ),
	    fail
	),
	close_input(SI).

fill_data_table(N, TimeOption) :-
% 	register_module(profcost('caliresource/caliresource_bench_auto')),
% 	use_module(profcost(caliresource(caliresource_bench_auto))),
% 	use_module(profcost(caliresource(caliresource_cost))),
	measure_list(10, true, TimeOption, TimeFakeList),
%	do_measure_fake_list(20, TimeFakeList),
	minimum(TimeFakeList, '<', TimeFake),
% 	TimeFake = 0,
	(
	    bench_metric(Pred, ArgTypesIn, Metrics, UsedMetrics),
	    UsedMetrics0 =.. [usedm|UsedMetrics],
	    functor(Pred, Functor, Arity),
	    atom_concat([_, ':', PredName], Functor),
	    check_types(ArgTypesIn, ArgTypesInC),
% 	    Pred =.. [_|Args],
% 	    Pred2 =.. [PredName|Args],
% 	    module_loaded(Module, Path,_,_),
% 	    prepare_measure(Path, Pred2),
	    note(['Doing tests in ', Functor, '/', Arity]),
	    assert_cs_values(ArgTypesInC, N),
	    assert_cs_time(N, PredName, TimeFake, TimeOption),
	    assert_cs_data(N, Metrics, UsedMetrics),
% 	    findall([Time|Data],cs_data(_Index, Data), Datas),
	    findall(Value, cs_value(_Index, Value), Values),
	    data_table_dump(Pred, UsedMetrics0, Values),
	    fail
	;
	    retractall_fact(cs_model(_, _)),
	    retractall_fact(cs_time(_,  _)),
	    retractall_fact(cs_value(_, _)),
	    retractall_fact(cs_data(_,  _))
	).

:- data cs_model/2.
:- data cs_value/2.
:- data cs_data/2.
:- data cs_time/2.

% measure_list(N, Pred, Times) :-
% 	measure_list_(N, Pred, ~time_option, Times).

measure_list(0, _Pred, _TimeOption, []) :- !.
measure_list(N, Pred,  TimeOption,  [Time|Times]) :-
	measure(Pred, TimeOption, Time),
	N1 is N - 1,
	measure_list(N1, Pred, TimeOption, Times).

count(N, I) :-
	count_(N, 1, I).

count_(N, I0, I) :-
	N > 0,
	(
	    I = I0
	;
	    N1 is N - 1,
	    I1 is I0 + 1,
	    count_(N1, I1, I)
	).

assert_cs_values(ArgTypesInC, N) :-
	retractall_fact(cs_value(_, _)),
	(
	    count(N, Index),
	    (map(ArgTypesInC, type_value, Value) -> true ; true),
	    assertz_fact(cs_value(Index, Value)),
	    fail
	;
	    true
	).

assert_cs_time(N, PredName, TimeFake, TimeOption) :-
	retractall_fact(cs_time(_, _)),
	(
	    count(N, Index),
	    cs_value(Index, Value),
% 		display_list(['\r{NOTE: in ', Functor, '/', ArityA,
% 		    ' doing test ', Index, '/', N,'}']),
	    PredTest =.. [PredName|Value],
	    measure_list(5, PredTest, TimeOption, TimeGoalList),
%		do_measure_goal_list(Value, 10, TimeGoalList),
	    minimum(TimeGoalList, '<', TimeGoal),
% 		freeze_measure(2, PredTest, TimeGoal),
	    Time0 is TimeGoal - TimeFake,
	    (Time0 < 0 -> Time is -Time0 ; Time = Time0),
	    assertz_fact(cs_time(Index, Time)),
	    fail
	;
	    true
	).

assert_cs_data(N, Metrics, UsedMetrics) :-
	retractall_fact(cs_data(_, _)),
	(
	    count(N, Index),
	    cs_value(Index, Value),
	    apply_cost_metrics(Metrics, UsedMetrics, Value, Data),
	    assertz_fact(cs_data(Index, Data)),
	    fail
	;
	    true
	).

assert_cs_model(Model, UsedMetrics0) :-
	retractall_fact(cs_model(_, _)),
	(
	    cs_time(Index, Time),
	    cs_data(Index, Data),
	    apply_data_to_terms(Data, Model, UsedMetrics0, DataMetric),
	    assertz_fact(cs_model(Time, DataMetric)),
	    fail
	;
	    true
	).

data_table_dump(Pred, UsedMetrics0, Values) :-
	(
	    bench_model(Pred, Model),
	    assert_cs_model(Model, UsedMetrics0),
	    findall(Time,       cs_model(Time, _),          Times),
	    findall(DataMetric, cs_model(_,    DataMetric), DataMetrics),
	    portray_clause(data_table(Pred, resource_av, Values, DataMetrics,
		    Times)),
	    fail
	;
	    true
	).

% ----------------------------------------------------------------------------
:- multifile basic_type_value/3.

basic_type_value('caliresource_bench_auto:int_list', _, List) :-
	random(50, 500, Length),
	length(List, Length),
	fill_list_const(List, 5).
basic_type_value('caliresource_bench_auto:short_int_list', _, List) :-
	random(15, 100, Length),
	length(List, Length),
	fill_list_const(List, 5).
basic_type_value('caliresource_bench_auto:deep_list', _, List) :-
	random(15, 100, Length),
	length(List, Length),
	gen_deep_term(4, A),
	fill_list_const(List, A).
basic_type_value('caliresource_bench_auto:flat_list', _, List) :-
	random(15, 100, Length),
	length(List, Length),
	gen_flat_term(4, A),
	fill_list_const(List, A).
basic_type_value('caliresource_bench_auto:input_list', _, List) :-
	random(15, 100, Length),
	length(List, Length),
	gen_input_term(7, A),
	fill_list_const(List, A).

gen_deep_term(X, E) :-
	E = f(
	    a(b(c(d(e(f(g(h(i(j(
					   a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(
						a(b(c(d(e(f(g(h(i(j(X
						)
						))))))))))
						))))))))))
						))))))))))
						))))))))))
					 )))))))))).

gen_flat_term(X, E) :-
	E = f(
	    a, b, c, d, e, f, g, h, i, j,
	    a, b, c, d, e, f, g, h, i, j,
	    a, b, c, d, e, f, g, h, i, j,
	    a, b, c, d, e, f, g, h, i, j,
	    a, b, c, d, e, f, g, h, i, j, X).

gen_input_term(X, E) :-
	E = f(
	    X, X, X, X, X, X, X, X, X, X,
	    X, X, X, X, X, X, X, X, X, X,
	    X, X, X, X, X, X, X, X, X, X,
	    X, X, X, X, X, X, X, X, X, X,
	    X, X, X, X, X, X, X, X, X, X,

	    X, X, X, X, X, X, X, X, X, X,
	    X, X, X, X, X, X, X, X, X, X,
	    X, X, X, X, X, X, X, X, X, X,
	    X, X, X, X, X, X, X, X, X, X,
	    X, X, X, X, X, X, X, X, X, X
	).

fill_list_const([],     _X).
fill_list_const([X|As], X) :-
	fill_list_const(As, X).
