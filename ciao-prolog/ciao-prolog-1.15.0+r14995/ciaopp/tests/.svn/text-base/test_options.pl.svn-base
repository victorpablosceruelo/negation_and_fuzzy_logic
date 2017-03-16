:- module(test_options , [get_test_options/2] , []).



:- use_module(library(idlists), [subtract/3]).

:- use_module(benchmarks, [benchmarks_of/2]).

:- data filtering_of/3.

% --- DTM: Who wrote this forgot to put the SETTINGS file or write down
%     in readme how to generate it.
%:- include('SETTINGS').
:- include('settings.pl').

get_test_options(Test, [ModuleList, TestDir, GoodDir|Rest]) :-
	test_options(Test, [Bench, Good|Rest]),
	test_dir(Bench, TestDir),
	good_dir(Good, GoodDir),
	benchmarks_of(Bench, ModuleList0),
	(filtering_of(Test, Bench, FilterList)
	-> subtract(ModuleList0, FilterList, ModuleList)
	; ModuleList = ModuleList0
	).
