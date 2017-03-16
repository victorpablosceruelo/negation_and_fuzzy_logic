:- module(resources_tests,
	    [
		initialize_example/1,
		initialize_example/2,
		initialize_examples/0,
		initialize_java_examples/0,
		initialize_ciao_examples/0,
		analyze_file_and_output/4
	    ], [assertions, fsyntax]).

:- doc(author, "Edison Mera").

:- doc(module, "Integration tests of Resources Analysis.").

:- doc(summary, "This module is used to test how the Resource
	Analysis works together with all the additional required
	analyses, and to cover all possible cases of cost functions.
	The examples in this directory were not designed for didactic
	purposes, they are not elegant, so don't modify them unless
	you understand the effect that such examples have in the
	analyzer.").

:- use_module(ciaopp(driver)).
:- use_module(ciaopp(printer)).

% Eager load of lazy-loadable analyzers:
:- use_module(ciaopp(resources), []).
:- use_module(ciaopp(infercost), []).

:- use_module(ciaopp(tests(resources(resources_tests_utils)))).
:- use_module(ciaopp(tests(resources(resources_tests_basic)))).
:- use_module(ciaopp(tests(resources(resources_tests_config)))).

:- use_module(ciaopp(resources), []).
:- use_module(ciaopp(infercost), []).

:- doc(analyze_file_and_output/4, "Support file used to facilitate
	testing of resource analysis.").

analyze_file_and_output(Lang, Example, FileName, OutFile) :-
	module(FileName),
	apply_analysis(Lang, Example),
	(skip_old(Example) -> true ; apply_old_analysis(Lang)),
	output(OutFile),
	!.

apply_analysis(ciao, AExample) :-
	type_analysis(AExample, TypeAn),
	analyze(TypeAn),
	analyze(shfr),
	analyze(nfg),
	analyze(resources).
apply_analysis(java, _) :-
	analyze(java_cha),
	analyze(java_nullity),
	analyze(resources).

apply_old_analysis(ciao) :-
	analyze(steps_ualb).

:- doc(initialize_examples/0, "Generates the output for each
	example that will be used as the base for further
	comparison.").

initialize_ciao_examples :-
	initialize_example(ciao, ~ciao_example),
	fail
    ;
	true.

initialize_java_examples :-
	initialize_example(java, ~java_example),
	fail
    ;
	true.

initialize_example(M) :-
	ciao_alias(M, A),
	initialize_example(ciao, A).

initialize_examples :-
	load_profiling_results,
	initialize_ciao_examples.
% 	initialize_java_examples.

initialize_example(Lang, Example) :-
	example_file_name(Lang, Example, FileName),
	test_pattern(FileName, OutFile),
	analyze_file_and_output(Lang, Example, FileName, OutFile).
