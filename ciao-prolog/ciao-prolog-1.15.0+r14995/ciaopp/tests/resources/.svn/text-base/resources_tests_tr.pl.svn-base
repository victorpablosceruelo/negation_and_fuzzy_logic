:- module(_, _, [assertions, fsyntax]).

:- use_module(ciaopp(tests(resources(resources_tests_config)))).
:- use_module(library(compiler(c_itf_internal)), [location/1]).
:- use_module(library(format)).

test_assertion(AExample, Assertion) :-
	Assertion =
	(
	    :- test analyze_file_and_output(Lang, Example, FileName, Output) :
	    (
		Example = AExample,
		example_file_name(Lang, Example, FileName),
		test_output(FileName, Output),
		test_pattern(FileName, Pattern)
	    ) =>
	    (
		file_equal(Pattern, Output)
	    ) + (is_det, not_fails) # Comment
	),
	sformat(Comment, "Resources test for ~w", [AExample]).

expand_tests((:- expand_this_test), Assertion, Module) :-
	ciao_alias(Module, AExample) ->
	test_assertion(AExample, Assertion)
    ;
	Assertion = [],
	(
	    atom_concat(_, '_pa', Module) ->
	    true
	;
	    location(loc(Src, Ln0, Ln1)),
	    messages([message_lns(Src, Ln0, Ln1, warning, ['Unable to expand test'])])
	).
