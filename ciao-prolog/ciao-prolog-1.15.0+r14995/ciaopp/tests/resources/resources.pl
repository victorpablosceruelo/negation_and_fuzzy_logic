:- package(resources).
:- use_package(resdefs).
:- use_package(unittestprops).

:- load_test_module(ciaopp(tests(resources(resources_tests)))).
:- load_test_module(ciaopp(tests(resources(resources_tests_basic)))).
:- load_compilation_module(ciaopp(tests(resources(resources_tests_tr)))).
% TODO: uncertain priority: custom expansion
:- add_sentence_trans(expand_tests/3, 9110).

% This declaration will be expanded as a unit-test assertion in order
% to test this example:

:- expand_this_test.
