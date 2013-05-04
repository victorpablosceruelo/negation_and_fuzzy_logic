:- module(resources_tests_basic, _, [assertions, fsyntax]).

:- use_module(library(file_utils)).
:- use_module(library(terms)).
:- use_module(library(system)).

lang_extension(ciao, '.pl').
lang_extension(java, '.java').

:- export(example_file_name/3).
example_file_name(Lang, Example, FileName) :-
	lang_extension(Lang, Extension),
	absolute_file_name(Example, '_opt', Extension, '.', FileName, _, _).

:- pred file_equal/2 # "Succeeds if the given files are equal,
	fails if not.".

file_equal(File1, File2) :-
	get_diff(File1, File2, S),
	(
	    S == [] -> true
	;
	    throw(error(file_differs, File1, File2, S))
	).

% file_equal(A, B) :-
% 	absolute_file_name(A, AF),
% 	absolute_file_name(B, BF),
% 	file_to_string(AF, AS),
% 	file_to_string(BF, BS),
% 	AS == BS,
% 	!.

get_diff(A, B, S) :-
	popen(~atom_concat(['diff -ruN ', A, ' ', B]), read, OS),
	stream_to_string(OS, S).

test_output(Example, Output) :-
	atom_concat(ExampleBase, '.pl', Example),
	atom_concat(ExampleBase, '_co.pl', Output).

test_pattern(Example, Pattern) :-
	atom_concat(ExampleBase, '.pl',    Example),
	atom_concat(ExampleBase, '_pa.pl', Pattern).
