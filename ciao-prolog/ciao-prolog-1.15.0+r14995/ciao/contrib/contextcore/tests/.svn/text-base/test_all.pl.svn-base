:- module(test_all, [main/0], []).
:- use_module(.(demo1)).
:- use_module(.(demo2)).
:- use_module(.(ciao_icon)).
:- use_module(.(mandelbrot)).

:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(dec10_io)).

:- use_module(library(system), [system/2]).

main :-
	display('Testing contextcore'), nl,
	SavedFile = 'saved_output.txt',
	CurrentFile = 'current_output.txt',
	tell(CurrentFile),
	run_tests,
	told,
	compare_files(CurrentFile, SavedFile).

compare_files(CurrentFile, SavedFile) :-
	display('Comparing '), display(CurrentFile), display(' and '), display(SavedFile), nl,
	atom_concat(['diff ', CurrentFile, ' ', SavedFile], Command),
	system(Command, ReturnStatus),
	ReturnCode is (ReturnStatus /\ 0xFF00) >> 8,
	( ReturnCode = 0 ->
	    display('Ok!'), nl
	; display('There was some differences. Please, check them.'), nl
	).

run_tests :-
	( run_test__2 ->
	    true
	; display('Tests failed'), nl
	).

run_test__2 :-
	inform('Demo1...'),
	demo1:test,
	inform('Demo2...'),
	demo2:test,
	inform('Ciao icon demo...'),
	ciao_icon:main(['15']),
	inform('Mandelbrot demo...'),
	mandelbrot_test,
	inform('Done').

inform(Msg) :-
	display(user_error, Msg), nl(user_error).

mandelbrot_test :-
	tell('/tmp/mandel.pnm'),
	mandelbrot(256),
	told,
	system('md5sum /tmp/mandel.pnm', _). 
	
