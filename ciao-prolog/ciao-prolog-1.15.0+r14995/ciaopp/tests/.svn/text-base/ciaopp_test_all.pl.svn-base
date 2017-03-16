:- module(ciaopp_test_all, [main/0, test_all/0], [ciaopaths, assertions]).

:- use_module(library(modtester)).

:- doc(module, "This is the main file for testing ciaopp functionality. 
          It basically traverses all subdirectories executing the corresponding 
	  tests.").

main :-
	test_all.

:- test test_all.

test_all :-
	L = [
	    'Plai/test_plai',
	    'Spec/test_spec',
	    'Out/test_out',
	    'Check_Fixp/test_check_fixp',
	    'Plai_types/test_plai_types'
	],
	modules_tester('log/ciaopp', L).
