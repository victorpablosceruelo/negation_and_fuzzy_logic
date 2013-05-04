:-module( peval_test , [main/0] , [assertions] ).


:- use_module(library(tester), [run_tester/9]).
:- use_module(library(lists), [length/2]).

:- use_module(library(ciaopp)).

:- doc(module,"This is the main file for testing ciaopp functionality. 
          It basically traverses all subdirectories executing the corresponding 
	  tests.").




main :-
	L = [ 
		'rev.pl',
		'rec3.pl',
		'rec2.pl',
		'rec.pl',
		'qsortapp.pl',
		'partconc.pl',
		'motivating.pl',
		'example.pl',
		'dapp.pl',
		'app_two_calls.pl',
		'app_not_finished.pl',
		'app_none.pl',
		'app_direct.pl',
		'app.pl'
	    ],

 	run_tester(
		      'peval_ana_test.log',
		      'peval_ana_result.log',
		      init_func ,
		      auto_optimize ,
		      L,
		      checker_func,
		      L,
		      true,
		      Res
		  ),
	length( L , LL ),
	message( note , [ 'Number of correct analyzed files: ' , Res , 'of' , LL ] ).


init_func :-
	set_pp_flag( fixpoint , di ),
	set_pp_flag( unfold , onestep ),
	set_pp_flag( inter_optimize, peval ).

	

checker_func( _ ).
