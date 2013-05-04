:- module( test_ctchecks, [ main/0 ] , [ assertions ] ).


:- use_module(library(lists), [length/2]).
:- use_module(plai(fixpo_ops)).
:- use_module(ciaopp(plai)).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(driver), [module/1, module/2, analyze/1]).
:- use_module(ciaopp(printer), [output/1, output/0]).

:- use_module('..'(test_options), [get_test_options/2]).

:- use_module(library(tester), [run_tester/10]).

:- use_module('..'(basic_test), [
	                              atom_concat_list/2,
				      build_dump_name/6,
				      build_source_name/3,
				      combine/7
				    ] ).

:- doc(module,"This module analyzes programs and then realize a
   ctchecks. It compare the output loading a new module with the good
   results.").


main:-
	get_test_options( ctchecks , [ModulesList ,
	                              TestDir     ,
				      GoodDir , (A,B,C)] ),
				  
	combine( ModulesList , A , B , C , TestDir , GoodDir , ML ),
			  
 	run_tester(
		      '../log/ctchecks_test.log',
		      '../log/ctchecks_summary.log',
		      init_func ,
		      compare ,
		      ML,
		      checker_func,
		      ML,
		      end_func,
		      Res,
		      slider( 'CtChecks Test: ' )
		  ),

	 length( ModulesList , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '% (' ,
	                    Res , ' of ' , LL , ')' ] ),
	 fail.
main :- display( 'CtChecks tests done\n' ).


init_func :-
	display( 'Starting CtChecks test\n' ).

checker_func( _ ).

end_func.


compare( (File , Fixpoint,AbsInt,Dump_Level,TestDir,GoodDir) ):-
	set_pp_flag( fixpoint , Fixpoint ),

	build_source_name( TestDir , File , Source_Name ),
	build_source_name( GoodDir , File , Dump_Name   ),

	display( 'Analizing ' ),
	display( Source_Name ), display( ' and comparing with ' ),
	display( Dump_Name ),nl.


%	analyze_file( Source_Name , Dump_Name , AbsInt ).



% analyze_file( Source_Name , Dump_Name , AbsInt ) :-
% 	module( Source_Name , LE ),
% 	!,
% 	\+ member( error , LE ),
% 	restore( Dump_Name  ),
% 	store_previous_analysis( shfr ),
% 	module( Source_Name ),
% 	analyze( AbsInt ),
% 	output,
% 	remove_useless_info( AbsInt ),
% 	compare_completes_with_prev( AbsInt , E ), 
% 	E \== error.
