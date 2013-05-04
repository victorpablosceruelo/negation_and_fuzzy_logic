:- module(test_plai_types,[main/0,gen_dump_in_ascii/0],[assertions]).

:- use_module(library(lists), [length/2]).
:- use_module(ciaopp(plai)).
:- use_module(plai(fixpo_ops)).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(driver), [module/1, module/2, analyze/1]).
:- use_module(ciaopp(printer), [output/1, output/0]).
:- use_module(program(p_dump)).

:- use_module('..'(test_options), [get_test_options/2]).

:- use_module(library(tester), [run_tester/10]).

:- use_module('..'(basic_test), [
	                            atom_concat_list/2,
				    build_dump_name/6,
				    build_source_name/3,
				    combine/7
				  ] ).

:- doc(module,"This module analyzes programs and compares the 
	results with previous analysis results stored").


main:-
	get_test_options(plai_types , [ModulesList, TestDir, GoodDir, (A,B,C)]),
	combine( ModulesList , A , B , C , TestDir , GoodDir , ML ),
	set_pp_flag( pp_info , off ),
 	run_tester(
		      '../log/plai_types_test.log',
		      '../log/plai_types_summary.log',
		      init_func ,
		      compare ,
		      ML,
		      checker_func,
		      ML,
		      end_func,
		      Res,
		      slider( 'Plai Types Test: ' )
		  ),

	 length( ModulesList , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '%' ] ),
	 fail.
main :- display( 'Plai tests done' ), nl.


init_func :-
	display( 'Starting PLAI test\n' ).

checker_func( _ ).

end_func.


compare( (File , Fixpoint,AbsInt,Dump_Level,TestDir,GoodDir) ):-
	set_pp_flag( fixpoint , Fixpoint ),
	set_pp_flag( reuse_fixp_id , on ),

	build_dump_name(   GoodDir    , File   ,
	                   Fixpoint   , AbsInt ,
			   Dump_Level , Dump_Name ),
	build_source_name( TestDir , File , Source_Name ),

	display( 'Analizing ' ),
	display( Source_Name ), display( ' and comparing with ' ),
	display( Dump_Name ),nl,


	analyze_file( Source_Name , Dump_Name , AbsInt ).



analyze_file( Source_Name , Dump_Name , AbsInt ) :-
	module( Source_Name , LE ),
	!,
	\+ member( error , LE ),
%jcf- module/1 must be called *before* restore, since it cleans up typedefs.
	module( Source_Name ),
	restore( Dump_Name  ),
	store_previous_analysis( AbsInt ),
	analyze( AbsInt ),
	remove_useless_info( AbsInt ),
	compare_completes_with_prev(AbsInt, E, '='), 
	E \== error.



gen_dump_in_ascii :-
	get_test_options( plai , [ModulesList ,
	                          TestDir , GoodDir , _ ] ),
	member( File , ModulesList ),
	gen_one_dump_in_ascii( File , TestDir , GoodDir ),
	fail.

gen_dump_in_ascii.



gen_one_dump_in_ascii( File , TestDir , GoodDir ) :-
	build_dump_name(   GoodDir    , File   ,
	                   plai   , 
			   eterms,
			   dep , Dump_Name ),

	build_source_name( TestDir , File , Source_Name ),

	display( 'Transforming: ' ),
	display( Source_Name ),
	display( ' with ' ),
	display( Dump_Name ),nl,
	
	module( Source_Name ),
	restore( Dump_Name ),
	atom_concat( Source_Name , '_dump.pl' , SDName ),

	display( 'Writting output: ' ),
	display( SDName ), nl,

	output( SDName ).
