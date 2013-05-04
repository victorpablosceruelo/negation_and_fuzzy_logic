:- module(test_check_fixp,[main/0,analyze_file/7],[assertions]).

:- use_module(ciaopp(plai)).
:- use_module(plai(fixpo_ops)).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(driver), [module/1, analyze/1]).
:- use_module(program(p_dump)).
:- use_module(library(lists), [length/2]).


:- use_module('..'(test_options), [get_test_options/2]).

:- use_module(library(tester), [run_tester/10]).

:- use_module('..'(basic_test), [
	                            atom_concat_list/2,
				    build_dump_name/6,
				    build_source_name/3,
				    combine/7
				  ] ).


main:-
	get_test_options( checkfixpo , [ModulesList ,
	                          TestDir , GoodDir , 
				  C] ),
				  
	combine( ModulesList , C , no, no, TestDir , GoodDir , ML ),
			  
 	run_tester(
		      '../log/checkfp_test.log',
		      '../log/checkfp_summary.log',
		      init_func ,
		      compare ,
		      ML,
		      checker_func,
		      ML,
		      end_func,
		      Res,
		      slider( 'Check FixPo Test: ' )
		  ),

	 length( ModulesList , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '% (' , 
	                    Res , ' of ',LL , ')' ] ),
	 fail.

main :- 
	display( 'Plai tests done' ), nl.



init_func :-
	display( 'Starting Check Fix Point test\n' ).



checker_func( _ ).



end_func.



compare( (File ,
	(Fixpoint1,Fixpoint2,Fixpoint3,Fixpoint4,Fixpoint5,AbsInt),
	no,no,TestDir, _GoodDir) ):-

	build_source_name( TestDir , File , Source_Name ),

	display( 'Analizing ' ), display( Source_Name ), nl,

	analyze_file( Source_Name ,
	              Fixpoint1,Fixpoint2,Fixpoint3,Fixpoint4,Fixpoint5,AbsInt).

analyze_file(F,Fixpoint1,Fixpoint2,Fixpoint3,Fixpoint4,Fixpoint5,AbsInt):-
	reset_previous_analysis(AbsInt),
	set_pp_flag(fixpoint,Fixpoint1),
	module(F),
	analyze(AbsInt),
	remove_useless_info(AbsInt),
 	set_pp_flag(fixpoint,Fixpoint5),
	analyze(AbsInt),
	set_pp_flag(fixpoint,Fixpoint4),
	analyze(AbsInt),
	set_pp_flag(fixpoint,Fixpoint3),
 	analyze(AbsInt),
	set_pp_flag(fixpoint,Fixpoint2),
	analyze(AbsInt).
