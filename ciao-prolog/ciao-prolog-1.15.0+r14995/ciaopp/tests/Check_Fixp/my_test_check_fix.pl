:- module(my_test_check_fix , [main/0,analyze_file/1],[assertions]).

:- use_module(ciaopp(plai)).
:- use_module(plai(fixpo_ops)).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(driver), [module/1, analyze/1]).
:- use_module(program(p_dump)).

:- use_module(library(tester), [run_tester/9]).

:- use_module('..'(test_options), [get_test_options/2]).

:- use_module('..'(basic_test), [
	                            build_source_name/3,
				    combine/7
				  ] ).

:- use_module(library(lists), [length/2]).


main:-
	get_test_options( checkfixpo , [ModulesList , _ , 
	                                TestDir , _ , A] ),

	combine( ModulesList , TestDir , A , _ , _ , _ , ML ),

 	run_tester(
		      '../log/check_fix_test.log',
		      '../log/check_fix_summary.log',
		      init_func ,
		      analyze_file ,
		      ML,
		      checker_func,
		      ML,
		      end_func,
		      Res
		  ),

	 length( ModulesList , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Check fix analysis result: ' , Op , '%' ] ),
	 fail.

main.




init_func :-
	display( 'Starting Check Fixpoint test\n' ).

checker_func( _ ).

end_func.

analyze_file( (File,TestDir,(Fixpoint1,Fixpoint2,Fixpoint3,Fixpoint4,Fixpoint5,AbsInt),_,_,_,_) ):-
	build_source_name( TestDir , File , Source_Name ),
	set_pp_flag( fixpoint , Fixpoint1 ),
	module( Source_Name ),
	analyze(AbsInt),
	remove_useless_info(AbsInt),
	set_pp_flag(fixpoint,Fixpoint2),
	analyze(AbsInt),
 	set_pp_flag(fixpoint,Fixpoint5),
	analyze(AbsInt),
	set_pp_flag(fixpoint,Fixpoint3),
 	analyze(AbsInt),
	set_pp_flag(fixpoint,Fixpoint4),
	analyze(AbsInt).
