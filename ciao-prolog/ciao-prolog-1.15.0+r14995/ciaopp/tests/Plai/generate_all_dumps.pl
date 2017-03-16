:- module(generate_all_dumps,[main/0],[assertions]).

:- use_module('..'(test_options), [
	                             get_test_options/2
				   ] ).

:- use_module('..'(basic_test), [
				     build_dump_name/6,
				     build_source_name/3,
				     combine/7
				   ] ). 

:- use_module(library(tester), [run_tester/10]).

:- use_module(ciaopp(plai)).
:- use_module(plai(fixpo_ops)).
:- use_module(ciaopp(ciaopp)).
:- use_module(library(lists), [length/2]).

:- doc(module,"This module analyzes programs and store the analysis 
	results as dumps. This will allow comparing these results with
        other analyses later on.").

main:- 
	get_test_options( plai , [ModulesList ,
	                          TestDir , GoodDir , (A,B,C)] ),
				  
	combine( ModulesList , A , B , C , TestDir , GoodDir , ML ),

 	run_tester(
		      '../log/play_gen_test.log',
		      '../log/play_gen_summary.log',
		      init_func ,
		      generate ,
		      ML,
		      checker_func,
		      ML,
		      end_func,
		      Res,
		      slider( 'Plai modes Test: ' )
		  ),

	 length( ModulesList , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Generation result: ' , Op , '%' ] ),
	 fail.
main.
	



init_func :-
	display( 'Generating PLAI test data\n' ).

checker_func( _ ).

end_func.


generate( (File , Fixpoint,AbsInt,Dump_Level,TestDir,GoodDir) ):-
%	push_pp_flag(dump_level,Dump_Level),
	set_pp_flag(fixpoint,Fixpoint),

	build_dump_name(   GoodDir    , File       ,
	                   Fixpoint   , AbsInt     ,
			   Dump_Level , Dump_Name ),
			   
	build_source_name( TestDir    , File       , Source_Name ),

	analyze_file( Source_Name , Dump_Name , AbsInt ),

%	pop_pp_flag(dump_level).
	true.


analyze_file(Absolute_File_Name,Output_File,AbsInt):-
	module(Absolute_File_Name),
	analyze(AbsInt),
	remove_useless_info(AbsInt),
	dump(Output_File).
