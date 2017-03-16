:- module(generate_all_spec,[main/0],[assertions]).

:- use_module(ciaopp(plai)).
:- use_module(plai(fixpo_ops)).
:- use_package(ciaopp).

:- use_module(library(lists), [length/2]).
:- use_module(library(write), [write/1]).

:- use_module('..'(test_options), [get_test_options/2]).

:- use_module(library(tester), [run_tester/9]).

:- use_module('..'(basic_test), [
	                            atom_concat_list/2,
				    build_dump_name/6,
				    build_spec_name/6,
				    build_source_name/3
				  ] ).


:- doc(module,"This module analyzes programs and compares the 
	results with previous analysis results stored").











main:- 
	get_test_options( spec , [ModulesList , _ , 
	                          Dir , Storage , (A,B,C)] ),
	combine_and_compare( ModulesList , A ,B , C ,
	                                  Dir, Storage ),
	fail.

main :- display( 'All test generated' ).


combine_and_compare( ModuleList , A , B , C , Dir , Sto ) :-
	combine( ModuleList , A , B , C , Dir , Sto , ML ),


	
	atom_concat( '../log/' , C , C1 ),
	atom_concat( C1 , '_gen_spec_test.log'    , Test    ),
	atom_concat( C1 , '_gen_spec_summary.log' , Summary ),
	
 	run_tester(
		      Test,
		      Summary,
		      init_func ,
		      spec_compare ,
		      ML,
		      checker_func,
		      ML,
		      end_func,
		      Res
		  ),


	 length( ML , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Spec generation result: ' , Op , '%' ] ).


combine( [A] , B , C , D , E , F , [(A,B,C,D,E,F)] ) :- 
	!.

combine( [A|R] , B , C , D , E , F , [(A,B,C,D,E,F)|RR] ) :-
	combine( R , B , C , D , E , F , RR ).
	

init_func.

checker_func( _ ).

spec_compare( (F, Fixpoint,AbsInt,Level,Dir,Storage) ):-
	push_pp_flag(fixpoint,Fixpoint),
	push_pp_flag(dump_ai,off),
	push_pp_flag(dump_level,plain),
	set_pp_flag(reuse_fixp_id,on),

	set_pp_flag(fixpoint,Fixpoint),
	set_pp_flag(reuse_fixp_id,on),

	build_dump_name(Storage,F,Fixpoint,AbsInt,Level,Dump_Name),
	build_spec_name(Storage,F,Fixpoint,AbsInt,Level,Spec_Name),
	build_source_name(Dir,F,Source_Name),

	analyze_file(Source_Name,Dump_Name,Spec_Name,AbsInt,Level),
	pop_pp_flag(fixpoint),
	pop_pp_flag(dump_ai),
	pop_pp_flag(dump_level).

end_func.

analyze_file(Source_Name,Dump_Name,Spec_Name,AbsInt,Level):-
	module(Source_Name),
	analyze(AbsInt),
	remove_useless_info(AbsInt),
	dump(Dump_Name),
	transform(Level),
	output(Spec_Name).

separator('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%').
