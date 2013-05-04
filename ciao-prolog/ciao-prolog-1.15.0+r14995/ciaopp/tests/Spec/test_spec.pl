:- module(test_spec,[main/0],[assertions]).

:- use_module(ciaopp(plai)).
:- use_module(plai(fixpo_ops)).
:- use_module(ciaopp(preprocess_flags)).
:- use_module(ciaopp(driver), [module/1, analyze/1, transform/1]).
:- use_module(ciaopp(printer), [output/0]).
:- use_module(program(p_dump)).
:- use_module(program(p_unit), [program/2]).
:- use_module(program(unexpand), [transform_clause_list/3]).
:- use_module(program(clidlist), [inverse_rewrite_source_program/2]).

:- use_module(library(lists), [length/2]).
:- use_module(library(write), [write/1]).

:- use_module('..'(test_options), [get_test_options/2]).

:- use_module(library(tester), [run_tester/10]).

:- use_module('..'(basic_test), [
	                            atom_concat_list/2,
				    build_dump_name/6,
				    build_spec_name/6,
				    build_source_name/3,
				    combine/7
				  ] ).


:- doc(module,"This module analyzes programs and compares the 
	results with previous analysis results stored").


main:- 
	get_test_options( spec , [ModulesList ,
	                          TestDir , GoodDir , (A,B,C)] ),
	combine_and_compare( ModulesList , A ,B , C ,
	                                  TestDir, GoodDir ),
	fail.

main :- display( 'Spec tests done' ), nl.



combine_and_compare( ModuleList , A , B , C , Sto , Dir ) :-
	combine( ModuleList , A , B , C , Sto , Dir , ML ),
	
	atom_concat( '../log/' , C , C1 ),
	atom_concat( C1 , '_spec_test.log'    , Test    ),
	atom_concat( C1 , '_spec_summary.log' , Summary ),

	print_separator( A , B , C ),
	
 	run_tester(
		      Test,
		      Summary,
		      init_func ,
		      spec_compare ,
		      ML,
		      checker_func,
		      ML,
		      end_func,
		      Res,
		      slider( 'Spec Test: ' )
		  ),

	 length( ML , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Spec analysis result: ' , Op , '%' ] ).


print_separator( A , B , C ) :-
	separator(Sep),
	write(Sep), nl,
	write('Testing specializer with '),
	write(A), %Fixpoint
	write(' '),
	write(B), %AbsInt
	write(' '),
	write(C), %Level
	nl, write(Sep), nl.

init_func.


checker_func( _ ).

spec_compare( (F, Fixpoint,AbsInt,Level, TestDir, GoodDir) ):-
	set_pp_flag(fixpoint,Fixpoint),
	set_pp_flag(reuse_fixp_id,on),
	push_prolog_flag(quiet,on),

	build_dump_name( GoodDir ,F,Fixpoint,AbsInt,Level,Dump_Name),
	build_spec_name( GoodDir ,F,Fixpoint,AbsInt,Level,Spec_Name),
	build_source_name( TestDir ,F,Source_Name),

	atom_concat_list( [ F , '_' , Fixpoint , '_' , AbsInt , '_' , Level ],
	                  F_Stored ),
	analyze_file(Source_Name,Dump_Name,Spec_Name,AbsInt,Level,F,F_Stored),
	pop_prolog_flag(quiet).

end_func :-
	separator(Sep),
	write(Sep), 
	nl.


analyze_file(Source_Name,Dump_Name,Spec_Name,AbsInt,Level,F,F_Stored):-
	nl,write(F),nl,
	module(Spec_Name),
	program(Old_Cls,_Old_Ds),
	restore(Dump_Name),
	store_previous_analysis(shfr),
	write('.'),
	module(Source_Name),
	analyze(AbsInt),
	remove_useless_info(AbsInt),
	write('.'),
	transform(Level),
	output,
	program(Cls,_Ds),
	write('.'),
	compare_programs(Cls,Old_Cls,F,F_Stored,Flag),
	write(Flag),
	nl.

compare_programs(Cls,Old_Cls,Module,Module_Stored,Flag):-
	inverse_rewrite_source_program(Cls,Cls0),
	inverse_rewrite_source_program(Old_Cls,Old_Cls0),
	transform_clause_list(Cls0,Module,NCls),
	transform_clause_list(Old_Cls0,Module_Stored,NCls),!,
	Flag = 'OK'.
compare_programs(_,_,_,_,Flag):-
 	error('different programs'),
	Flag = 'failed'.

separator('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%').
