:- module(my_test_out,[main/0],[assertions]).


:- use_module(library(lists), [append/3, length/2]).

:- use_module(ciaopp(driver), [module/1, analyze/1, transform/1]).
:- use_module(ciaopp(preprocess_flags), [set_pp_flag/2]).
:- use_module(ciaopp(printer), [output/0]).

:- use_module(library(compiler), [use_module/1]).

:- use_module('..'(my_benchmarks), [benchmarks_of/2]).

:- use_module(library(tester), [run_tester/9]).

:- use_module('..'(basic_test), [
	                            atom_concat_list/2,
				    build_dump_name/6,
				    build_source_name/3
				  ] ).

:- doc(module,"This module analyzes programs and try to reload the
	output").

benchmark_dir( '/home/clip/Benchmarks/ciaopp/modes/' ).


main:- 
	benchmarks_of( modes  , ModulesList ),
 	run_tester(
		      '../log/out_test.log',
		      '../log/out_summary.log',
		      init_func ,
		      gen_and_load_out ,
		      ModulesList,
		      checker_func,
		      ModulesList,
		      end_func,
		      Res
		  ),

	 length( ModulesList , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '% (' , 
	                    Res , ' of ' , LL , ')' ] ).
	

init_func :-
	set_pp_flag(pp_info,on),
	display( 'Starting OUT test\n' ).

checker_func( _ ).

end_func.


gen_and_load_out( File ) :- 
	benchmark_dir( Dir   ),
	build_source_name( Dir , File , Source_Name ),
	galo( (Source_Name , (shfr,none)) ).

galo( (File , (AbsInt,Trans) ) ):-
	module(File),
	analyze(AbsInt),
	( Trans == none -> true
	; transform(Trans)
	),
	output,
	use_module(File).
