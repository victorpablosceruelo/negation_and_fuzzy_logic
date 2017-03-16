:- module(test_out,[main/0],[assertions]).


:- use_module(library(lists), [append/3, length/2]).

:- use_module(ciaopp(driver), [module/1, analyze/1, transform/1]).
:- use_module(ciaopp(preprocess_flags), [set_pp_flag/2]).
:- use_module(ciaopp(printer), [output/0]).

:- use_module(library(compiler), [use_module/1]).

:- use_module('..'(test_options), [get_test_options/2]).

:- use_module(library(tester), [run_tester/10]).

:- use_module('..'(basic_test), [
	                            atom_concat_list/2,
				    build_dump_name/6,
				    build_source_name/3,
				    combine/7
				  ] ).

:- doc(module,"This module analyzes programs and try to reload the
	output").



main:- 
	get_test_options( out , [ModulesList , TestDir , _ , (A,B)] ),
	combine( ModulesList , TestDir , A , B , _ , _ , ML ),

 	run_tester(
		      '../log/out_test.log',
		      '../log/out_summary.log',
		      init_func ,
		      gen_and_load_out ,
		      ML,
		      checker_func,
		      ML,
		      end_func,
		      Res,
		      slider( 'Out Test: ' )
		  ),

	 length( ModulesList , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Out Analysis Result: ' , Op , '% (' , 
	                    Res , ' of ' , LL , ')' ] ),
	 fail.
main :- display( 'Out tests done' ), nl.
	

init_func :-
	set_pp_flag(pp_info,on),
	display( 'Starting OUT test\n' ).

checker_func( _ ).

end_func.


gen_and_load_out( (File,TestDir,A,B,_,_,_) ) :- 
	build_source_name( TestDir , File , Source_Name ),
	galo( Source_Name , A , B ).

galo( File , AbsInt , Trans ):-
	module(File),
	analyze(AbsInt),
	( Trans == none -> true
	; transform(Trans)
	),
	output,
	use_module(File).
