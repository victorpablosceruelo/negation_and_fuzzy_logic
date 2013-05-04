% Compile with:
%   ciaoc -u ../../ciao/lib/component_registry/component_registry.pl ciaopprt

:- module( ciaopprt , [main/1], [assertions] ).

:- use_module(library(component_registry)).
:- use_module(ciaopp(minipp)).
:- use_module(ciaopp(preprocess_flags)).

:- use_module(rtchecks(rtchecks_pred), [rtchecks_pred/0]).
:- use_module(auto_interface(auto_rtchecks)).

:- use_module(library(lpdist(collect_modules)), [current_dir_module/2]).


:- comment( title  , "The CiaoPP run-time command-line interface" ).
:- comment( author , "David Trallero Mena" ).
:- comment( module,  

"The run-time checks command-line interface of CiaoPP allows to
compile a project with run-time test. 

@section{Run-time checks command-line options}

This interface can be used by means of the following command-line
options:

@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}

@section{Description of the execution examples}

@begin{itemize}

@item The following command will compile only the main file of the
  project @tt{myproject.pl} with run-time checks:

@tt{ciaopprt -m myproject.pl}

@item The option @tt{-md} indicates that all files that compound the
project, exluding the system libraries, are compiled with rin-time
checks. Also, if we want to use another compiler version, we can
specify it via @tt{-compiler} option:

@tt{ciaopprt -compiler ~/bin/ciaoc-1.12 -md myproject.pl}

@item Additionally we can compile our project with run-time checks and
also link it with the run-time libraries of the system. In this case
we will use the option @tt{-a}:

@tt{ciaopprt -a myproject.pl }
@end{itemize}
").


main( Args ) :-
	process_args( Args ).



process_args( [] ) :-
	!,
	process_args( ['-h'] ).
process_args( [ X ] ) :-
	( X == '-h' ; X == '--help' ),
	!,
	usage_message( Msg ),
	display_string( Msg ).
process_args( [ '-gen_rtc_libs', Libs ] ) :-
	!,
	generate_rtc_libs( Libs ).
process_args( [ File ] ) :-
	!,
	process_args( ['-m' , File ] ).
process_args( ['-m' | File ] ) :-
	!,
	compile_files_with_rtc( File, main, generate_main ).
process_args( [ '-c' | File ] ) :-
	!,
	compile_files_with_rtc( File, obj, generate_main ).
process_args( ['-md' | File ] ) :-
	!,
	compile_files_with_rtc( File, main_and_deps, generate_main ).
process_args( ['-a' | File ] ) :-
	!,
	compile_files_with_rtc( File, all, generate_main ).
process_args( ['-compiler', C|Opts] ) :-
	!,
	set_pp_flag( compiler, C ),
	process_args( Opts ).



compile_files_with_rtc( Files , A , B ) :-
	member( F, Files ),
	compile_prj_with_rtc( F, A, B ),
	fail ; true.



generate_rtc_libs( Libs ) :-
	current_dir_module( Libs, Module ),
	\+ ( atom_concat( _, '_co.pl' , Module ) 
	   ; atom_concat( _, '_co'    , Module )
	   ; atom_concat( _, '_doc.pl', Module ) 
	   ; atom_concat( _, '_doc'   , Module )
	   ; (atom_concat( _A, B, Module ),
	      atom_concat( '/rtchecks', _   , B )) ),
	compile_prj_with_rtc( Module, obj, generate_main ),
	fail ; true.


:- push_prolog_flag(multi_arity_warnings,off).

:- multifile transformation/2.
:- multifile transformation/1.

transformation( rtc ).
transformation( rtc, _ ) :- 
	rtchecks_pred:rtchecks_pred.

:- pop_prolog_flag(multi_arity_warnings).


usage_message( "
Usage: ciaopprt -h
       ciaopprt --help
       ciaopprt [-compiler Compiler] Option fileName1 filename2...
       ciaopprt -gen_rtc_libs BaseLib

  Where:
    -h, --help     print this message

    -compiler      use Compiler as compiler executable

    -gen_rtc_libs  compile all modules of BaseLib with run-time 
                   checks

    Option must be one of the following:

    -m    only the main file of the project will be
          compiled with rtchecks.

    -c    generate rtchecks obj file of the file (it 
	  supposes file is not main file).

    -md   the main file and its dependencies will be 
          compiled with rt-checks.

    -a    all project is compiled with run-time checks
          and linked against run-time libraries.

Execution Examples:

  ciaopprt -m  myfile.pl
  ciaopprt -compiler ~/bin/ciaoc-1.12 -md myfile.pl
  ciaopprt -a  myfile.pl
  ciaopprt -h
").
