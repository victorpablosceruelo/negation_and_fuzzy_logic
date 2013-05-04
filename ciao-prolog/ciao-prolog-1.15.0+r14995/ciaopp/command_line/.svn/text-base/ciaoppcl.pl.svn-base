:- module(ciaoppcl, [main/1], [ciaopaths, assertions]).

% ciaoc -u ../paths ciaoppcl

:- use_module(library(terms), [atom_concat/2]).

%:- include(ciaoppsrc('path_init_auto_src.pl')).
%:- include('~/.ciaorc').

:- use_module(ciaopp(ciaopp)).

:- use_module(library(menu(menu_generator))).

:- use_module(library(toplevel)).

:- use_module(library(format)).

:- doc(title,"The CiaoPP command-line interface").
:- doc(author, "The CLIP Group").
:- doc( module,  

"The command-line interface of CiaoPP allows the use of the system in
batch mode, using command-line arguments for setting preprocessor
flags and performing actions.  

@section{Command-line options}

This interface can be used by means of the following command-line
options:


@begin{verbatim}
@includefact{usage_message/1}
@end{verbatim}

@section{Description of the execution examples}

@begin{itemize}

@item The following command will prompt the user with the options needed to
preprocess @tt{myfile.pl}:

@tt{ciaoppcl -Q myfile.pl}

@item If we want to verify the assertions of @tt{myfile.pl}, and generate
the resulting source code that will the new status of the assertions
(either @tt{checked}, if CiaoPP has proved that the assertion holds,
or @tt{false} if it has falsified the assertion), the command line is
as follows:

@tt{ciaoppcl -o myfile_checked.pl -V myfile.pl}

@item To optimize @tt{myfile.pl}, and write the optimize code in a file
named automatically (e.g., @tt{myfile_pd_codegen_af_co.pl}), the
following command line must be used:

@tt{ciaoppcl -O myfile.pl}

@item If the default flag values need to be changed, the @tt{-f} option can
be used. For example, in order to analyze @tt{myfile.pl} to change the
types analysis domain to @tt{terms} instead of the default one, and
the mode-aliasing domain to @tt{pd}, the command line to use should
be:

@tt{ciaoppcl -A myfile.pl -ftypes=terms -f modes=pd}

@item Finally, the following command line can be used to start a top-level
CiaoPP shell:

@tt{ciaoppcl -T}

@end{itemize}
").


:- data output_file/1.


get_output_filename( X ) :-
	output_file( X ),
	!.
get_output_filename( _ ).


main( Args ) :-
%	display( args( Args ) ), nl,
	process_args( Args ).


process_args( ['-T'] ) :-
	set_prolog_flag( quiet , warning ),
	toplevel:toplevel( ['-p', 'ciaopp ?- ', 
	                    '-l', '~/.ciaorc', 
%			    '-u', library(ciaopp) ,
			    '-u', program( p_asr ),
			    '-u', typeslib(typeslib),
			    '-u', ciaopp(driver),
			    '-u', ciaopp(printer),
			    '-u', auto_interface( auto_interface ),
			    '-u', auto_interface( auto_help ),
			    '-g', set_prolog_flag(quiet, off)
                            ] ).

process_args( ['-Q' , File ] ) :-
	!,
	customize_and_preprocess( File ).

process_args( ['-A' , File | Opts ] ) :-
	!,
	process_args_opt( Opts , ana   , File ).

process_args( ['-V' , File | Opts ] ) :-
	!,
	process_args_opt( Opts , check , File ).

process_args( ['-O' , File | Opts ] ) :-
	!,
	process_args_opt( Opts , opt   , File ).

process_args( ['-o' , File | More ] ) :-
	!,
	retractall_fact( output_file( _ ) ),
	asserta_fact( output_file( File ) ),
	process_args( More ).

process_args( ['-U' , Menu , File ] ) :-
	!,
	display( 'Restoring Menu Configuration ' ),
	display( Menu ), nl,
	restore_menu_config( Menu ),
	set_last_file( File ),
	again.

process_args( _ ) :-
	usage_message(Text),
	format(user_error,Text,[]).

usage_message(
"Usage 1: (batch mode)
	ciaoppcl [-o OutFile] Option Filename [FlagsValues]

  Where:
    -o OutFile  after processing Filename, the resulting source 
                code is written to OutFile.  If this option is
                omitted, the output is written to a file
                automatically named depending on the actions
                performed.

    Option must be one of the following:
    -Q          runs the interactive (text-based) menu for
                preprocessing Filename.
    -A          analyzes Filename with the default options
                except the flag values set with -f at the 
                command line.
    -O          optimizes Filename with the default options
                except the flag values set with -f at the 
                command line.
    -V          verifies the assertions of Filename with
                the default options except the flag values set 
                with -f at the command line.
    -U Config   processes Filename with the
                options set in the CiaoPP configuration Config.

    FlagsValues is a list of options -fFlagName=FlagValue
    separated by blank spaces, where FlagName is a valid
    CiaoPP flag name.  This list is optional, and does not need
    to include all flags applicable to the action to be performed:
    the flags not included in this list will be assumed to take
    their default value.  Examples:

    -flocal_control=on   where local_control is expected to be
                         a CiaoPP flag;
    -f local_control=on  same as above, with additional blank spaces

    Internal flags can also be changed using -pIntFlagName=Value.

Usage 2: (top-level mode)
       ciaoppcl -T 

  -T option starts a CiaoPP top-level shell.  Any of the predicates
  described in the Section CiaoPP User Menu Interface of the CiaoPP
  Reference Manual can be used in this top-level.

Execution Examples:

  ciaoppcl -Q myfile.pl
  ciaoppcl -o myfile_checked.pl -V myfile.pl
  ciaoppcl -O myfile.pl
  ciaoppcl -A myfile.pl -ftypes=terms -f modes=pd
  ciaoppcl -T
").

process_args_opt( [] , ana          , File ) :-
	get_output_filename( OFile ),
	auto_analyze( File , OFile ).
process_args_opt( [] , check        , File ) :-
	get_output_filename( OFile ),
	auto_check_assert( File , OFile ).
process_args_opt( [] , opt          , File ) :-
	get_output_filename( OFile ),
	auto_optimize( File , OFile ).
process_args_opt( ['-o', OFile|More] ,  A   , File ) :-
	retractall_fact( output_file( _ ) ),
	asserta_fact( output_file( OFile ) ),
	process_args_opt( More, A, File ).
process_args_opt( ['-f', FV|Opts] ,  A   , File ) :-
	is_flag_value( FV , F , V ),
	set_menu_flag_option( A , F , V ),
	!,
	process_args_opt( Opts , A , File ).
process_args_opt( [FV|Opts] ,  A   , File ) :-
	is_flag_value_f( FV , F , V ),
	set_menu_flag_option( A , F , V ),
	!,
	process_args_opt( Opts , A , File ).
process_args_opt( ['-p', FV|Opts] ,  A   , File ) :-
	is_flag_value( FV , F , V ),
	set_pp_flag( F , V ),
	!,
	process_args_opt( Opts , A , File ).
process_args_opt( [PV|Opts] ,  A   , File ) :-
	is_flag_value_p( PV , P , V ),
	set_pp_flag( P , V ),
	!,
	process_args_opt( Opts , A , File ).
process_args_opt( [F|Opts] ,  A   , File ) :-
	display( 'Unrecognized option ' ),
	displayq( F ),
	nl,
	process_args_opt( Opts , A , File ).


set_menu_flag_option( opt ,  inter_optimize , V ) :-
	!,
	set_menu_flag_option( opt ,  inter_optimize , V ).
set_menu_flag_option( opt ,  F , V ) :-
	!,
	get_menu_flag( opt , inter_optimize , P ),
	set_menu_flag_option( P ,  F , V ).
set_menu_flag_option( A , F , V ) :-
	set_menu_flag( A , F , V ).


is_flag_value( FV , F , V ) :-
	atom_concat( [ F , '=' , V ] , FV ).

is_flag_value_f( FV , F , V ) :-
	atom_concat( [ '-f' , F , '=' , V ] , FV ).

is_flag_value_p( PV , P , V ) :-
	atom_concat( [ '-p' , P , '=' , V ] , PV ).
