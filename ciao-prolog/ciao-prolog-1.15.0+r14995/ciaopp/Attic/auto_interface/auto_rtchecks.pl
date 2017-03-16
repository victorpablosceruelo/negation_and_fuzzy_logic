:- module(auto_rtchecks,
	[compile_prj_with_rtc/3],
	[assertions, regtypes, condcomp, ciaopp_options]).

:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2
                                         ,  pop_pp_flag/1
					 ,  push_pp_flag/2
					 ] ).
:- use_module(library(system), [delete_file/1
                                     , system/1      ] ).
:- use_module(program(p_unit), [get_dependent_files_of/3
			             , get_dependent_files_set/2
			             ] ).

:- use_module(library(lists), [append/3]).
:- use_module(library(messages)).

:- use_module(library(format)).

:- use_module(ciaopp(api(api_base)), [file_up_to_date/2
                                          , clear_modified_flag/0
					  , is_modified/0 ] ).


:- use_module(ciaopp(api(api_direc_assrt)), [add_directive/1]).

:- use_module(library(terms), [atom_concat/2]).

:- if(defined(mini_pp)).
:- use_module(ciaopp(mini_driver), [module/1, transform/1]).
:- use_module(ciaopp(mini_printer), [output/0]).
:- else.
:- use_module(ciaopp(driver), [module/1, transform/1]).
:- use_module(ciaopp(printer), [output/0]).
:- endif.

:- pred compile_prj_with_rtc( MainMod, Mode, GenMain )
	: (atm( MainMod ), crtc_mode(Mode), crtc_genmain(Main))

# "Compile the project which mainfile is @var{MainMod} with run-time
  checks. @var{Mode} decides which modules will have rt-test: only the
  main module (main), the main module and its dependencies
  (main_and_deps) or the main module, its dependencies and system
  libraries (all). The option @tt{obj} will treat @var{MainMod} as a
  single module and it will be only transformed and compiled as a
  module (generating only the object files). If @var{GenMain} is
  generate_main then main module will be transformed if needed. This
  option has to do with the integration with the whole system, because
  when a file is treated in CiaoPP it can have several transformation
  before and after the run-time checks one.".


:- regtype crtc_mode/1 # "Run-Time Checks Mode".

crtc_mode( obj ).
crtc_mode( main ).
crtc_mode( mand_and_deps ).
crtc_mode( all ).

:- regtype crtc_genmain/1 # "main generation enum type".

crtc_genmain( generate_main ).
crtc_genmain( do_not_generate_main ).


compile_prj_with_rtc( MainMod0, Mode, GenMain ) :-
    absolute_file_name( MainMod0, MainMod ),
    ( Mode == obj ->
         generate_files_with_rtc( [MainMod], include_libs, Modif ),
	 Libs = '-c'
    ; 
	get_dependent_files_of( MainMod , Dep , [] ),
	get_dependent_files_set( Dep , Bases ),
	( Mode == main ->
	     ( GenMain == generate_main -> 
	       generate_files_with_rtc( [MainMod], no_include_libs, _Modif ) 
	     ; true ),
	     % deleting deps files is enought to compile
	     Modif = true,
	     delete_rtc_files( Bases ),
	     Libs = ''
	;
	     generate_files_with_rtc( Bases, no_include_libs, Modif1 ),
	     (GenMain == generate_main
	     ->  generate_files_with_rtc( [MainMod], no_include_libs, Modif2 ) 
	     ;   true ),

	     ((Modif1 == true ; Modif2 == true) -> Modif = true ; Modif = false ),

	     ( Mode == all
	     ->  get_rtc_libs( Libs )
	     ;   Libs = '' )
	)
    ),
    ( Modif == true 
    ->  compile_prj( MainMod, Libs )
    ;   true ).



:- pred generate_files_with_rtc( Bases, Include_Libs, Modified ) 
	: list( Bases, atm )

# "Performs run-time checks transformation in all the modules of
  @var{Bases} list. If @var{Include_Libs} is include_libs, then files
  from libraries can be transformed. @var{Modified} is true is a file
  has to be transformated.".

:- data file_modified/0.

generate_files_with_rtc( Bases, Include_Libs, Modified ) :-
   ( push_pp_flag( use_new_rdisj , on ), 
     push_pp_flag( output_rebuild_djcd , on )
   ;
     pop_pp_flag( use_new_rdisj       ), 
     pop_pp_flag( output_rebuild_djcd ),
     fail
   ),
   retractall_fact( file_modified ),
   member( BaseA_0, Bases ),
   ( atom_concat( BaseA, '.pl', BaseA_0 )
   ->  true
   ;	BaseA = BaseA_0 ),

   % ugly trick for not compiling libs!
   atom_codes( BaseA , AC ),
   ( Include_Libs == include_libs ->
	true
   ;
	( ( append( _ , "/lib/"    ||_ , AC )
	  ; append( _ , "/library/"||_ , AC )
	  ; append( _ , "/contrib/"||_ , AC ))
	->  fail 
	;   true)
   ),
   atom_concat( BaseA , '.pl' , A ),
   atom_concat( BaseA , '_rtc_co.po' , RtcPo ),
   %% verbose
   %% note_message( "comparing ~w with ~w" , [RtcPo , A] ),
   \+ file_up_to_date(RtcPo , A),
   module( BaseA ),
   clear_modified_flag,
   transform( rtc ),
   ( is_modified ->
        add_directive( use_module( library( 'rtchecks/rtchecks_printer' ) ) ),
        output,
        (current_fact( file_modified ) -> true ; asserta_fact( file_modified ))
   ; 
	note_message( "The file does not contain any run-time assertion" )
   ),
   fail ; ( current_fact( file_modified ) -> Modified = true ; Modified = false ).



:- multifile library_directory/1.
:- dynamic   library_directory/1.


:- pred get_rtc_libs( RtL ) => atm( RtL )

# "Returns the run-time library path.".

get_rtc_libs( RtL ) :-
	library_directory( L ),
	atom_concat( Base, '/library', L ),
	atom_concat( Base, '/library_rt', RtL ).


:- pred compile_prj( MainMod, Libs ) : (atm( MainMod ), atm( Libs ))

# "Pints the command to compile the project with rt-checks.".

compile_prj( MainMod , '' ) :-
	!,
	current_pp_flag( compiler, CiaoC ),
	atom_concat( [CiaoC,' -rc ', MainMod], Str ),
	do_compile_prj( Str ).
compile_prj( MainMod , '-c' ) :-
	!,
	current_pp_flag( compiler, CiaoC ),
	atom_concat( [CiaoC,' -rc -c ', MainMod], Str ),
	do_compile_prj( Str ).
compile_prj( MainMod , Libs ) :-
	current_pp_flag( compiler, CiaoC ),
	atom_concat( [CiaoC,' -rc -L ', Libs, ' ', MainMod], Command ),
	do_compile_prj( Command ).


do_compile_prj( Command ) :-
	note_message( "Compiling <~w>" , [Command] ),
	system( Command ).


:- pred delete_rtc_files( Bases ) : list( Bases, atm )

# "Delete all generated files by the run-time transformation from the
  files of @var{Bases}.".

delete_rtc_files( Bases ) :-
	member( B , Bases ),
	member( Suffix, ['_rtc_co.pl', '_rtc_co.po', '_rtc_co.itf'] ),
	atom_concat( B , Suffix, B1 ),
	catch(delete_file( B1 ),_Error,true),
	fail ; true.
