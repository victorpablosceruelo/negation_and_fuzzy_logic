:- module(ciaopp_daemon, [call_ciaopp/4], [api(api_menu)]).

:- use_module(library(file_utils)).
:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(library(io_alias_redirection)).
:- use_module(library(system)).

:- use_module(ciaopp(ciaopp)).
:- use_module(ciaopp(menu_generator)).

:- use_module(ciaopp(menu_generator)).
:- use_module(auto_interface(auto_interface)).

:- use_module(library(system_extra)).



create_stream_from_file( File , Stream ) :-
	open( File , write , Stream ).


create_tmp_file( AtomTmpFile ) :-
	mktemp_in_tmp( 'ciaoppwebfileXXXXXX' , AtomTmpFile ).


display_lines( [] ).
display_lines( [S|Ss] ) :-
	display_string( S ),
	nl,
	display_lines( Ss ).


create_module( AtomTmpFile , StringLines ) :-
	open_output( AtomTmpFile , IO ),
	display_lines( StringLines ),
	close_output( IO ).


call_ciaopp( Option , String , Output , Error ) :-
	intercept( 
		     call_ciaopp__( Option , String , Output , Error ),
		     X,
		     (display( uncaught_exception(X) ),nl)
		 ).


call_ciaopp__( Option , String , Output , Error ) :-
	% Lets create all temporary files
	create_tmp_file( AtomTmpFile ),
	atom_concat( AtomTmpFile , '_co.pl'  , AtomOutputFile ),
	atom_concat( AtomTmpFile , '_error'  , AtomErrorFile  ),
	create_stream_from_file( AtomErrorFile  , ErrorStream  ),

	% Create the module
	create_module( AtomTmpFile , String ),
	
	% Redirect the output and error streams
	current_output( OldOutputStream ),
	set_output( ErrorStream ),
	set_stream( user_error  , ErrorStream  , OldErrorStream  ),
	
	% Analize the module
	execute_option( Option , AtomTmpFile ),
	
	% Restore the output and error streams
 	set_stream( user_error  , OldErrorStream  , _ ),
	set_output( OldOutputStream ),
 	close( ErrorStream  ),
	
	% Read output and error
        safe_file_to_string( AtomErrorFile  , Error  ),
 	safe_file_to_string( AtomOutputFile , Output ),

	-( delete_file( AtomTmpFile    ) ),
	-( delete_file( AtomOutputFile ) ),
	-( delete_file( AtomErrorFile  ) ),
	!.


safe_file_to_string( AtomFile , Str ) :-
 	( 
	    file_to_string( AtomFile  , Str  ) 
	->
            true 
	;
	    atom_concat( "Could not read " , AtomFile , EE ),
	    atom_codes( EE , Str )
	).



execute_option( analyze , AtomTmpFile ) :-
	auto_analyze( AtomTmpFile ),
	!.

execute_option( check_assert , AtomTmpFile ) :-
	auto_check_assert( AtomTmpFile ),
	!.

execute_option( optimize , AtomTmpFile ) :-
	auto_optimize( AtomTmpFile ),
	!.

execute_option( flags(L) , AtomTmpFile ) :-
	put_flag_values( L ),
	set_last_file( AtomTmpFile ),
	again,
	restore_flags( L ),
	!.

execute_option( _ , _ ).
	
	


back_translation( analyze          , ana   ).
back_translation( check_assertions , check ).
back_translation( optimize         , spec  ).


put_flag_values( M ) :-
	member( (inter_all,Menu) , M ),
	back_translation( Menu , RM ),
	!,
	put_flag_values__( M , RM ).
put_flag_values( M ) :-
	display( no_inter(M) ),nl,
	put_flag_values__( M , ana ).


	

put_flag_values__( [] , _ ).
put_flag_values__( [ (F,FV) | Fs ] , M ) :-
	set_all_possible_flags( F , FV , M ),
	put_flag_values__( Fs , M ).




set_all_possible_flags( F , FV , M ) :-
	menu_opt${ menu => M , flag => F },
	!,
	(FV \== '',set_menu_flag( M , F , FV ) -> true ; true ).
set_all_possible_flags( F , FV , _ ) :-
	menu_opt${ menu => M , flag => F },
	(FV \== '',set_menu_flag( M , F , FV ) -> true ; true ),
	fail.
set_all_possible_flags( _ , _ , _ ).




restore_flags( _ ).
% restore_flags( [] ).
% restore_flags( [ (F,_) | Fs ] ) :-
% 	(pop_pp_flag( F )->true;true),
% 	restore_flags( Fs ).
