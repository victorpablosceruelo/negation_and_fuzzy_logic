:- module( generate_js_menu , [main/1] , [] ).


:- use_module(ciaopp(menu_generator)).
:- use_module(auto_interface(auto_interface)).
:- use_module(library(file_utils)).


main( File ) :-
	file_to_string( 'ciaopp_menu_body.js' , String ),
	open( File, write, Stream ),
        current_output(O),
	set_output( Stream ),
	display( 'var menus = new Array( ) ;\n' ),
	( generate_js_menu( [menu_config_name, menu_last_config ] ) 
	  -> true ; true
	),
	display_string( String ),
	close(Stream),
	set_output(O).
