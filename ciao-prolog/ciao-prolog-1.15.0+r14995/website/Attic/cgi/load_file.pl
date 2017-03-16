:- module( load_file , [main/1] , [ pillow ] ).


:- use_module(library(lists)).
:- use_module(library(file_utils)).

:- use_module(.(lines_to_string)).


main( _ ) :-
	get_form_input( Input ),
	file_to_string( 'templates/ciaopp_init_template.html', Str ),
	html_template( Str, Terms, Dict ),
	get_form_value( Input , file , FileNameOrStr ),
	( 
	    form_request_method( 'GET' )
	->
	    (
		html_protect( (
				 atom_concat( 'files/' , FileNameOrStr , ReadFile ),
				 file_to_string( ReadFile , StrReaded )
			      )
			    )
	    ->
	        true
	    ;
	        StrReaded = "Unexpeted error when reading from files/" || FileNameOrStr
	    )
	;
	    lines_to_string( FileNameOrStr , StrReaded )
	),
	member( (emacs_text=StrReaded) , Dict ),
	output_html( [ cgi_reply | Terms ] ).
