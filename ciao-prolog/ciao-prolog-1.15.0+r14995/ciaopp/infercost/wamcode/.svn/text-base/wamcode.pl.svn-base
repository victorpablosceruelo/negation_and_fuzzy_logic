:- module(wamcode,
	[
	    get_current_wam_code/1,
	    get_clause_wam_code/3,
	    get_wam_code/2
	], []).

:- use_module(library(compiler), [make_wam/1]).
:- use_module(api(api_module)).
:- use_module(library(read)).


get_current_wam_code(WamCode) :-
	get_output_name(_, Path),
	!,
	get_wam_code(Path, WamCode).

get_wam_code(Path, WamCode) :-
	make_wam(Path),
	atom_concat(Path, '.wam', PathWam),
	open(PathWam, read, S),
	read_terms(S, WamCode),
	close(S).

read_terms( Stream , Out ) :-
	read( Stream , R ),
	!,
	( 
	    R = end_of_file
	->
	    Out = []
	;
	    read_terms( Stream , Rs ),
	    Out = [ R | Rs ]
	).

get_clause_wam_code(Key, WamCode, ClauseWamCode) :-
	member(clause(Key, ClauseWamCode), WamCode),
	!.
