:- module(filenames_pcpe,
	[ 
	    get_src_name/1, 
	    get_spec_name/3, 
	    create_driver_file/2, 
	    get_output_file_name/2,
	    get_best_solution_file_name/2,
	    get_tmp_file_name/1
	],[assertions, isomodes]).


:- doc(title,"PCPE Filenames").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module generates the different filenames of
	the specialized programs, as well as the filenames of the
	original program and its benchmark files containing runtime
	queries.").

:- use_module(poly_spec(db_pcpe)).
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(api(api_module), [get_packages_to_output/1]).
:- use_module(library(filenames), [basename/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [file_exists/1, delete_file/1]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).

:- use_module(library(read), [read/2]). 
:- use_module(library(write)). 
:- use_module(library(strings)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(sort)).

:- op(1150, fx, [profile]).

:- pred get_best_solution_file_name(+Id,-Out) # "It returns the name
	@var{Out} of the best solution (specialized)program, based on
	a unique @var{Id} and the current file being specialized".

get_best_solution_file_name(Id,Out):-
	curr_file(F,_),
	basename(F,Base),
	current_pp_flag(poly_strategy,Strat),
	atom_number(At_id,Id),
	atom_concat([Base,'_',Strat,'_',At_id,'_co.pl'],Out).


:- pred get_output_file_name(+Id,-Out) 
	# "It obtains the name @var{Out} of the output
          (specialized)program, based on a unique @var{Id} and the
          current file being specialized".

get_output_file_name(Id,Out):- 
	curr_file(F,_),
	atom_concat([Base,'.pl'],F), 
	atom_number(At_id,Id),
	atom_concat([Base,'_',At_id,'_co.pl'],Out).

:- pred get_tmp_file_name(-Out) 
	# "It obtains the name @var{Out} of the temporary
	  (specialized) program, usually used when closing
	  intermediate configurations".

get_tmp_file_name(Out):- 
	curr_file(F,_),
	atom_concat([Base,'.pl'],F), 
	atom_concat([Base,'_tmp_co.pl'],Out).


:- pred get_src_name(-Orig) # "It obtains the name and extension of
	the original source program".

get_src_name(Orig):-
	curr_file(F,_),
	atom_concat([Orig,'.pl'],F).

:- pred get_spec_name(+Orig,+Suffix,-Spec) # "It obtains the name of
	the specialized program (with a given suffix)".


get_spec_name(Orig,Suffix,Spec):-
	(Suffix \== tmp ->
	    atom_number(At_Suffix,Suffix)
	;
	    At_Suffix=Suffix),	
	atom_concat([Orig,'_',At_Suffix,'_co'],Spec).


:- pred create_driver_file(+F,-Drv) #"Takes a name of a source prolog
	file @var{F} and it creates a file containing a loop that runs @tt{n}
	tims a runtime query given by the user in the original source
	file".

create_driver_file(F,DrvPL):-
	atom_concat([F,'.pl'],FPL),
	get_src_name(Orig),
	atom_concat([Orig,'_sp_co'],Drv),
	atom_concat([Drv,'.pl'],DrvPL),
 	atom_concat([Drv,'.po'],DrvPO),
	(file_exists(DrvPO)->
	    delete_file(DrvPO)
	;
	    true), % to generate new timestamp
	open(FPL,read,S1),
	open(DrvPL,write,S2),
	init_driver_file(S2),
	read(S1,Line),
	consume_module(Line,S1),
	copy_rest(S1,S2),
	close(S1),
	close(S2).	

:- pred init_driver_file(+S) #"Writes into stream @var{S} a few lines
	of code containing common code for the loop of the runtime
	query".

init_driver_file(S):-
	build_package_list(Packages),
	portray_clause(S,(:- module( _, [pcpe_rt_test/0, pcpe_dummy_test/0] , Packages ))),nl(S),
	(profiling ->
	    portray_clause(S,(:- profile pcpe_rt_test/0)),nl(S)
	;
	    true),
	findall(Q,current_fact(rtquery(Q)),L),
	write_rt_queries(L,S),
	rt_iterations(Iterations),
	portray_clause(S,(pcpe_rt_test :- loop(Iterations))),nl(S),
	portray_clause(S,(pcpe_dummy_test :- dummy_loop(Iterations))),nl(S),
	portray_clause(S,(loop(N):-N>0,!,\+ \+ callit,N1 is N-1,loop(N1))),nl(S),
	portray_clause(S,(loop(_))),nl(S),nl(S),
	portray_clause(S,(dummy_loop(M):-M>0,!,dummy,M1 is M-1,dummy_loop(M1))),
	portray_clause(S,(dummy_loop(_))),nl(S),nl(S),
	portray_clause(S,dummy),nl(S).



:- pred copy_rest(S1,S2) #"Copy the contents of stream @var{S1} in the
	stream @var{S2}".

copy_rest(S1,S2):-
	repeat,
	get_line(S1,Line),
	(Line == end_of_file ->
	    true
	;
	    write_string(S2,Line),nl(S2),
	    fail).
copy_rest(_,_).


:- pred consume_module(Line,S) #"Reads lines in stream @var{S} until a
	module/2 or a module/3 declaration is found.".

consume_module(Line,_):- 
	Line = (:-module(_,_)),!.
consume_module(Line,_):- 
	Line = (:-module(_,_,_)),!.
consume_module(_,S1):- 
	read(S1,Line1), 
	consume_module(Line1,S1).


:- pred write_rt_queries(L, S) #"Writes in stream @var{S} all previously
	asserted runtime queries contained in @var{L}, specified by
	the user in the current file".

write_rt_queries([],S):-
	portray_clause(S,callit),nl(S).
write_rt_queries([Q|Qs],S):-
	portray_clause(S,(callit :- Q, fail)),
	write_rt_queries(Qs,S).


:- pred build_package_list(-Packages) #"Collects all @var{Packages}
	needed by the resulting program".

build_package_list(Packages):-
	get_packages_to_output(R1),
	(profiling ->
	    R2 = [profiler,pcpe_rtquery|R1]
	;
	    (time_efficiency ->
	        R2 = [pcpe_rtquery|R1]
	    ;
	        R2 = R1)),
	sort(R2,Packages).
		
