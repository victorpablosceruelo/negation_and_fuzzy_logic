:- module(oracle_calibration,
	[
	    init_history/0,
	    start_level/0,
	    end_level/0,
	    start_siblings/0,
	    end_siblings/0,
	    follow_spec_history/2,
	    choose_by_hist/5,
	    write_conf/2
	],[]).

:- use_package(assertions).
%:- use_package(.(notime_stats)).

:- use_module(library(lists),      [append/3, reverse/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(compiler),   [use_module/1]).
:- use_module(library(filenames),  [no_path_file_name/2, basename/2]).


:- doc(title,"Calibration of Oracle-based Poly-Controlled Partial Deduction").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module calibrates the oracle. In order to do
	so, the flag @var{oracle_calibration} in @tt{db_pcpe} must be
	set to @tt{first}, and the PCPE must be run in all-solutions
	mode to generate the corresponding specialization
	history. After that, the flag @var{oracle_calibration} in
	@tt{db_pcpe} must be set to @tt{second}, and PCPE must be run
	again to generate the .signature files which then can be
	solved with a constraint solver or by a least squares
	solver").
%



:- use_module(db_pcpe).
:- use_module(library(read), [read/2]). 
:- use_module(library(format)).
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [file_exists/1, delete_file/1]).



:- data history/1.
:- data cur_mod/1.
%:- data cur_his/1.

init_history:-
	oracle_calibration(second),!,
	curr_file(Src,_),
	basename(Src,Base),
	clean_up_data(Base),
	load_solutions(Base),
	assert_his_best(Base).
init_history.

clean_up_data(Base):-
	retractall_fact(history(_)),
%	retractall_fact(cur_his(_)),
	retractall_fact(cur_mod(_)),
	atom_concat([Base,'.signature'],Filename),
	(file_exists(Filename) ->
	    delete_file(Filename)
	;
	    true).

load_solutions(Base):-
	atom_concat([Base,'_spec_his.pl'],Filename),
	(file_exists(Filename) -> 
	    basename(Filename,M),
	    use_module(M),
	    no_path_file_name(M,ModHist),
	    asserta_fact(cur_mod(ModHist))

	 ;
	    error(['File ',Filename,' does not exist!']),
	    error(['Please run the first phase of the oracle calibration']),
	    error(['Read the Documentation on this matter'])).


assert_his_best(Base):-
	atom_concat([Base,'.spec_his_best'],Filename),
	file_exists(Filename),!,
	open(Filename,read,S),
	assert_history(S),
	close(S).


assert_history(S):-
	repeat,
	read(S,Line),
	(Line == end_of_file ->
	    true
	;
	    Line = history(GU),
	    assertz_fact(history(GU)),
	    fail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


follow_spec_history([e([a],[],_)|_],_):- !,fail.
follow_spec_history(_,GU):-
	retract_fact(history(GU)).
%	update_cur_his(GU).


open_signature(S):-	
	curr_file(Src,_),
	basename(Src,Base),
	atom_concat([Base,'.signature'],Filename),
	open(Filename,append,S).
close_signature(S):- close(S).	

start_level:-
	oracle_calibration(second),!,
	open_signature(Out),
	format(Out,"level([",[]),
	close_signature(Out).
start_level.

end_level:-
	oracle_calibration(second),!,
	open_signature(Out),
	format(Out,"end]).~n",[]),
	close_signature(Out).
end_level.

start_siblings:-
	oracle_calibration(second),!,
	open_signature(Out),
	format(Out,"[",[]),
	close_signature(Out).
start_siblings.

end_siblings:-
	oracle_calibration(second),!,
	open_signature(Out),
	format(Out,"end],",[]),
	close_signature(Out).
end_siblings.

/*
variables(bytecode,3).
variables(speedup,3).
variables(balance,5).

write_vars(N,N,Out):-
	format(Out,"V~q",[N]).
write_vars(N,M,Out):-
	format(Out,"V~q,",[N]),
	N1 is N+1,
	write_vars(N1,M,Out).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


write_conf(H,values(Values)):-
	get_spec_history(H,SpecHis),
	open_signature(Out),
	mfv(SpecHis,Fit),
	format(Out,"(fit(~q),vec([",[Fit]),
	write_values(Values,Out),
	close_signature(Out),!.

get_spec_history(H,His):-
	filter_strats(H,Strats),
	reverse(Strats,His).

filter_strats([],[]).
filter_strats([t(_,_,CS)|T],[CS|NT]):-
	filter_strats(T,NT).


write_values([V],Out):-!,
	format(Out,"~q])),",[V]).
write_values([V|Vs],Out):-
	format(Out,"~q,",[V]),
	write_values(Vs,Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

choose_by_hist([e(S,H,Val)|_],GU,N,N,e(S,H,Val)):-
	H = [t(_,_,GU)|_],!.
choose_by_hist([e(S,H,Val)|_],[GU],N,N,e(S,H,Val)):-
	H = [t(_,_,GUs)|_],
	member(GU,GUs),!.
choose_by_hist([_|T],GU,N,P,Picked):-
	N1 is N + 1,
	choose_by_hist(T,GU,N1,P,Picked).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mfv(Hist,Fit):-
	current_fact(cur_mod(M)),
	findall(Fit,
	       (M:solution(_,fit(Fit),history(H)),
		append(Hist,_,H)),
	       Sols),
	mfv_(Sols,Fit),!.
mfv(Hist,-1):-
	warning(['There was a problem estimating the mfv']),
	warning(['Please check that this specialization history:',Hist]),
	warning([' is in the spec_his file. Have you run the first phase of the algorithm?']).

mfv_([Fit],Fit).
mfv_([F1,F2|T],Fit):-
	F1 > F2,
	mfv_([F1|T],Fit).
mfv_([_,F2|T],Fit):-
	mfv_([F2|T],Fit).
/*
same_branch([],_).
same_branch([[GU]|GUs],[Step|Steps]):-
	member(GU,Step),!,
	same_branch(GUs,Steps).
same_branch([GU|GUs],[GU|Steps]):-
	same_branch(GUs,Steps).
*/
