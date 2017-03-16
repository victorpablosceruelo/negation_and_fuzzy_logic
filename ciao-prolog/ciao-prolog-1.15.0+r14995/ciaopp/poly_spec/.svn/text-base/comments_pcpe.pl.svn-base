:- module(comments_pcpe,
	[
	    add_comments_to_program/1,
	    write_sh/3
	],[]).

:- use_package(api(ciaopp_api)).
:- use_package(assertions).

:- doc(title,"PCPE comments").

:- doc(author, "Claudio Ochoa").

:- doc(module,"This module creates and adds comments to he
	specializaed programs, that contain information that can be
	usefule to the user.").

:- use_module(db_pcpe).
:- use_module(stats_pcpe, [get_spec_params/3]).
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(filenames), [basename/2]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(lists), [list_concat/2, append/3, reverse/2]).
:- use_module(library(write)).
:- use_module(library(format)).

:- pred add_comments_to_program(+Sol) # "Adds comments to program
	depending on the value of flag @tt{output_info}".

:- push_prolog_flag(multi_arity_warnings,off).


add_comments_to_program(Sol):-
	current_pp_flag(output_info,Out_info),
	add_comments_to_program(Out_info,Sol).

add_comments_to_program(none,_Sol):-!.
add_comments_to_program(medium,Sol):-!,
	get_spec_params_str(Sol,Comment),
	build_add_comment(Comment).
add_comments_to_program(high,Sol):-
	get_spec_params_str(Sol,Params),
	get_global_atoms_str(Sol,Atoms),
	append(Params,Atoms,Comment),
	build_add_comment(Comment).

:- pop_prolog_flag(multi_arity_warnings).


:- pred build_add_comment(+Comment) #"Adds a comment to the program
	being generated".

build_add_comment(Comment):-
	C = comment${ where   => begin,                  
	              type    => c,
		      comment => Comment
		    },		
        add_comment(C).



:- pred get_spec_params_str(+Sol,-) # "Returns a string representing
	the percentaje of global and local control used to obtain
	every atom in solution @var{Sol}".

get_spec_params_str(Sol,Comment):-
	get_spec_params(Sol,Perc,_),
	info_to_str(Perc,Info),
	list_concat(["\nGlobal and Local control combinations\n",Info,"\n\n"],Comment).

info_to_str([],"").
info_to_str([(H,Count,Perc)|T],Comment):-
	number_codes(Count,Count_str),
	number_codes(Perc,Perc_str),	
	get_parameter(H,(GC,LC)),
	info_to_str(T,NT),
	list_concat([GC," + ",LC," -> Count:",Count_str," - Perc:",Perc_str,"\n",NT],Comment).


get_parameter((Glob,Loc),(GC,LC)):-
%	global(G_ind,Glob),
%	local(L_ind,Loc),
	atom_codes(Glob,GC),
	struct_code(Loc,LC).

struct_code([],"").
struct_code([H|T],HC):-
	functor(H,Prop,_A),
	arg(1,H,Param),
	atom_codes(Prop,Pr),
	(number(Param)->
	    number_codes(Param,Par)
	;
	    atom_codes(Param,Par)),
	struct_code(T,TC),
	(T==[]->Sep="";Sep="+"),
	list_concat([Pr,"(",Par,")",Sep,TC],HC).

/*
functor_code([],"").
functor_code([H|T],HC):-
	functor(H,Name,Arity),
	atom_codes(Name,N),
	number_codes(Arity,A),
	functor_code(T,TC),
	list_concat([N,"/",A," ",TC],HC).

list_atom_code([],"").
list_atom_code([H|T],L):-
	atom_codes(H,NH),
	list_atom_code(T,NT),
	list_concat([NH," ",NT],L).
*/

:- pred get_global_atoms_str(+Sol,-Atoms) : (list(Sol) , list(Atoms)) #
	"Retrieves a list of atoms present in the global control of
	solution @var{sol}".

get_global_atoms_str(e([],Visited,_Value),Atoms_str):-
	reverse(Visited,Visited_r),
	collect_atoms(Visited_r,Atoms),
	rename(Atoms,Renamed_Atoms),
	atoms_to_str(Renamed_Atoms,Atoms_codes),
	list_concat(["***Atoms in Global Control***\n",Atoms_codes,"*************************\n"],Atoms_str).

atoms_to_str([],[]).
atoms_to_str([(H,GU)|T],R):-
	atoms_to_str(T,NT),
	format_to_string("~w - ~w~n",[H,GU],NH),
	list_concat([NH,NT],R).

collect_atoms([],[]):-!.
collect_atoms([t(_A,AG,GU)|Tuples],[(AG,GU)|T]):-
	collect_atoms(Tuples,T).

%rename_and_sort(L,R):-
%	rename(L,R),
%	sort(L1,R).

rename([],[]).
rename([(H,GU)|T],[(NH,GU)|NT]):-
	copy_term(H,NH),
	numbervars(NH,0,_),
	rename(T,NT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_sh(Sols,fits(Fits),Best):-
	oracle_calibration(first),!,
	curr_file(Src,_),
	basename(Src,Base),
	atom_concat([Base,'.spec_his_best'],FileBest),
	atom_concat([Base,'_spec_his.pl'],FileAll),
	write_all_spec_his(Sols,Fits,FileAll),
	write_sh_best(Sols,Best,FileBest).
write_sh(_,_,_).



write_sh_best(Sols,best(Best),FileBest):-
	find_best(Sols,1,Best,Vis),
	write_spec_history_best(Vis,FileBest).



find_best([e(_,V,_)|_],Best,Best,V).
find_best([_|R],B,Best,V):-
	B1 is B + 1,
	find_best(R,B1,Best,V).

write_spec_history_best(Visited,Filename):-
	open(Filename,write,Out),
	write_visited(Visited,best,Out),
	close(Out).

write_all_spec_his(Sols,Fits,Filename):-
	open(Filename,write,Out),
	format(Out,":- module(_,_).~n~n",[]),
	write_all_spec_his_(Sols,Fits,Out),
	close(Out).

write_all_spec_his_([],[],_).
write_all_spec_his_([e(_,V,_)|Sols],[(Sol,Info)|Fits],Out):-
	member(fitness(Fit),Info),
	format(Out,"solution(~q,fit(~q),history([",[Sol,Fit]),
	write_visited(V,all,Out),
	format(Out,"end])).~n",[]),
	write_all_spec_his_(Sols,Fits,Out).


write_visited(Visited,Who,Out):-
	reverse(Visited,Visited_r),
	collect_atoms(Visited_r,Atoms),
	rename(Atoms,Renamed_Atoms),
	(Who == best -> 
	   write_atoms_best(Renamed_Atoms,Out)
	;
	   write_atoms_all(Renamed_Atoms,Out)).

write_atoms_best([],_).
write_atoms_best([(_,GU)|T],Out):-
	format(Out,"history(~w).~n",[GU]),
	write_atoms_best(T,Out).


write_atoms_all([],_).
write_atoms_all([(_,GU)|T],Out):-
	format(Out,"~w,",[GU]),
	write_atoms_all(T,Out).

