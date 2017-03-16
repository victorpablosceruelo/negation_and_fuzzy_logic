:- module(stats_pcpe,
	[
	    get_spec_params/3, 
	    get_best/2, 
	    get_bests/2
	],[assertions, isomodes]).

:- doc(title,"PCPE statistics").

:- doc(author, "Claudio Ochoa").

:- doc(module,"This module estimates statistics related with the
	obtained specialized solutions.").


:- use_module(customization, [global/2, local/2]).
:- use_module(filenames_pcpe, [get_output_file_name/2]).
:- use_module(plot).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(assoc)).
:- use_module(library(lists), [append/3, length/2, last/2]).
:- use_module(library(sort), [sort/2]). 
:- use_module(library(math(stat(stat_basic))),
	[lvariance/2, lmean/2, ldiameter/2]).
:- use_module(library(strings), [get_line/2]).


:- pred get_spec_params(+Sol, -Perc, -Type) # "Returns the parameters
	of the poly-controlled specialization".

get_spec_params(Sol,Perc,Type):-
	Sol=e([],Visited,_Value),
	stats(Visited,Perc),
	purity(Perc,Type).	

:- pred purity(+,-) # "Determines if a solution is pure or hybrid".

purity([_],pure).
purity([_|_],hybrid).


:- pred stats(+,-) # "Generates statistics for the current
	solution. These include the percentage of control strategies
	used with every atom of the global control".

stats(Visited,Perc):-
	get_all_comb(Visited,Combs),
	empty_assoc(Emp_dic),
	list_to_dict(Combs,Emp_dic,Dict),
	sort(Combs, Scombs),
%	get_totals(Scombs,Dict,0,Totals),
	length(Visited,Len),
	get_percentages(Scombs,Len,Dict,Perc).

get_percentages([],_,_,[]).
get_percentages([H|T],Total,Dict,[(NH,Count,Perc)|NT]):-
	(get_assoc(H,Dict,Count)->
	    Perc is Count * 100 / Total
	; 
	    Count = 0,
	    Perc = 0
	),
	translate(H,NH),
	get_percentages(T,Total,Dict,NT).


translate((IG,IL),(G,U)):-
	global(IG,G),
	local(IL,U).

/*
get_totals([],_Dict,C,C).
get_totals([H|T],Dict,C,FC):-
	(get_assoc(H,Dict,Count)->
	    NC is C + Count
	; 
	    NC is C 
	),
	get_totals(T,Dict,NC,FC).
*/

get_all_comb([],[]):-!.
get_all_comb([t(_,_,GU)|T],All):-
	get_all_comb(T,NT),
	append(GU,NT,All).


list_to_dict([],L,L).
list_to_dict([H|T],Curr_dict,Final_dict):-
	(get_assoc(H,Curr_dict,Count)->
	    NCount is Count + 1,
	    update_assoc(H,Curr_dict,NCount,New_dict,Count)
	;
	    add_assoc(H,Curr_dict,1,New_dict)),
	list_to_dict(T,New_dict,Final_dict).


/*
print_stats([]).
print_stats([(H,Count,Perc)|T]):-
	write(H),write(': '),
	write(Count),write('  '),
	write(Perc),write('%'),nl,
	print_stats(T).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred get_best(+Info, -Best) # "Returns best solution, based on
fitness value. If several solutions have the same fitness value, it
returns any of them".

get_best(Info,Best):- best_prog(Info,Best).

best_prog([],no).
best_prog([E],E):-!.
best_prog([(F1,St1),(F2,St2)|T],Best):-
	member(fitness(S1),St1),
	member(fitness(S2),St2),
	(S1>S2 ->
	    best_prog([(F1,St1)|T],Best)
	;
	    best_prog([(F2,St2)|T],Best)
	).

:- pred get_bests(+Info, -Bests) # "Returns best solutions, based on
fitness value. There may be more than one solution with the same
fitness value".


get_bests(Info,(mean(Avg),variance(Var),diameter(Diam),bests(Bests_f))):-
	filter_fit(Info,Fit),
	get_stats(Fit,Avg,Var,Diam),
	(current_pp_flag(output_info,high) ->
	    generate_gnu_plot_file(Fit,[('Mean',Avg),('Variance',Var),('Diameter',Diam)])
	;
	    true),
	best_fit(Fit,BF),
	filter_progs(Info,BF,Bests),
	(current_pp_flag(output_info,high) ->
	    add_global_ctrl_atoms(Bests,Bests_f)
	;
	    Bests_f=Bests).

filter_fit([],[]).
filter_fit([(_F,St)|T],[Fit|NT]):-
	member(fitness(Fit),St),
	filter_fit(T,NT).

best_fit(L,F):-
	sort(L,L1),
	last(L1,F).



filter_progs([],_,[]).
filter_progs([(F,St)|T],Fit,[(F,St)|NT]):-
	member(fitness(Fit),St),!,
	filter_progs(T,Fit,NT).
filter_progs([(_F,_St)|T],Fit,NT):-
	filter_progs(T,Fit,NT).




get_stats(L,Avg,Var,Diam):-
	lmean(L,Avg),
	lvariance(L,Var),	     
	ldiameter(L,Diam).

add_global_ctrl_atoms([],[]).
add_global_ctrl_atoms([(F,St)|R],[(F,St2)|R2]):-
	get_atoms(F,Atoms),
	append(St,[atoms(Atoms)],St2),
	add_global_ctrl_atoms(R,R2).

get_atoms(Id,Atoms):-
	get_output_file_name(Id,File),
	open(File,read,Handle),
	read_atoms(Handle,Atoms),
	close(Handle).

read_atoms(Handle,Atoms):-
	get_line(Handle,Line),
	(Line == "***Atoms in Global Control***" ->
	    atoms(Handle,[],Atoms)
	 ;
	    read_atoms(Handle,Atoms)).

atoms(Handle,Tmp,Atoms):-
	get_line(Handle,Line),
	(Line == "*************************" ->
	    Tmp = Atoms
	;
	    atom_codes(Atom,Line),
	    atoms(Handle,[Atom|Tmp],Atoms)).
	
 


