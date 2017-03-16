:- module(breadth_first_spec,
	[ 
	    bf_spec/7
	],
	[]).

:- use_package(assertions).

:- doc(title,"Breadth-first traversal of search space").

:- doc(author, "Germ@'{a}n Puebla").
:- doc(author, "Claudio Ochoa").

:- doc(module," This module implements a breadth-first traversal
	of search space of poly-controlled partial evaluation.").

:- use_module(control_pcpe).
:- use_module(common_spec).
:- use_module(db_pcpe).
:- use_module(oracle_calibration, [start_level/0, end_level/0]).
:- use_module(oracle).
:- use_module(spec(filter), [decide_predicate_filter/6]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(terms_check), [variant/2, instance/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(lists)).




:- pred bf_spec(+Confs,+G,+U,+Int,+SolsIn,-SolsOut,-Tree)# "Takes a
	list @var{Confs} of configurations, and applies @tt{PCPE} on
	them using a set @var{G} of global control rules, and a set
	@var{U} of local control rules. The search space is traversed
	in a @tt{breadth-first} fashion. Current solutions are stored
	in @var{SolsIn} and later returned in @var{SolsOut}. A
	representation of the search space is (optionally) returned in
	@var{Tree}".

bf_spec([],_G,_U,_,Sols,Sols,tree(Mid,[])):-!,
	assertz_fact(max_id(Mid)).
bf_spec(Confs,G,U,Int,SolsIn,SolsOut,tree(MId,Tree)):-
	debug(iteration),
	start_level,
	spec_one_level(Confs,G,U,Int,NewConfs,nodes(MId,NmId,Nodes1)),    
 	(current_pp_flag(poly_strategy,oracle) ->
	    oracle(NewConfs,Some_Confs)
	;
	    (oracle_calibration(second) -> end_level; true),
	    Some_Confs=NewConfs
	),
	pick_solutions(Some_Confs,Some_Sols,RestConfs),
	append(Some_Sols,SolsIn,Sols_Tmp),
	bf_spec(RestConfs,G,U,Int,Sols_Tmp,SolsOut,tree(NmId,Nodes2)),
	append(Nodes1,Nodes2,Tree).


:- pred spec_one_level(+ConfsIn,+G,+U,Int,-ConfsOut,-) # "Implements the
	breadth-first traversal, by applying the control strategies
	@var{G} and @var{U} to all configurations @{ConfsIn} in a
	given level".

spec_one_level([],_G,_U,_,[],nodes(Mid,Mid,[])).
spec_one_level([Sol],_G,_U,_,[Sol],nodes(Mid,Mid,[])):-
	Sol = e(TV,_,_),
	TV = [],!.
spec_one_level([e([(Atom,Id)|To_Visit],Visited,_Value)|As],G,U,Int,Confs,nodes(MID,NMID,Tree)):-
	decide_predicate_filter(Atom,[],'$top',AtomF,_FSv,_FProj),
	generalize_all(G,AtomF,Visited,Generalized),
	(current_pp_flag(poly_filter_equiv,on) ->
	    filter_equiv_gens(Generalized,Filter_Gen)	
	;
	    Filter_Gen = Generalized),
	unfold_all(Filter_Gen,U,[],Visited,Gen_Unfolded),
	(current_pp_flag(poly_filter_equiv,on) ->
	    filter_equiv_unfolds(Gen_Unfolded,Filter_Gen_Unfolded)
	;
	    Filter_Gen_Unfolded = Gen_Unfolded),
	complete_configurations(Filter_Gen_Unfolded,To_Visit,Visited,Int,Temp_Confs,nodes(MID,Tmp_Id,Childs,Sols)),
	(current_pp_flag(output_info,high) ->
	    append([(Id,Atom,Childs)],Sols,TSols)
	;
	    TSols=[]),	    
	append(Temp_Confs,More_Confs,Confs),
	append(TSols,TS,Tree),
	spec_one_level(As,G,U,Int,More_Confs,nodes(Tmp_Id,NMID,TS)).


:- pred filter_equiv_gens(+,-) # "Filter (just) generalized atoms that
	are variant of one another".

filter_equiv_gens([],[]).
filter_equiv_gens([t(A,AG,GA)|Generalized],Filter_Gen):-
	member(t(B,BG,GB),Generalized),
	variant(AG,BG),!,
	append(GA,GB,GG),
	sort(GG,GGS),
	delete(Generalized,t(B,BG,GB),Generalized_),
	filter_equiv_gens([t(A,AG,GGS)|Generalized_],Filter_Gen).
filter_equiv_gens([G|Generalized],[G|Filter_Gen]):-
	filter_equiv_gens(Generalized,Filter_Gen).


:- pred filter_equiv_unfolds(+,-) # "Filter (just) unfolded atoms that
	are variant of one another".

filter_equiv_unfolds([],[]).
filter_equiv_unfolds([t(A,AG,GUA,TreeA,Unf_statsA)|Gen_Unfolded],Filter_Gen_Unfolded):-
	member(t(B,BG,GUB,TreeB,Unf_statsB),Gen_Unfolded),
	variant(TreeA,TreeB),!,
	replace(Gen_Unfolded,t(A,AG,GUA,TreeA,Unf_statsA),t(B,BG,GUB,TreeB,Unf_statsB),Gen_Unfolded_),
	filter_equiv_unfolds(Gen_Unfolded_,Filter_Gen_Unfolded).
filter_equiv_unfolds([G|Gen_Unfolded],[G|Filter_Gen_Unfolded]):-
	filter_equiv_unfolds(Gen_Unfolded,Filter_Gen_Unfolded).


replace(L,CA,CB,NL):-
	choose_conf(CA,CB,Winner,CR),
	(Winner == 1 -> 
	    delete(L,CB,L1),
	    update([CA|L1],CA,CR,NL)
	;
	    update(L,CB,CR,NL)).

choose_conf(t(A,_,GUA,TA,USA),t(B,_,GUB,TB,USB),Winner,GGS):-
 	current_pp_flag(poly_strategy,oracle),!,
 	assess(o(A,GUA,TA,USA,[]),no,Q1),
  	assess(o(B,GUB,TB,USB,[]),no,Q2),
	oracle([e([1],[],Q1),e([2],[],Q2)],[e([Winner],[],_)|_]),
	(Winner == 1 -> 
	    append(GUA,GUB,CS)
	;
	    append(GUB,GUA,CS)),
	sort(CS,GGS).
choose_conf(t(_A,AG,GUA,_TA,_USA),t(_B,BG,GUB,_TB,_USB),1,GGS):-
	instance(BG,AG),!,
	append(GUA,GUB,GG),
	sort(GG,GGS).
choose_conf(t(_A,AG,GUA,_TA,_USA),t(_B,BG,GUB,_TB,_USB),2,GGS):-
	instance(AG,BG),!,
	append(GUB,GUA,GG),
	sort(GG,GGS).
choose_conf(t(_A,_AG,GUA,_TA,_USA),t(_B,_BG,GUB,_TB,_USB),1,GGS):-
	warning(['{In breadth_first_spec:choose_conf: new code are variants but atoms not. Report this to developers.}']),
	append(GUA,GUB,GG),
	sort(GG,GGS).


update([C|T],C,CR,[t(A,AG,CR,AT,U)|T]):-
	C = t(A,AG,_,AT,U),!.
update([C1|T],C2,CR,[C1|R]):-
	update(T,C2,CR,R).



:- pred pick_solutions(+Conf,-Sols,-Rest_Confs) # "Splits a set
         @var{Conf} of configurations into two sets, one containing
         solutions @var{Sols} and the other one containing non-final
         configurations @{Rest_\Confs}".


pick_solutions([],[],[]).
pick_solutions([e([],Visited,Value)|Confs],Some_Sols,Rest_Conf):-!,
	Some_Sols = [e([],Visited,Value)|Other_Sols],
	pick_solutions(Confs,Other_Sols,Rest_Conf).
pick_solutions([Conf|Confs],Some_Sols,[Conf|Rest_Conf]):-
	pick_solutions(Confs,Some_Sols,Rest_Conf).

