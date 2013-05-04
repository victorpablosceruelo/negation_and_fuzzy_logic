:- module(control_pcpe,
	[
	 unfold_all/5,
	 unfold_one_step/4,
	 unfold/4,
	 generalize_all/4,
	 generalize/4,
	 dyn/3,
	 id/3
	],[]).

:- use_package(hiord).
:- use_package(assertions).
:- use_package(.(notime_stats)).

:- doc(title,"Poly-Controlled Partial Evaluation Control Strategies").

:- doc(author, "Germ@'{a}n Puebla").
:- doc(author, "Claudio Ochoa").

:- doc(module," This module implements the control strategies for
	poly-controlled partial evaluation.").

:- use_module(customization).
:- use_module(db_pcpe).
:- use_module(chtrees_pcpe).
:- use_module(modes, [abstract/2]).
:- use_module(spec(unfold), [unf_int/11]).
:- use_module(spec(unfold_stats), [reset_unfold_stats/0, ask_unfold_stats/1]).
:- use_module(ciaopp(preprocess_flags), [push_pp_flag/2, pop_pp_flag/1]).
:- use_module(library(lists),           [append/3]).
:- use_module(library(prolog_sys),      [statistics/2]). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Breadth-first

:- pred generalize_all(+G,+A,+,-Generalized) #"Applies all global
	control rules in @var{G} to atom @var{A}, and returns
	temporary configurations @var{Generalized}. Used in
	breadth-first traversal.".

generalize_all(Gs,A,Visited,Generalized):-
	statistics(runtime,[_,_]),
	generalize_all_(Gs,A,Visited,Generalized),
	statistics(runtime,[_,T_g]),
	increment_time(global_ctrl,T_g).

generalize_all_([],_A,_Visited,[]).
% use same abstraction for all atoms of a pred
generalize_all_(_,A,Visited,[t(A,AG,[Ind_G])]):-  
	pred_consistency,
	functor(A,Name,Arity),
	functor(Goal,Name,Arity),
	member(t(_,Goal,[(Ind_G,_)|_]),Visited),!,
	global(Ind_G,G),
	G(Visited,A,AG).
% use same abstraction for all atoms of a pred with same mode
generalize_all_(_,A,Visited,[t(A,AG,[Ind_G])]):-  
	modes_consistency,
	functor(A,Name,Arity),
	functor(Goal,Name,Arity),
	member(t(_,Goal,[(Ind_G,_)|_]),Visited),
	abstract(A,A_abst),
	abstract(Goal,A_abst),!,
	global(Ind_G,G),
	G(Visited,A,AG).
generalize_all_([Ind_G|Gs],A,Visited,[t(A,AG,[IndF_G])|Generalized]):-
	global(Ind_G,G),
	G(Visited,A,AG),
	IndF_G=Ind_G,
	generalize_all_(Gs,A,Visited,Generalized).

:- pred unfold_all(+C,+Unfold,+,+,-) #"Applies an unfolding rule
	@var{Unfold} to all temporary configurations stored in
	@var{C}. Used in breadth-first traversal.".

unfold_all(Gs,U,More_Unf,Vis,Gen_Unfolded):-
	statistics(runtime,[_,_]),
	unfold_all_(Gs,U,More_Unf,Vis,Gen_Unfolded),
	statistics(runtime,[_,T_u]),
	increment_time(local_ctrl,T_u).


unfold_all_([],_U,Gen_Unfolded,_Vis,Gen_Unfolded):-!.
unfold_all_([G|Gs],U,Some_Unf,Vis,Gen_Unfolded):-
	unfold_all_unf(U,G,Vis,Tmp_Unfolded),
	append(Tmp_Unfolded,Some_Unf,More_Unf),
	unfold_all_(Gs,U,More_Unf,Vis,Gen_Unfolded).

unfold_all_unf([],_G,_Vis,[]).
% use same unfolding for all atoms of a pred
unfold_all_unf(_,t(A,AG,G),Vis,[t(A,AG,GU,Tree,Unf_stats)]):-  
	pred_consistency,
	functor(AG,Name,Arity),
	functor(Goal,Name,Arity),
	member(t(_,Goal,[(_,Ind_U)|_]),Vis),!,
	local(Ind_U,Unfold),
	unfold(Unfold,AG,Tree,Unf_stats),
	pairs(G,Ind_U,GU).
% use same unfolding for all atoms of a pred with same mode
unfold_all_unf(_,t(A,AG,G),Vis,[t(A,AG,GU,Tree,Unf_stats)]):-  
	modes_consistency,
	functor(AG,Name,Arity),
	functor(Goal,Name,Arity),
	member(t(_,Goal,[(_,Ind_U)|_]),Vis),
	abstract(A,A_abst),
	abstract(Goal,A_abst),!,
	local(Ind_U,Unfold),
	unfold(Unfold,AG,Tree,Unf_stats),
	pairs(G,Ind_U,GU).
unfold_all_unf([Ind_U|Us],t(A,AG,G),Vis,[t(A,AG,GU,Tree,Unf_stats)|Tmp_Unfolded]):-
	local(Ind_U,Unfold),
	unfold(Unfold,AG,Tree,Unf_stats),
	pairs(G,Ind_U,GU),
	unfold_all_unf(Us,t(A,AG,G),Vis,Tmp_Unfolded).

pairs([],_,[]).
pairs([G|Gs],U,[(G,U)|T]):-
	pairs(Gs,U,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Depth-first


:- pred unfold_one_step(+Unfold,+C,+,-) #"Applies an unfolding rule
	@var{Unfold} to temporary configuration @var{C}. Used in
	depth-first traversal.".

unfold_one_step(U,T,Vis,NT):-
	statistics(runtime,[_,_]),
	unfold_one_step_(U,T,Vis,NT),
	statistics(runtime,[_,T_u]),
	increment_time(local_ctrl,T_u).



% use same unfolding for all atoms of a pred
unfold_one_step_(Ind_U,t(A,AG,Ind_G),Visited,t(A,AG,[(Ind_G,Ind_U)],Tree,_)):-
	pred_consistency,
	functor(A,Name,Arity),
	functor(Goal,Name,Arity),
	member(t(_,Goal,[(_,Ind_U2)|_]),Visited),!,
	Ind_U==Ind_U2,
	local(Ind_U,U),
	unfold(U,AG,Tree,_),!.
% use same unfolding for all atoms of a pred  with same mode
unfold_one_step_(Ind_U,t(A,AG,Ind_G),Visited,t(A,AG,[(Ind_G,Ind_U)],Tree,_)):-
	modes_consistency,
	functor(A,Name,Arity),
	functor(Goal,Name,Arity),
	member(t(_,Goal,[(_,Ind_U2)|_]),Visited),
	abstract(A,A_abst),
	abstract(Goal,A_abst),!,
	Ind_U==Ind_U2,
	local(Ind_U,U),
	unfold(U,AG,Tree,_),!.
unfold_one_step_(Ind_U,t(A,AG,Ind_G),_Visited,t(A,AG,[(Ind_G,Ind_U)],Tree,_)):-
	local(Ind_U,U),
	unfold(U,AG,Tree,_),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred generalize(+G,+A,+,-Generalized) #"Applies a global
	control rules @var{G} to atom @var{A}, and returns a temporary
	configuration @var{Generalized}. Used in depth-first
	traversal.".

generalize(G,A,Vis,T):-
	statistics(runtime,[_,_]),
	generalize_(G,A,Vis,T),
	statistics(runtime,[_,T_g]),
	increment_time(global_ctrl,T_g).



% use same abstraction for all atoms of a pred
generalize_(Ind_G,A,Visited,t(A,AG,Ind_G)):-
	pred_consistency,
	functor(A,Name,Arity),
	functor(Goal,Name,Arity),
	member(t(_,Goal,[(Ind_G2,_)|_]),Visited),!,
	Ind_G==Ind_G2,
	global(Ind_G,G),
	G(Visited,A,AG),!.
% use same abstraction for all atoms of a pred with same mode
generalize_(Ind_G,A,Visited,t(A,AG,Ind_G)):-
	modes_consistency,
	functor(A,Name,Arity),
	functor(Goal,Name,Arity),
	member(t(_,Goal,[(Ind_G2,_)|_]),Visited),
	abstract(A,A_abst),
	abstract(Goal,A_abst),!,
	Ind_G==Ind_G2,
	global(Ind_G,G),
	G(Visited,A,AG).
generalize_(Ind_G,A,Visited,t(A,AG,Ind_G)):-
	global(Ind_G,G),
	G(Visited,A,AG),!.

:- pred unfold(+U,+A,-Res,-Stats)#"Applies unfolding rules @var{U} to
	atom @var{A}, obtaining some resultants @var{Res} and
	collecting some unfolding statistics @var{Stats}".

unfold(Rules,Atom,Resultants,[nondet-nl(Ndnl),atoms(Atoms)|Unf_stats]):-
	get_rules(Rules,Real_Rule,Comp_Rule),
	push_pp_flags(Rules),
	functor(Atom,F,A),
	reset_unfold_stats,
	unf_int(Real_Rule,Comp_Rule,Atom,[],top,pd,_Id,F,A,Resultants,Chtree),
%	display(Chtree),nl,
	check_ndnl(Chtree,Ndnl),
%	(Ndnl \== 0 -> display('ndnl '),display(Ndnl),nl; true),
	count_atoms(Chtree,Atoms),
	ask_unfold_stats(Unf_stats),
        pop_pp_flags(Rules).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GLOBAL CONTROL RULES

:- pred id(_,+A,-B) #"Generalizes the atom @var{A} into @var{B} by
	using the identity abstraction".

id(_,A,B):-
	copy_term(A,B).

:- pred dyn(_,+A,-B) #"Generalizes the atom @var{A} into @var{B} by
	abstracting away all arguments".


dyn(_,A,AG):-
	functor(A,F,Arity),
	functor(AG,F,Arity).

/*
:- pred hom_emb(+Vis,+A,-B) #"Generalizes the atom @var{A} into
	@var{B} only if @var{A} is not homeomorphically embedded by
	any ancestors in @var{Vis}".

hom_emb(Visited,A,B):-
	member(t(_,Goal,_),Visited),
	homeomorphic_embedded(Goal,A),!,
	most_specific_generalization(Goal,A,B).
hom_emb(_Visited,A,B):-
	copy_term(A,B).

hom_emb_num(Visited,A,B):-
	member(t(_,Goal,_),Visited),
	homeomorphic_embedded_num(Goal,A),!,
	most_specific_generalization(Goal,A,B).
hom_emb_num(_Visited,A,B):-
	copy_term(A,B).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_rules(Rules,Real_Rule,Comp_Rule):-
	(member(local_control(LC),Rules)->
	    Real_Rule = LC
	;
	    Real_Rule = det
	),
	(member(comp_rule(CR),Rules)->
	    Comp_Rule = CR
	;
	    Comp_Rule = leftmost
	).
	
push_pp_flags([]).
push_pp_flags([local_control(_)|T]):- 
	push_pp_flags(T).
push_pp_flags([comp_rule(_)|T]):- 
	push_pp_flags(T).
push_pp_flags([Rule|T]):-
	functor(Rule,Real_Rule,1),
	arg(1,Rule,Arg),
	push_pp_flag(Real_Rule,Arg),
	push_pp_flags(T).

pop_pp_flags([]).
pop_pp_flags([local_control(_)|T]):- !,
	pop_pp_flags(T).
pop_pp_flags([comp_rule(_)|T]):- !,
	pop_pp_flags(T).
pop_pp_flags([Rule|T]):-
	functor(Rule,Real_Rule,1),
	pop_pp_flag(Real_Rule),
	pop_pp_flags(T).

