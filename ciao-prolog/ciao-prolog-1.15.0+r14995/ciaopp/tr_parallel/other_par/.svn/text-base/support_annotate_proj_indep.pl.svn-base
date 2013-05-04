%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             THIS IS FOR SUPPORT_ANNOTATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op( 950, xfy,[(&),(\&)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% performing the local analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local_analysis(Body,HvFv,Fv):-
	unlinked_conds(Fv,HvFv,Pos_u,[]),
	sort(Pos_u,Pos),
	build_not_def(Fv,Neg,[]),
	local_analysis(Body,Fv,_,HvFv,global(Pos,Neg,[]),_).

unlinked_conds([],_,Tail,Tail).
unlinked_conds([F|Fs],HvFv,[unlinked(I1,I2)|Pos],Tail):-
	ord_subtract(HvFv,[F],Vars),
	( Vars = [] ->
	    Pos = Pos0
	; compare_sets([F],Vars,I1,I2),
	  Pos = [unlinked(I1,I2)|Pos0]
        ),
	unlinked_conds(Fs,HvFv,Pos,Tail).

compare_sets(I1,I2,I3,I4):-
	compare(D,I1,I2),
	compare_sets1(D,I1,I2,I3,I4).

compare_sets1(<,I1,I2,I1,I2).
compare_sets1(=,I1,I2,I1,I2).
compare_sets1(>,I1,I2,I2,I1).

build_not_def([],Tail,Tail).
build_not_def([X|Xs],[not(def([X]))|Neg],Tail):-
	build_not_def(Xs,Neg,Tail).

build_list_if_not_empty([],_,List,List):- !.
build_list_if_not_empty(_,El,[El|Tail],Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propagating the local info
% PACO: no habria que considerear not(unlinked(___)))?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take_out_facts0([],_Vars,_Frees,[]).
take_out_facts0([not(def(D))|Info],Vars,Frees,InfoOut):- !,
	ord_subtract(D,Vars,NewD), 
	build_list_if_not_empty(NewD,not(def(NewD)),InfoOut,Tail),
	take_out_facts0(Info,Vars,Frees,Tail).
take_out_facts0([unlinked(I1,I2)|Info],Vars,Frees,InfoOut):-
	ord_intersect(I1,Vars), !,
	take_out_facts1(Frees,I2,unlinked(I1,I2),InfoOut,RestInfoOut),
	take_out_facts0(Info,Vars,Frees,RestInfoOut).
take_out_facts0([unlinked(I1,I2)|Info],Vars,Frees,InfoOut):-
	ord_intersect(I2,Vars), !,
	take_out_facts1(Frees,I1,unlinked(I1,I2),InfoOut,RestInfoOut),
	take_out_facts0(Info,Vars,Frees,RestInfoOut).
take_out_facts0([I|Info],Vars,Frees,[I|InfoOut]):-
	take_out_facts0(Info,Vars,Frees,InfoOut).

take_out_facts1(Frees,I2,I,[I|RestInfoOut],RestInfoOut):-
	ord_subset(I2,Frees), !.
take_out_facts1(_Frees,_,_I,InfoOut,InfoOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translate local info into Pos,Newg,Imp
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arg_info(g,Vars,global([def(D)],Neg,Imp),global([def(ND)],Neg,Imp)):-
	merge(D,Vars,ND).
%% var(f) does not have a clear meaning in clpr. Our benchmarks don't have it
%% so I have not implemented
%% arg_info(f,Vars,global(Pos,Neg,Imp),global(Pos,TailNeg,Imp)):-
%% 	not_ground_conds(Vars,Neg,TailNeg).
arg_info(?,_Vars,Info,Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% obtaining the global info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For the abstraction a(D,R) in Def:
%  *  Neg = [], 
%  * Pos = [def(D)], 
%  * Imp is computed by def_get_impl 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For the abstraction Abs in Fr:
%  * Neg = not(def(D)) D = {X | {X} not in Abs}
%  * Pos = [unlinked(I1,I2)| forall nonempty I'1 \subseteq I1 and forall
%          nonempty I'2 \subseteq I2: I'1 \cup I'2 \not \in closure(Abs)]
%  * Imp = []
% Pos is computed by first detecting unconstrained variables (which are
% independent of the rest) and then checking the possibly constrained vars
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

global_info(def,a(Ground,Set),_HvFv,Pos,[],Imp):-
	build_list_if_not_empty(Ground,def(Ground),Pos,[]),	
        def_get_impl(Set,Imp).
global_info(fr,as(Old,New),HvFv,Pos,Neg,[]):-
        merge(Old,New,TmpAbs),
        ss_minimise(TmpAbs,Abs), 
	collect_singletons(Abs,SingletonVars),
        ord_subtract(HvFv,SingletonVars,NonDef),
	build_not_def(NonDef,Neg,[]),
	merge_list_of_lists(Abs,ConstrVars),
        ord_subtract(HvFv,ConstrVars,UnconstrVars),
	unlinked_conds(UnconstrVars,HvFv,Pos,Tail),  
	ord_subtract(ConstrVars,SingletonVars,FreeVars),
        powerset(FreeVars,Power),
	fr_get_rest_unlinked(Power,HvFv,SingletonVars,Abs,Tail,[]).
global_info(fd,(F,D),HvFv,Pos,Neg,Imp):-
        global_info(def,D,HvFv,PosD,_,Imp),
        D = a(G,_),
        ord_subtract(HvFv,G,NonGround),
	F = as(_G1,Sh1,_G2,Sh2),
        global_info(fr,as(Sh1,Sh2),NonGround,PosF,Neg,_),
        append(PosD,PosF,Pos).
global_info(shfr,(Sh,Fr),_HvFv,Pos,Neg,Imp):-
	member_value_freeness(Fr,GVars,g),
	ground_conds(GVars,Pos,Pos1),
	member_value_freeness(Fr,FreeVars,f),
	not_ground_conds(FreeVars,Neg,Neg1),
	asubs_to_dep(Sh,Dep,NGVars),
	dep_to_indep(NGVars,Dep,Pos1),
	not_indep(FreeVars,Sh,Neg1,[]),
	ground_imply_ground(NGVars,Sh,Imp,Imp1),
	ground_imply_indep(NGVars,NGVars,Sh,Imp1,Imp2),
	indep_imply_ground(NGVars,Sh,Imp2,Imp3),
	indep_imply_indep(NGVars,NGVars,Sh,Imp3,Imp4),
	ground_imply_not_indep(FreeVars,Sh,Imp4,Imp5),
	indep_imply_not_indep(FreeVars,Sh,Imp5,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getting the implications for DEF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for each (X,SS) and each S in SS we have:
%     1/          def(S) -> def([X])
%     2/    unlinked([X],S) -> def([X])
% The rule 2/ is useful when:
%    * if non(def([X])) is entailed  (we obtain then
%      know that not(unlinked([X],S)))
%    * when both def([X]) and unlinked([X],S) have to be 
%      tested at run-time (we can avoid testing def([X])
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

def_get_impl([],[]).
def_get_impl([(X,SS)|Set],Imp):-
        def_get_impl_each(SS,X,Imp,Tail),
        def_get_impl(Set,Tail).

def_get_impl_each([],_,Tail,Tail).
def_get_impl_each([S|SS],X,[([def(S)]->def([X])),([unlinked(A,B)]->def([X]))|Imp],Tail):-
	compare_sets([X],S,A,B),
	def_get_impl_each(SS,X,Imp,Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getting the rest of facts in Pos for Fr
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Given Abs and the powerset of the set of possibly constrained 
%  but free vars (Power), it computes the following part of Pos:
%  [unlinked(S1,S2) | exists S in Abs s.t. S=S1 \cup S2 \cup S3,
%  S1,S2,S3, are mutually disjunct and non-empty (also
%  for avoiding redundancies, S3 must be a singleton)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fr_get_rest_unlinked([],_,_,_,Tail,Tail).
fr_get_rest_unlinked([Xs|PowerXs],HvFv,Sing,Abs,Pos,Tail):-
	ord_subtract(HvFv,Xs,Rest),
	( Rest = [] ->
	    Pos = Tail1
	;   ord_split_lists_from_list(Xs,Abs,Int,_),
	    merge_list_of_lists(Int,Vars),
	    ord_subtract(Rest,Vars,Rest0),
	    ord_subtract(Vars,Xs,Vars0),
	    powerset(Vars0,Power0),
	    ( Rest0 = [] ->
		Power = Power0
	    ; setproduct_lists(Power0,[Rest0],Power_u,[]),
	    sort(Power_u,Power)
            ),
	    fr_get_rest_unlinked_one(Int,Xs,Power,Sing,Indep),
	    fr_put_indep(Indep,Xs,Pos,Tail1)
        ),
        fr_get_rest_unlinked(PowerXs,HvFv,Sing,Abs,Tail1,Tail).

fr_get_rest_unlinked_one([],_,Power,_,Power).
fr_get_rest_unlinked_one([Ys|Int],Xs,Power,Sing,Indep):-
	ord_subtract(Ys,Xs,NYs),
	fr_get_rest_unlinked_one0(NYs,Power,Sing,NewPower),
	( NewPower = [] ->
	    Indep = []
	; fr_get_rest_unlinked_one(Int,Xs,NewPower,Sing,Indep)).

fr_get_rest_unlinked_one0([],Power,Sing,NewPower):- !,
	ord_split_lists_from_list(Sing,Power,_,NewPower).
fr_get_rest_unlinked_one0(Ys,Power,_,NewPower):- 
	delete_lists_from_list_subset(Ys,Power,NewPower).
	
delete_lists_from_list_subset([],L,L).
delete_lists_from_list_subset([X|Xs],Xss,NotInt):-
	delete_lists_from_list_subset1(Xss,[X|Xs],NotInt).

delete_lists_from_list_subset1([],_,[]).
delete_lists_from_list_subset1([Xs|Xss],Ys,NotInt):-
	( ord_subset(Ys,Xs) ->
	    NotInt1 = NotInt
	; NotInt = [Xs|NotInt1]
        ),
	delete_lists_from_list_subset1(Xss,Ys,NotInt1).

fr_put_indep([],_,Tail,Tail).
fr_put_indep([Xs|Xss],Ys,Pos,Tail):-
	fr_eliminate_subsets(Xss,Xs,NewXss,NewXs),
	( NewXs = [] ->
	    Pos1 = Pos
	;   compare_sets(NewXs,Ys,A,B),
	    Pos = [unlinked(A,B)|Pos1]
        ),
	fr_put_indep(NewXss,Ys,Pos1,Tail).

fr_eliminate_subsets([],Xs,[],Xs).
fr_eliminate_subsets([Ys|Xss],Xs,NewXss,NewXs):-
	ord_intersection(Xs,Ys,Intersect),
	( Intersect == Xs ->
	    NewXss = [Ys|Xss],
	    NewXs = []
	; ( Intersect == Ys ->
	       NewXss = NewXss1
	   ;  NewXss = [Ys|NewXss1]
           ),
	   fr_eliminate_subsets(Xss,Xs,NewXss1,NewXs)
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merging info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_info(global(Pos0,Neg0,Imp0),global(Pos1,Neg1,Imp1),Info):-
	merge_pos(Pos0,Pos1,Pos2),
	merge_neg(Neg0,Neg1,Neg2),
	merge(Imp0,Imp1,Imp2),
	close_info(global(Pos2,Neg2,Imp2),Info,_).

merge_pos([def(D1)|Pos0],[def(D2)|Pos1],Pos):- !,
	Pos = [def(D)|Pos2],
	merge(D1,D2,D),
	merge(Pos0,Pos1,Pos2).
merge_pos(Pos0,Pos1,Pos):-
	merge(Pos0,Pos1,Pos).

merge_neg(Pos0,Pos1,Pos):-
	merge(Pos0,Pos1,Pos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% closing info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close_info0((P->Q),Pos,Neg,NewPos,NewNeg,F0,F,Flag):-
	member_pos_each(P,Pos), !,  %% only thing changed
	close_info1(Q,Pos,Neg,TmpPos,TmpNeg,F0,F1,Flag1),
	( Flag1 = consistent ->
	    close_info2(Q,P,TmpPos,TmpNeg,NewPos,NewNeg,F1,F,Flag)
	; Flag = inconsistent).
close_info0((P->Q),Pos,Neg,NewPos,NewNeg,F0,F,Flag):- 
	close_info2(Q,P,Pos,Neg,NewPos,NewNeg,F0,F,Flag).

%% test for checking entailment of Q by either Pos or Neg
member_pos_or_neg(Q,Pos,_Neg):-    
	member_pos(Q,Pos),!.
member_pos_or_neg(Q,_Pos,Neg):- 
	member_neg(Q,Neg).

member_pos_each([],_).
member_pos_each([El|Rest],Pos):-
	member_pos(El,Pos),
	member_pos_each(Rest,Pos).

member_pos(def(D),[def(D2)|_]):- !, % we assume defs have been collapsed and sorted
	ord_subset(D,D2).
member_pos(unlinked(I1,I2),Pos):-
	member_pos_unlinked(Pos,I1,I2).

member_pos_unlinked([unlinked(I3,I4)|_],I1,I2):-
	ord_subset(I1,I3), % e.g. unlinked([1,2],[3]) entails unlinked([1],[3])
	ord_subset(I2,I4),!.
member_pos_unlinked([unlinked(I3,I4)|_],I1,I2):-
	ord_subset(I1,I4), % e.g. unlinked([1,3],[2]) entails unlinked([2],[3])
	ord_subset(I2,I3),!.
member_pos_unlinked([_|Pos],I1,I2):-
	member_pos_unlinked(Pos,I1,I2).

member_neg(not(def(D)),[not(def(D2))|Neg]):- !,
	( ord_subset(D2,D) ->
	    true
	; member_neg(not(def(D)),Neg)).
member_neg(not(unlinked(I1,I2)),Neg):-
	member_neg_unlinked(Neg,I1,I2).

member_neg_unlinked([not(unlinked(I3,I4))|_],I1,I2):-
	ord_subset(I3,I1), % e.g. not(unlinked([1],[2])) entails not(unlinked([1],[2,3]))
	ord_subset(I4,I2),!.
member_neg_unlinked([not(unlinked(I3,I4))|_],I1,I2):-
	ord_subset(I4,I1), % e.g. not(unlinked([2],[3])) entails not(unlinked([1,3],[2]))
	ord_subset(I3,I2),!.
member_neg_unlinked([_|Neg],I1,I2):-
	member_neg_unlinked(Neg,I1,I2).

%% Q is not entailed, so has to be added. We collaps defs
insert_pos_or_neg(not(X),Pos,Neg,NewPos,NewNeg):- !,
	insert_neg(Neg,not(X),NewNeg),
	NewPos = Pos.
insert_pos_or_neg(X,Pos,Neg,NewPos,Neg):- 
	insert_pos(Pos,X,NewPos).

insert_neg(Neg,X,NewNeg):- 
	insert(Neg,X,NewNeg).

insert_pos([def(D1)|Pos],def(D2),NewPos):- !,
	merge(D1,D2,D),
	NewPos = [def(D)|Pos].
insert_pos(Pos,X,NewPos):- 
	insert(Pos,X,NewPos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getting the initial conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

siap_condition([],_,conds([],[],[])):- !. 
siap_condition(_,[],conds([],[],[])):- !.
siap_condition(Xs,Ys,conds(Def,Unlinked,[])):-
	ord_intersection(Xs,Ys,Gvars),
	build_list_if_not_empty(Gvars,def(Gvars),Def,[]),
	ord_subtract(Xs,Gvars,Xvars),
	ord_subtract(Ys,Gvars,Yvars),
	build_unlinked_if_any(Xvars,Yvars,Unlinked,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simplifying the condition (only the last clause has been modified)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simplify_condition(false,Info,false,Info):- !.                       
simplify_condition(conds([],[],Neg),Info,conds([],[],Neg),Info):- !. 
simplify_condition(conds(Ground,Indep,_),Info,NewConds,NewInfo):-
	Info = global(_,Neg,_),
	append(Ground,Indep,Pos),
	inconsistent_lists(Pos,Neg), !,
	NewConds = false,
	NewInfo = Info.
simplify_condition(conds(Ground,Indep,Neg),Info,NewConds,NewInfo):-
	Info = global(Pos,_Neg,_Imp),
	simplify_ground(Ground,Pos,UpdatedGround),   % eliminates the entailed def vars
	simplify_unlinked(Indep,UpdatedGround,SimpleIndep1),
	simplify_unlinked(SimpleIndep1,Pos,SimpleIndep2_u), 
	sort(SimpleIndep2_u,SimpleIndep2),
	add_and_update(UpdatedGround,SimpleIndep2,Neg,Info,NewConds,NewInfo).

add_and_update(false,_Indep,_Neg,Info,false,Info):- !.
add_and_update([],Indep,Neg,Info,NewConds,NewInfo):-
	add_and_update1(Indep,Neg,Info,NewConds,NewInfo).
add_and_update([def([X|Xs])],Indep,Neg,Info,NewConds,NewInfo):-
	add_info(def([X]),Info,TmpInfo,Flag),
	( Flag = consistent ->
	    build_list_if_not_empty(Xs,def(Xs),RestDef,[]),
	    simplify_condition(conds(RestDef,Indep,Neg),TmpInfo,TmpConds,NewInfo),
	    add_and_update_g0(TmpConds,def([X]),NewConds)
	; NewConds = false).

simplify_ground([],_,[]).
simplify_ground([def(D1)],Pos,UpdatedGround):-
	( Pos = [def(D2)|_] ->
	    ord_subtract(D1,D2,D),
	    build_list_if_not_empty(D,def(D),UpdatedGround,[])
	; UpdatedGround = [def(D1)]
        ).

simplify_unlinked([],_,[]).
simplify_unlinked([unlinked(I1,I2)|Unl],Pos,UpdatedUnl):-
	merge(I1,I2,I),
	simplify_unlinked1(Pos,I1,I2,I,UpdatedUnl,Tail,NewUnl,Unl),
	simplify_unlinked(NewUnl,Pos,Tail).
	
simplify_unlinked1([],I1,I2,_,UpdatedUnl,Tail,T,T):-
	build_unlinked_if_any(I1,I2,UpdatedUnl,Tail).
simplify_unlinked1([def(D)|Pos],I1,I2,I,UpdatedUnl,Tail,NewUnl,TailU):-
	ord_subtract(I1,D,NI1),
	ord_subtract(I2,D,NI2),
	( (I1 = [];I2 = []) ->
	     UpdatedUnl = Tail,
	     NewUnl = TailU
	  ; ord_subtract(I,D,NI),
	    simplify_unlinked1(Pos,NI1,NI2,NI,UpdatedUnl,Tail,NewUnl,TailU)
        ).
simplify_unlinked1([unlinked(I3,I4)|_],I1,_,I,UpdatedUnl,Tail,NewUnl,TailU):-
	ord_intersection_diff(I,I3,Int,Disj),
	Int \== [], Disj \== [],
	ord_subset(Disj,I4),!,
	UpdatedUnl = Tail,
	ord_intersection_diff(Int,I1,NI1,NI2),
	build_unlinked_if_any(NI1,NI2,NewUnl,Tail1),
	ord_intersection_diff(Disj,I1,NNI1,NNI2),
	build_unlinked_if_any(NNI1,NNI2,Tail1,TailU).
simplify_unlinked1([unlinked(I3,I4)|_],I1,_,I,UpdatedUnl,Tail,NewUnl,TailU):-
	ord_intersection_diff(I,I4,Int,Disj),
	Int \== [], Disj \== [],
	ord_subset(Disj,I3),!,
	UpdatedUnl = Tail,
	ord_intersection_diff(Int,I1,NI1,NI2),
	build_unlinked_if_any(NI1,NI2,NewUnl,Tail1),
	ord_intersection_diff(Disj,I1,NNI1,NNI2),
	build_unlinked_if_any(NNI1,NNI2,Tail1,TailU).
simplify_unlinked1([_|Pos],I1,I2,I,UpdatedUnl,Tail,NewUnl,TailU):-
	simplify_unlinked1(Pos,I1,I2,I,UpdatedUnl,Tail,NewUnl,TailU).

build_unlinked_if_any([],_,Tail,Tail):- !.
build_unlinked_if_any(_,[],Tail,Tail):- !.
build_unlinked_if_any(I1,I2,[unlinked(A,B)|Tail],Tail):-
	compare_sets(I1,I2,A,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add_and_update
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_and_update_g0(false,_Fact,false).
add_and_update_g0(conds(Facts,Indep,Neg),Fact,conds(NewFacts,Indep,Neg)):-
	merge_pos([Fact],Facts,NewFacts).

add_and_update_i0(false,_Fact,_NewInfo,false).
add_and_update_i0(conds(Ground,Facts,Neg),Fact,global(Pos,_,Imp),NewConds):-
 	Fact = unlinked(A,B),
	( Pos = [def(NG)|_] ->
	    ord_intersection_diff(A,NG,InterA,NotInterA),
	    ord_intersection_diff(B,NG,InterB,NotInterB),
	    merge(InterA,InterB,Intersect),
	    ( Intersect = [] ->
		NewGround = Ground
	    ;  simplify_condition(conds([def(Intersect)],[],[]),global(Ground,[],Imp),conds(NewGround,_,_),_)
	    ),
	    build_unlinked_if_any(NotInterA,NotInterB,NewFacts,Facts),
	    NewConds = conds(NewGround,NewFacts,Neg)
	;   NewConds = conds(Ground,[Fact|Facts],Neg)
        ).


add_info(Fact,global(Pos,Neg,Imp),Info,Flag):-
	insert_pos(Pos,Fact,TmpPos),
	close_info(global(TmpPos,Neg,Imp),Info,Flag).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inconsistencies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% check inconsistency of a positive fact and a negative fact:

inconsistent(def(D1),not(def(D2))) :- !,
	ord_subset(D2,D1).
inconsistent(def(D),not(unlinked(I,_))) :-
	ord_subset(I,D),!.    % note that all vars in I must be in D
inconsistent(def(D),not(unlinked(_,I))) :-
	ord_subset(I,D),!.
inconsistent(unlinked(I1,I2),not(unlinked(I3,I4))) :-
	ord_subset(I3,I1),   % e.g. unlinked([1,2],[3]) not(unlinked([2],[3]))
	ord_subset(I4,I2),!.
inconsistent(unlinked(I1,I2),not(unlinked(I3,I4))) :-
	ord_subset(I4,I1),   % e.g. unlinked([1,3],[2,4]) not(unlinked([2,4],[3]))
	ord_subset(I3,I2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% building Goals to check conds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

goal_conditions(Conds,Info,count(0,1,0,G,I),Goal):-
	simplify_condition(Conds,Info,SimpConds,_),
	SimpConds=conds(Ground,Indep,_),
	goal_conditions_(Ground,Indep,Goal,G,I).

goal_conditions_([],[],true,0,0):- !.
goal_conditions_([],Indep,Goal,0,I):- !,
	goal_unlinked(Indep,Goal,0,I).
goal_conditions_([def(D)],[],def(D),G,0):- !,
	length(D,G).
goal_conditions_([def(D)],Indep,(def(D),Goal),G,I):-
	length(D,G),
	goal_unlinked(Indep,Goal,0,I).
	
goal_unlinked([unlinked(In1,In2)],Goal,I0,I):- !,
	Goal = unlinked(In1,In2),
	length(In1,A),
	length(In2,B),
	I is I0 + (A*B).
goal_unlinked([unlinked(In1,In2)|Indep],(unlinked(In1,In2),Goal),I0,I):-
	length(In1,A),
	length(In2,B),
	I1 is I0 + (A*B),
	goal_unlinked(Indep,Goal,I1,I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% grouping conditions NOTE DONE YET
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

group_indep1(_,B,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  THIS IS FOR SIAP_ANNOTATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_conds(conds(G1,I1,N1),conds(G2,I2,N2),conds(G,I,N)):-
	merge_pos(G1,G2,G),
	merge_pos(I1,I2,I),
	merge_neg(N1,N2,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traversing the nodes in reverse order looking for "false"
%  or creating the condition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partition_nodes1(Cond,Node,Nodes,_Dict,Left,RightVs,Right0,Right,RVx,Conds):- 
	Cond==false, !,
	cleanup_nodes([Node|Nodes],RightVs,Left),
	RightVs=[RVx|_],
	Right=Right0, Conds=conds([],[],[]).
partition_nodes1(_,Node,Nodes,Dict,Left,RightVs,Right0,Right,RVx,Conds):-
	dep_vertex_goal(Node,Dict,Vertex,Goal),
	varset(Goal,GoalVars),
	varset(Right0,Right0Vars),
	siap_condition(GoalVars,Right0Vars,Cond),
	partition_nodes0(Nodes,Dict,Left,[Vertex|RightVs],(Goal&Right0),Right,
	                 RVx,NConds),
	merge_conds(Cond,NConds,Conds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For a particular node, looking for an edge labelled "false"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

false_condition(Node,Cond):- sequential_dep_vertex(Node), !, Cond=false.
false_condition(dep(_,Deps),Cond):-
	false_condition_(Deps,conds([],[],[]),Cond).

false_condition_([],Conds,Conds).
false_condition_([to(_,false)|_Deps],_Conds,false):- !.
false_condition_([_|Deps],Conds0,Conds):-
	false_condition_(Deps,p(a,Conds0),Conds).


%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

