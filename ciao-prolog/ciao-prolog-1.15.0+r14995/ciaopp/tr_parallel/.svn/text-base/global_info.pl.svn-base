:- module(global_info,
	[
	    absint_nodes/7,
	    translate_nodes/3,
	    get_absint_info/4,
	    get_absint_info_nsiap_lp/5,
	    global_info/6
	],
	[]).

:- use_module(library(lists), [append/3]).

:- use_module(library(llists), [collect_singletons/2]).

:- use_module(library(sets), 
	[
	    merge/3,
	    ord_member/2,
	    ord_subset/2,
	    ord_subtract/3
	]).

:- use_module(infer(infer), [get_absint/4]).

:- use_module(plai(domains), 
	[
	    asub_to_info/5,
	    info_to_asub/5,
	    unknown_entry/3
	]).

:- use_module(domain(s_grshfr), 
	[
	    asubs_to_dep/3,
	    dep_to_indep/3,
	    ground_conds/3,
	    indep_conds_one_var/4,
	    not_ground_conds/3,
	    ground_imply_ground/4,
	    ground_imply_indep/5,
	    ground_imply_not_indep/4,
	    indep_imply_ground/4,
	    indep_imply_indep/5,
	    indep_imply_not_indep/4,
	    not_indep/4,
	    member_value_freeness/3
	]).

:- use_module(domain(fr_top), [get_free_vars/3]).

:- use_module(domain(fr_sets), [ss_minimise/2]).

:- use_module(domain(lsign), [lsign_global_info/5]).

:- use_module(domain(top_path_sharing), [path_to_shfr/3]).


%-----------------------------------------------------------------------------
% abs_int_to_global_info(+,+,+,-)
% abs_int_to_global_info(ASub,HvFv,AbsInt,GlobalInfo)
% abs_int_to_global_info/4 - builds "global" info for each abstract domain
% GlobalInfo in the output is closed
%-----------------------------------------------------------------------------

global_info(share,ASub,HvFv,Pos,[],Imp):-
	asubs_to_dep(ASub,Dep,NGVars),
	dep_to_indep(NGVars,Dep,Indep),
	ord_subtract(HvFv,NGVars,GVars),
	ground_conds(GVars,Pos,Pos1),
	Pos1 = Indep,
	ground_imply_ground(NGVars,ASub,Imp,Imp1),
	ground_imply_indep(NGVars,NGVars,ASub,Imp1,Imp2),
	indep_imply_ground(NGVars,ASub,Imp2,Imp3),
	indep_imply_indep(NGVars,NGVars,ASub,Imp3,[]).
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
global_info(son,(GVars,Sh),HvFv,Pos,[],[]):-
	ground_conds(GVars,Pos,Pos1),
	asubs_to_dep(Sh,Dep,_NGVars_bad),
	ord_subtract(HvFv,GVars,NGvars),
	dep_to_indep(NGvars,Dep,Pos1).
global_info(shareson,(_,Sh),HvFv,Pos,Neg,Imp):-
	global_info(share,Sh,HvFv,Pos,Neg,Imp).
global_info(shfrson,(_,ShFr),HvFv,Pos,Neg,Imp):-
	global_info(shfr,ShFr,HvFv,Pos,Neg,Imp).
global_info(path,PathSh,HvFv,Pos,Neg,Imp):-
	path_to_shfr(PathSh,Sh,Fr),
	global_info(shfr,(Sh,Fr),HvFv,Pos,Neg,Imp).
global_info(gr,Gr,HvFv,Pos,Neg,Imp):-
	asub_to_info(gr,Gr,HvFv,UserInfo,_),
	info_to_asub(shfr,_,UserInfo,HvFv,ShFr),
	global_info(shfr,ShFr,HvFv,Pos,Neg,Imp).
global_info(def,a(Ground,Set),_HvFv,Pos,[],Imp):-
	ground_conds(Ground,Pos,[]),
	def_get_impl(Set,Imp).
global_info(fr,as(Old,New),HvFv,Pos,Neg,[]):-
	merge(Old,New,TmpAbs),
	ss_minimise(TmpAbs,Abs), 
	collect_singletons(Abs,SingVars),
	ord_subtract(HvFv,SingVars,NonGround),
	not_ground_conds(NonGround,Neg,[]),  
	fr_indep(HvFv,Abs,Pos).
global_info(fd,(F,D),HvFv,Pos,Neg,Imp):-
	global_info(def,D,HvFv,PosD,_,Imp),
	D = a(G,_),
	ord_subtract(HvFv,G,NonGround),
	F = as(_G1,Sh1,_G2,Sh2),
	global_info(fr,as(Sh1,Sh2),NonGround,PosF,Neg,_),
	append(PosD,PosF,Pos).
global_info(lsign,Abs,HvFv,Pos,Neg,Imp):-
	lsign_global_info(Abs,HvFv,Pos,Neg,Imp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getting the implications for DEF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for each (X,SS) and each S=[Y1,..,Yn] in SS we have:
%     1/    [ground(Y1),...,ground(Yn)] -> ground(X)
%     2/    [indep(X,Y1),..,indep(X,Yn)] -> ground(X)
% The rule 2/ is useful when:
%    * if non(ground(X)) is entailed  (we obtain then
%      know that not(indep(X,Y)))
%    * when both ground(X) and indep(X,Y) have to be 
%      tested at run-time (we can avoid testing indep(X,Y))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

def_get_impl([],[]).
def_get_impl([(X,SS)|Set],Imp):-
	def_get_impl_each(SS,X,Imp,Tail),
	def_get_impl(Set,Tail).

def_get_impl_each([],_,Tail,Tail).
def_get_impl_each([S|SS],X,[(AllGround->ground(X)),(AllIndep->ground(X))|Imp],Tail):-
	ground_conds(S,AllGround,[]),
	indep_conds_one_var(S,X,AllIndep,[]),
	def_get_impl_each(SS,X,Imp,Tail).

/* OLD
%     2/    If S = [Y], then [indep(X,Y)] -> ground(X) good
def_get_impl_each([S|SS],X,[(AllGround->ground(X))|Imp],Tail):-
	ground_conds(S,AllGround,[]),
	( S = [Y] ->
	    sort([X,Y],[A,B]),
	    Imp = [([indep(A,B)] -> ground(X)) | Imp0]
	;   Imp = Imp0),
	def_get_impl_each(SS,X,Imp0,Tail).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getting the indep facts in Pos for Fr
% fr_indep(HvFv,Abs,Pos)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pos = [indep(x,y)|x,y in HvFv, x\neq y,{x,y},{x},{y} not Abs]
% Note that this is the same that saying that [x,y] not in the 
% closure under union of Abs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fr_indep([],_,[]).
fr_indep([X|HvFv],Abs,Pos):-
	fr_indep_each(HvFv,X,Abs,Pos,Tail),
	fr_indep(HvFv,Abs,Tail).

fr_indep_each([],_,_,Tail,Tail).
fr_indep_each([Y|HvFv],X,Abs,Pos,Tail):-
	( (ord_member([X,Y],Abs);ord_subset([[X],[Y]],Abs)) ->
	    Pos1 = Pos
	;   Pos = [indep(X,Y)|Pos1]
	),
	fr_indep_each(HvFv,X,Abs,Pos1,Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% domain-dependent operations for global info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_absint_info(_Key,_HvFv,none,top):- !.
get_absint_info(Key,HvFv,AbsInt,ASub):-
	get_absint(Key,AbsInt,HvFv,ASub), !.
get_absint_info(_Key,_HvFv,_AbsInt,'$bottom').

get_absint_info_nsiap_lp(Key,HvFv,_AVars,AbsInt,ASub):-
 	get_absint(Key,AbsInt,HvFv,ASub0), !,
	absint_info_to_shfr(AbsInt,ASub0,ASub).
get_absint_info_nsiap_lp(_Key,_HvFv,AVars,_AbsInt,ASub):-
	unknown_entry(shfr,AVars,ASub).

% nsiap conditions for lp are built by urlp/crlp
% urlp/crlp need substitutions in shfr format 

absint_info_to_shfr(shfr,ASub,ASub).
absint_info_to_shfr(shfrson,(_,ASub),ASub).
absint_info_to_shfr(path,ASub,(Sh,Fr)):-
	path_to_shfr(ASub,Sh,Fr).

% NSIAP conditions depend on each abstract domain

absint_nodes(shfr,CallP,CallQ,PVars,QVars,G1,G2):-
	urlp_node(CallP,PVars,G1,_,Exit),
	urlp_node(CallQ,QVars,G2,Exit,_).
absint_nodes(fr,Old,New,PVars,QVars,Vars,FreeVars):-
	merge(PVars,QVars,Vars),
        merge(Old,New,TmpAbs),
        ss_minimise(TmpAbs,Abs),
        get_free_vars(Vars,Abs,FreeVars).
absint_nodes(fd,Pre,Pos,PVars,QVars,NonGround,FreeVars):-
	merge(PVars,QVars,Vars),
        Pre = a(G,_),
        Pos = as(_,Old,_,New),
        merge(Old,New,TmpAbs),
        ss_minimise(TmpAbs,PosAbsF),
        ord_subtract(Vars,G,NonGround),
        get_free_vars(NonGround,PosAbsF,FreeVars).

urlp_node((Sh,Fr),Vars,node(_,Vars,Entry,Exit),Entry,Exit):-
% change shfr by mshare
        Entry = mshare(Sh,F),
        member_value_freeness(Fr,F,f).

% ----------------------------------------------------------------------

translate_nodes([], [], []).
translate_nodes([], [], _).
% change shfr by mshare
translate_nodes([node(Num, Vars, _, (mshare((B_sh, B_fr_0)),_))|Ns],
                [node(Num, Vars, Entry, Exit)|NNs], Entry) :-
% change shfr by mshare
        Entry = mshare(B_sh, B_fr),
        member_value_freeness(B_fr_0, B_fr, f),
        translate_nodes(Ns, NNs, Exit).
translate_nodes([node(Num, Vars, _, ((mshare((B_sh, B_fr_0)),_),_,_))|Ns],
                [node(Num, Vars, Entry, Exit)|NNs], Entry) :-
% change shfr by mshare
        Entry = mshare(B_sh, B_fr),
        member_value_freeness(B_fr_0, B_fr, f),
        translate_nodes(Ns, NNs, Exit).

% ----------------------------------------------------------------------

