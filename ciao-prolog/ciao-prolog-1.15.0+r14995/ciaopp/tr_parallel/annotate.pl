:- module(annotate,
	[
 	    annotate/8,
 	    is_udg/2,
	    thiscge/1,
 	    update_info/3,
 	    vertex/4,
 	    vertex_info/4,
 	    vertex_goal/3,
 	    vertex_goals/3,
 	    collapse_dep/4,
 	    sequential_dep_vertex/1,
 	    dep_vertex_goal/4,
 	    condition/7,
 	    goal_conditions/4,
 	    goal_conditions_/2,
 	    remove_ampersand/2,
 	    ground_imply_indep_simplified/3,
 	    inconsistent_conds/2,
 	    inconsistent_lists/2,
 	    inconsistent_neg/2,
 	    inconsistent_pos/2,
	    no_edge_if_no_cond/5
	],
	[]).


/*             Copyright (C)1990-94 UPM-CLIP				*/

%========================================================================
% Programmer: Kalyan Muthukumar
% Started   : 21st Nov 1989
% Programmer: Francisco Bueno
% Revised   : October 1993
%
% Single pre-process for all annotators
%========================================================================
% input:  internal format - plain syntax - with '\&'
% output: internal format (except for conditions for IAP) - full syntax 
%          - but no '\&'
%========================================================================

%------------------------------------------------------------------------%
%                    Meaning of the Program Variables                    %
%                                                                        %
%  AbsInt  : identifier of the abstract interpreter being used           %
%  Ann     : Identifier of the annotator being used                      %
%  Head    : Head of the clause being annotated                          %
%  Body    : Body of the clause being annotated                          %
%  Hv      : Head variables in the clause being considered               %
%  Fv      : Free variables in the body of the clause being considered   %
%  HvFv    : Variables in the clause being considered                    %
%  Vars    : Any possible set of variables                               %
%  ASub    : Abstract substitution                                       %
%  Clauses : List of program clauses                                     %
%  Flag    : Flag indicating if a clause should be annotated or not      %
%  Loc     : Flag indicating if the local analysis for a given clause    %
%            has been already performed or not                           %
%  Info    : Information about the characteristic of the clause variables%
%            at a given clause point. It has the structure:              %
%            Info = global(Pos,Neg,Imp) were Pos is the set of positive  %
%            facts (e.g. ground(X), indep(X,Y), etc), Neg is the set of  %
%            negative facts (e.g. not(ground(Y)), etc) and Imp is the    %
%            set of implications (e.g. [ground(X)] -> ground(Y)).        %
%  Dict    : Dictionary of goals in each program clause                  %
%  Count   : Counter for statistic pourposes. It has the structure:      %
%            count(C,E,UE,G,I) where E is the number of parallel exps.   %
%            UE is the subset of E which are unconditional, G is the     %
%            number of ground checks and I the number of independence    %
%            checks, and C the number of combinations for the exps.      %
%  Conds   : Conditions for IAP in the form conds(Ground,Indep,Negated)  %
%  Fact    : A simple (positive or negative) fact of ground/1 or indep/2 %
%------------------------------------------------------------------------%

:- use_package(assertions).

:- use_module(siap_annotate, 
	[
	    zero_count/1,
	    cgeannotate/7,
	    cdg0/3,
	    transitive_close/2,
	    udg/3,
	    add_count/3,
	    take_numbers_udg/2,
	    take_numbers_uudg/2,
	    take_numbers_tgudg/2,
	    take_numbers_cdg/2,
	    count_if_cge/2
	]).

:- use_module(nsiap_annotate, 
	[
	    urlp_annotate/5,
	    crlp_annotate/5,
%	    crlp_annotate_clp/5,
	    compute_indep0/4
	]).

:- use_module(uoudg, [exp_uoudg/5]).
:- use_module(uudg, [exp_uudg/5]).
:- use_module(disjwait, [exp_disjwait/5]).

:- use_module(tgudg, [exp_tgudg/5]).

:- use_module(global_info, 
	[
	    global_info/6,
	    get_absint_info/4,
	    get_absint_info_nsiap_lp/5,
	    absint_nodes/7
	]).

:- use_module(simplify, 
	[
	    simplify_condition/4,
	    close_info/2
	]).

:- use_module(library(lists), 
	[
	    append/3,
	    length/2
	]).

:- use_module(library(sets), 
	[
	    merge/3,
	    ord_intersection/3,
	    ord_member/2,
	    ord_subtract/3
	]).

:- use_module(library(terms_vars), [varset/2]).

:- use_module(program(clidlist), [first_key/2]).

:- use_module(library(sort)).

:- use_module(program(p_unit), 
	[
	    language/1,
	    type_of_goal/2
	]).

:- use_module(spec(s_simpspec), 
	[
	    list2body/2,
	    p_exp2list/2
	]).

:- use_module(domain(s_grshfr), 
	[
	    ground_conds/3,
	    indep_conds/4,
	    not_ground_conds/3
	]).

:- use_module(plai(domains), 
	[
	    info_to_asub/5,
	    unknown_entry/3
	]).

:- use_module(infer(infer), [get_info/5]).


:- op( 950,xfy, [(&),(\&)]).

:- data annotated_clauses/1,
	thiscge/1,
	lastgoal/1,
	local_info/3.

cleanup:-
	retractall_fact(annotated_clauses(_)),
	retractall_fact(thiscge(_)),
	retractall_fact(lastgoal(_)),
	retractall_fact(local_info(_,_,_)).

% -----------------------------------------------------------------------
% annotate(+,+,+,+,+,+,-,-)
% annotate(Clauses,Dicts,Ann,Mode,Local,AbsInt,AnnotatedClauses,NewDicts) 
% annotate/7 - entry point for all annotators
% This version uses structures constructed by body_with_info/6 and graph/2
% with all available info and conditions already built and simplified.
% It then calls either MEL or CDG or UDG or URLP or CRLP
% Flag is initialited to 'doit' (i.e. to annotate the clauses)
% -----------------------------------------------------------------------

annotate(Cls,Ds,Ann,Mode,Local,AbsInt,AnnCls,NewDs):-
	zero_count(Count0),
	cleanup,
	annotate5(Cls,Ds,Count0,Ann,Mode,Local,AbsInt,Count,AnnCls,NewDs,
                  Chs,doit),
	asserta_fact(annotated_clauses(Chs)),
	asserta_fact(thiscge(Count)).

annotate5([Cl:Id|Cls],[D|Ds],C0,Ann,Mode,Local,AbsInt,C,[NCl:Id|NCls],
          [ND|NDs],Chs,F):- 
	asserta_fact(lastgoal(Id),Ref),
	decide_annotate(Cl,D,C0,Ann,Mode,Local,AbsInt,C1,NCl1,ND1,F,F1),
	annotated_clause(C0,C1,Id,Chs,NChs,Cl,NCl1,NCl,D,ND1,ND),
	erase(Ref),
	annotate5(Cls,Ds,C1,Ann,Mode,Local,AbsInt,C,NCls,NDs,NChs,F1).
annotate5([],[],Count,_Ann,_Mode,_Local,_AbsInt,Count,[],[],[],_Flag).

annotated_clause(C0,C1,_Id,Chs,NChs,Cl,_,NCl,D,_,ND):-
	C0 = count(CDG,CGE,_,_,_),
	C1 = count(CDG,CGE,_,_,_),
	!,
	NChs=Chs,
	NCl = Cl,
	ND = D.
annotated_clause(C0,C1,_Id,Chs,NChs,_,NCl1,NCl,_,ND1,ND):-
	C0 = count(_,_,_,G,I),
	C1 = count(_,_,_,G,I),
	!,
	NChs=Chs,
	right_conjunctions(NCl1,NCl),
	ND = ND1.
annotated_clause(_,_,Id,[Id|Chs],Chs,_,NCl1,NCl,_,ND,ND):-
	right_conjunctions(NCl1,NCl).

right_conjunctions(clause(H,B),clause(H,NB)):-
	flatten_exp(B,FB),
	list2body(FB,NB).

flatten_exp((L,R),List):-
	flatten_exp(L,LL),
	flatten_exp(R,LR),
	append(LL,LR,List).
flatten_exp(Goal,[NewGoal]):-
	p_exp2list(Goal,NewGoal).

% -----------------------------------------------------------------------
% decide_annotate(+,+,+,+,+,+,+,-,-,-,+,-)
% decide_annotate(Clause,Dict,C,Ann,Mode,Local,AbsInt,C1,NewClause,NewDict,
%                 Flag,Flag1)
%   * If Clause = directive(parallelize) (means that the following clauses
%     until a directive(noparallelized) must be annotated), Flag1 is set
%     to 'doit'
%   * If Clause = directive(noparallelize) Flag1 is set to non
%   * If Clause is any other,  Flag1 = Flag (the previous value is valid)
% Also, if Clause is clause(Head,Body) and Body has either a single literal
% or a literal + a cut, no annotation is needed (nothing will run in parallel)
% -----------------------------------------------------------------------

decide_annotate(directive(G),D,C,_Ann,_Mode,_Local,_AbsInt,C,directive(G),D,
                Flag,Flag1):- 
	decide_annotate_directive(G,Flag,Flag1).
decide_annotate(clause(H,Body),D,C,_Ann,_Mode,_Local,_AbsInt,C1,Clause,Dict,
                Flag,Flag1):- 
	simple_body(Body),
	!,
	C1 = C,
	Clause = clause(H,Body),
	Dict = D,
	Flag1 = Flag.
decide_annotate(clause(H,B),D,C0,Ann,Mode,Local,AbsInt,C,clause(H,NB),
                ND,F,F):-
	( Local = local -> Loc0 = nloc ; Loc0 = loc ),
%	local_analysis_for_ann(Ann,Loc0),
	annotate_body(B,D,(H,B),C0,F,Mode,Loc0,Ann,AbsInt,F1,Loc1,C1,
                      B1,D1),
	annotate50(F1,B1,D1,(H,B),Mode,Loc1,C1,Ann,AbsInt,C,_,NB,ND).

decide_annotate_directive(parallelize,_Flag,Flag1):- !,	Flag1 = doit.
decide_annotate_directive(noparallelize,_Flag,Flag1):- !, Flag1 = non.
decide_annotate_directive(_D,Flag,Flag).

simple_body(!):- !.
simple_body(true):- !.
simple_body((_:_)):- !.
simple_body(((_:_),!)):- !.
simple_body(((_:_),(!:_))):- !.
simple_body((!,(_:_))):- !.
simple_body(((!:_),(_:_))).

/*
:- doc(local/1,
	"This predicate handles a flag that indicates whether to perform
	 or not local analysis of each clause.").
:- pred local(X) : var => ok_ans
	# "Mode for querying the current flag value" .
:- pred local(X) : {ground, ok_ans}
	# "Mode for setting the current flag value" .

local(X) :- do_local(Y), ok_local(X,Y), !.
local(X) :-
	ok_local(X,Y),
	retractall_fact(do_local(_)),
	asserta_fact(do_local(Y)).

:- data do_local/1.

%do_local(loc).
do_local(nloc).


ok_ans(loc).
ok_ans(nloc).

ok_local(no, loc).
ok_local(yes,nloc).

% for CLP language local analysis is not safe
% for LP language and nsiap it is not worth (ommitted in body_with_info)

local_analysis_for_ann(Ann,loc):- arg(1,Ann,nsiap), !.
local_analysis_for_ann(Ann,loc):- arg(1,Ann,siap), !.
%local_analysis_for_ann(_Ann,loc):- language(clp), !.
local_analysis_for_ann(_Ann,Flag):- do_local(Flag),!.
local_analysis_for_ann(_Ann,nloc).
*/

% -----------------------------------------------------------------------
% annotate_body(+,+,+,+,+,+,+,+,+,-,-,-,-,-)
% annotate_body(Body,Dict,HB,C0,F0,Mode,Loc0,Ann,AbsInt,F,Loc,C,NewBody,
%               NewDict)
% It traverses the body looking for special cases such as:
%    * A&B (the user has annotated the body, so do not try to annotate
%           it automatically)
%    * A\&B (the user has indicated where to annotate the body, so do it
%          only for the indicated goals) In this case the local analysis
%          must be executed but only once for each clause, thus a flag
%          is used.
% -----------------------------------------------------------------------
% In the two special cases '&' or '\&' we could call the annotator treating
% the subsexpressions as simple goals, however we cannot do this because
% the local analysis is not correct for this subexpressions, so Fout = non. 
% This may be corrected in further versions.

annotate_body((A,B),D,HB,C0,F0,Mode,Loc0,Ann,AbsInt,F,Loc,C,(NA,NBody),
              ND):- !,
	annotate_body0(A,B,D,HB,C0,F0,Mode,Loc0,Ann,AbsInt,F1,Loc1,C1,
                       NA,D1),
	annotate_body(B,D1,HB,C1,F1,Mode,Loc1,Ann,AbsInt,F,Loc,C,NBody,ND).
annotate_body(A,D,HB,C0,F0,Mode,Loc0,Ann,AbsInt,F,Loc,C,NBody,ND):-
	annotate_body0(A,last,D,HB,C0,F0,Mode,Loc0,Ann,AbsInt,F,Loc,C,
                       NBody,ND).

annotate_body0((A&B),_,D,_HB,Count,_F,_Mode,Loc,_Ann,_AbsInt,non,Loc,Count,
               (A&B),D).
annotate_body0((A:K),_,D,_HB,Count,F,_Mode,Loc,_Ann,_AbsInt,F,Loc,Count,
               (A:K),D).
annotate_body0((!),_,D,_HB,Count,F,_Mode,Loc,_Ann,_AbsInt,F,Loc,Count,
               (!),D).
annotate_body0((A\&B),R,D,HB,Count0,_F,Mode,Loc0,Ann,AbsInt,non,Loc,Count,
               Exp,NewD) :-
	( R=last ;
	    first_key(R,Key),
	    asserta_fact(lastgoal(Key),Ref)
	),
	sequentialize_par_exp((A\&B),TmpExp),
	annotate50(doit,TmpExp,D,HB,Mode,Loc0,Count0,Ann,AbsInt,Count,Loc,
                   Exp,NewD),
	( R=last ; erase(Ref) ).

sequentialize_par_exp((A\&B),(A,NewB)):- !,
	sequentialize_par_exp(B,NewB).
sequentialize_par_exp(A,A).

% -----------------------------------------------------------------------
% annotate50(+,+,+,+,+,+,+,+,+,-,-,-,-)
% annotate50(Flag,Body,Dict,HB,Mode,Loc0,Count0,Ann,AbsInt,Count,Loc,
%            NBody,NDict)
% Decides depending on the Flag if the Body should be annotated or not
% If so, it executes the local analysis (if necessary), collects the 
% information about the clause variables at each body point and calls
% the annotator chosen with a structure with all info collected.
% -----------------------------------------------------------------------

annotate50(non,Body,Dict,_HB,_Mode,Loc,Count,_Ann,_AbsInt,Count,Loc,
           Body,Dict).
annotate50(doit,Body,Dict,HB,Mode,Loc0,Count0,Ann,AbsInt,Count,Loc,
           NBody,NDict):-
	head_body_vars(HB,Dict,_Hv,Fv,HvFv),
	decide_local_analysis(Loc0,Mode,HB,Fv,HvFv,Ann,Loc),
	body_with_info(Body,HvFv,Ann,Mode,AbsInt,GoalDict,TmpBody),
	do_annotate(Ann,Mode,TmpBody,Dict,GoalDict,Count0,Count,NBody,
                    NDict).

head_body_vars((Head,_Body),dic(HvFv,_Names),Hv,Fv,HvFv):-
	varset(Head,Hv),
	ord_subtract(HvFv,Hv,Fv).

decide_local_analysis(nloc,Mode,(_Head,Body),Fv,HvFv,Ann,loc):-
	local_analysis(Ann,Body,Mode,HvFv,Fv).
decide_local_analysis(loc,_Mode,_HB,_Fv,_HvFv,_Ann,loc).

do_annotate(mel(Iap),Mode,Body,Dict,GoalDict,Count0,Count,N0Body,
	    NDict):-
        !,
	NDict=Dict,
	cgeannotate(Body,Mode,Iap,Count0,GoalDict,Count,N0Body).
%%	cgeannotate(Body,Mode,Iap,Count0,GoalDict,Count,NBody),
%%	group_indep1(mel(Iap),NBody,N0Body).
do_annotate(Ann,Mode,Body,Dict,GoalDict,Count0,Count,N0Body,NDict):-
	body_to_delta(Body,Dict,GoalDict,Mode,Ann,N0Body,NDict),
	take_numbers(Ann,N0Body,Count1),
%%	take_numbers(Ann,NBody,Count1),
%%	group_indep1(Ann,NBody,N0Body),
	add_count(Count0,Count1,Count).

take_numbers(cdg(_),Body,Count):- 
	take_numbers_cdg(Body,Count0),
	count_if_cge(Count0,Count).
take_numbers(udg(_),Body,Count):- 
	take_numbers_udg(Body,Count0),
	count_if_cge(Count0,Count).
take_numbers(uoudg(_),Body,Count):- 
	take_numbers_uudg(Body,Count0),
	count_if_cge(Count0,Count).
take_numbers(uudg(_),Body,Count):- 
	take_numbers_uudg(Body,Count0),
	count_if_cge(Count0,Count).
take_numbers(disjwait(_),Body,Count):- 
	take_numbers_uudg(Body,Count0),
	count_if_cge(Count0,Count).
take_numbers(tgudg(_),Body,Count):- 
	take_numbers_tgudg(Body,Count0),
	count_if_cge(Count0,Count).
take_numbers(urlp(_),Body,Count):-
	take_numbers_udg(Body,Count0),
	count_if_cge(Count0,Count).
take_numbers(crlp(_),_Body,count(1,1,0,0,0)). % This must be changed!!
take_numbers(_,_,count(0,0,0,0,0)).

%------------------------------------------------------------------------
% body_to_delta(+,+,+,+,+,-,-)
% body_to_delta(Nodes,Dict,GoalDict,As,Mode,Ann,ParExp,NewDict)
% do not try to parallelize until some goal worth parallelizing is found
% Nodes is a list following the order of goals in the original clause 
% body: sequential atoms (no difference between "low-grain" and seffs)
% are passed over, the rest of (parts of) the body is parallelized
%------------------------------------------------------------------------

body_to_delta([G],Dict,GoalDict,_Mode,_Ann,Goal,Dict):-
	atom_goal(G,GoalDict,Goal).
body_to_delta([Goal|Body],Dict,GDict,Mode,uoudg(X),NewBody,NewDict):-
	compute_par_exp(uoudg(X),Mode,[Goal|Body],Dict,GDict,NewBody,
                        NewDict).
body_to_delta([Goal|Body],Dict,GDict,Mode,uudg(X),NewBody,NewDict):-
	compute_par_exp(uudg(X),Mode,[Goal|Body],Dict,GDict,NewBody,
                        NewDict).
body_to_delta([Goal|Body],Dict,GDict,Mode,disjwait(X),NewBody,
              NewDict):-
	compute_par_exp(disjwait(X),Mode,[Goal|Body],Dict,GDict,
                        NewBody,NewDict).
body_to_delta([G|Body],Dict,GoalDict,Mode,Ann,NewBody,NewDict):-
	sequential_atom(G), !,
	NewBody = (Goal,Rest),
	atom_goal(G,GoalDict,Goal),
	body_to_delta(Body,Dict,GoalDict,Mode,Ann,Rest,NewDict).
body_to_delta([Goal|Body],Dict,GDict,Mode,Ann,NewBody,NewDict):-
	body_to_delta_1(Body,Dict,[Goal|Tail],Tail,GDict,Mode,Ann,NewBody,
                        NewDict).

body_to_delta_1([],Dict,Goals,[],GoalDict,Mode,Ann,NewBody,NewDict):-
	body_to_delta_2(Goals,Dict,GoalDict,Mode,Ann,NewBody,NewDict).
body_to_delta_1([Goal|Body],Dict,Goals,TGoals,GoalDict,Mode,Ann,NewBody,
                NewDict):-
%	seff_atom(Goal), !,  for now we treat builtins as seffs:
	sequential_atom(Goal), !,
	NewBody = (Par,Rest),
	TGoals = [],
	body_to_delta_2(Goals,Dict,GoalDict,Mode,Ann,Par,Dict1),
	body_to_delta([Goal|Body],Dict1,GoalDict,Mode,Ann,Rest,NewDict).
body_to_delta_1([Goal|Body],Dict,Goals,[Goal|TGoals],GD,Mode,Ann,
                NewBody,NewDict):-
	body_to_delta_1(Body,Dict,Goals,TGoals,GD,Mode,Ann,NewBody,NewDict).

body_to_delta_2([G],Dict,GoalDict,_Mode,_Ann,Goal,NewDict):- !,
	NewDict = Dict,
	atom_goal(G,GoalDict,Goal).
body_to_delta_2(Goals,Dict,GoalDict,Mode,Ann,Par,NewDict):-
	compute_par_exp(Ann,Mode,Goals,Dict,GoalDict,Par,NewDict).

compute_par_exp(udg(Iap),Mode,Goals,Dict,GoalDict,Exp,Dict):-
	compute_vertices(Goals,Vs),
	graph(Goals,Mode,Iap,Edges),
	cdg_to_udg(Edges,NEdges1),
	transitive_close((Vs,NEdges1),(Vs,NEdges2)),
	udg(graph(Vs,NEdges2),GoalDict,Exp).
compute_par_exp(uoudg(Iap),Mode,Goals,Dict,GoalDict,Exp,NewDict):-
	compute_vertices(Goals,Vs),
	graph(Goals,Mode,Iap,Edges),
	exp_uoudg(graph(Vs,Edges),Dict,GoalDict,Exp,NewDict).
compute_par_exp(uudg(Iap),Mode,Goals,Dict,GoalDict,Exp,NewDict):-
	compute_vertices(Goals,Vs),
	graph(Goals,Mode,Iap,Edges),
	exp_uudg(graph(Vs,Edges),Dict,GoalDict,Exp,NewDict).
compute_par_exp(disjwait(Iap),Mode,Gs,Dic,GDic,Exp,NewDic):-
	compute_vertices(Gs,Vs),
	graph(Gs,Mode,Iap,Edges),
	exp_disjwait(graph(Vs,Edges),Dic,GDic,Exp,NewDic).
compute_par_exp(tgudg(Iap),Mode,Goals,Dict,GoalDict,Exp,NewDict):-
	compute_vertices(Goals,Vs),
	graph(Goals,Mode,Iap,Edges),
	exp_tgudg(graph(Vs,Edges),Dict,GoalDict,Exp,NewDict).
compute_par_exp(cdg(Iap),Mode,Goals,Dict,GoalDict,Exp,Dict):-
	compute_vertices(Goals,Vs),
	graph(Goals,Mode,Iap,Edges),
	cdg0(graph(Vs,Edges),GoalDict,Exp).
compute_par_exp(urlp(_),_Mode,Goals,Dict,GoalDict,Exp,NewDict):-
	language(lp), !,
	urlp_annotate(Goals,Dict,GoalDict,Exp,NewDict).
%compute_par_exp(urlp(_),_Mode,Goals,Dict,GoalDict,Exp,NewDict):-
%	urlp_annotate_clp(Goals,Dict,GoalDict,Exp,NewDict).
compute_par_exp(crlp(_),_Mode,Goals,Dict,GoalDict,Exp,NewDict):-
 	language(lp), !,
 	crlp_annotate(Goals,Dict,GoalDict,Exp,NewDict).
%compute_par_exp(crlp(nsiap),_Mode,Goals,Dict,GoalDict,Exp,NewDict):-
%	crlp_annotate_clp(Goals,Dict,GoalDict,Exp,NewDict).

cdg_to_udg([edge(P,Q,_)|Rest],[(P,Q)|Rest1]) :-
	cdg_to_udg(Rest,Rest1).
cdg_to_udg([],[]).

is_udg([edge(P,Q,false)|Rest],[(P,Q)|Rest1]) :-
	is_udg(Rest,Rest1).
is_udg([],[]).

%-----------------------------------------------------------------------------
% local_analysis(+,+,+,+,+)
% local_analysis(Ann,Body,Mode,HvFv,Fv)
% local_analysis/5 - performs local analysis of the whole body of a clause 
% Info is recorded as local(Key,HvFv,Info) where Info is global/3 functor
% Note that local analysis only infers positive and negative facts,
% never implications - so we lose propagation of properties. For this,
% we could use pseudo-global analysis over Sh+Fr, but its too expensive!
%-----------------------------------------------------------------------------

local_analysis(Ann,Body,Mode,HvFv,Fv):-
	not_ground_conds(Fv,Neg,[]),
	indep_conds(Fv,HvFv,Pos_u,[]),
	sort(Pos_u,Pos),
	(
	    ( Ann = urlp(_) ; Ann = crlp(_) ) ->
	      local_analysis_(Body,Mode,Ann,Fv,_,HvFv,_,_)
	;
	    local_analysis_(Body,Mode,Ann,Fv,_,HvFv,
                            global(Pos,Neg,[]),_)
	).

local_analysis_((A,B),Mode,Ann,Frees0,Frees,HvFv,Info,Info1):-
	local_analysis_(A,Mode,Ann,Frees0,Frees1,HvFv,Info,Info0),
	local_analysis_(B,Mode,Ann,Frees1,Frees,HvFv,Info0,Info1).
local_analysis_((A&B),Mode,Ann,Frees0,Frees,HvFv,Info,Info1):-
	local_analysis_(A,Mode,Ann,Frees0,Frees1,HvFv,Info,Info0),
	local_analysis_(B,Mode,Ann,Frees1,Frees,HvFv,Info0,Info1).
local_analysis_((A\&B),Mode,Ann,Frees0,Frees,HvFv,Info,Info1):-
	local_analysis_(A,Mode,Ann,Frees0,Frees1,HvFv,Info,Info0),
	local_analysis_(B,Mode,Ann,Frees1,Frees,HvFv,Info0,Info1).
local_analysis_((A:K),Mode,Ann,Frees0,Frees,HvFv,Info0,InfoOut):-
	varset(A,Av),
	insert_local_info(A,Av,Mode,Ann,Info0,Info),
	(
	    ( Ann = urlp(_) ; Ann = crlp(_) ) ->
	      asserta_fact(local_info(K,HvFv,(mshare(Info),_)))
	;
	    asserta_fact(local_info(K,HvFv,Info)),
	    propagate_local_info(A,Av,Mode,Info,Ann,Frees0,Frees,InfoOut)
	).
local_analysis_((!),_Mode,_Ann,Frees,Frees,_HvFv,Info,Info).

%-----------------------------------------------------------------------------
% insert_local_info(+,+,+,+,+,-)
% insert_local_info(Sg,Sv,Mode,Ann,Info0,Info)
% It updates Info0 (the already known information at the entry of Sg) with the
% additional information from directives or builtins which is also valid at
% the entry of Sg (i.e. InfoBefore)
%-----------------------------------------------------------------------------

insert_local_info(Sg,Sv,Mode,Ann,Info0,Info):-
	local_info(Ann,Mode,Sg,Sv,InfoBefore,entry), !,
	(
	    ( Ann = urlp(_) ; Ann = crlp(_) ) ->
	      Info = InfoBefore
	;
	    update_info(Info0,InfoBefore,Info)
	).
insert_local_info(_Sg,_Sv,_Mode,_Ann,Info,Info).

%-----------------------------------------------------------------------------
% propagate_local_info(+,+,+,+,+,+,-,-)
% propagate_local_info(Sg,Sv,Mode,Info,Ann,Frees0,Frees,InfoOut)
% To be safe, take out any fact regarding the variables involved in Sg,
% unless they refer to other variables already in Frees (which is Frees0
% minus the variables of Sg)
% Then, if Sg is a builtin or we have a directive, take it and update Info
%-----------------------------------------------------------------------------

propagate_local_info(Sg,Sv,Mode,Info,Ann,Frees0,Frees,InfoOut):-
	ord_subtract(Frees0,Sv,Frees),
	take_out_facts(Info,Sv,Frees,TmpInfo),
	propagate_local_info0(Sg,Sv,Mode,Ann,TmpInfo,InfoOut).

propagate_local_info0(Sg,Sv,Mode,Ann,TmpInfo,InfoOut):-
	local_info(Ann,Mode,Sg,Sv,InfoAfter,exit), !,
	update_info(TmpInfo,InfoAfter,InfoOut).
propagate_local_info0(_Sg,_Sv,_Mode,_Ann,Info,Info).

take_out_facts(global(Pos,Neg,Imp),Vars,Frees,global(NewPos,NewNeg,Imp)):-
	take_out_facts0(Pos,Vars,Frees,NewPos),
	take_out_facts0(Neg,Vars,Frees,NewNeg).

take_out_facts0([],_Vars,_Frees,[]).
take_out_facts0([not(ground(X))|Info],Vars,Frees,InfoOut):-
	ord_member(X,Vars), !,
	take_out_facts0(Info,Vars,Frees,InfoOut).
take_out_facts0([indep(X,Y)|Info],Vars,Frees,InfoOut):-
	ord_member(X,Vars), !,
	take_out_facts1(Frees,Y,indep(X,Y),InfoOut,RestInfoOut),
	take_out_facts0(Info,Vars,Frees,RestInfoOut).
take_out_facts0([indep(X,Y)|Info],Vars,Frees,InfoOut):-
	ord_member(Y,Vars), !,
	take_out_facts1(Frees,X,indep(X,Y),InfoOut,RestInfoOut),
	take_out_facts0(Info,Vars,Frees,RestInfoOut).
take_out_facts0([I|Info],Vars,Frees,[I|InfoOut]):-
	take_out_facts0(Info,Vars,Frees,InfoOut).

take_out_facts1(Frees,Y,I,[I|RestInfoOut],RestInfoOut):-
	ord_member(Y,Frees), !.
take_out_facts1(_Frees,_,_I,InfoOut,InfoOut).

%-----------------------------------------------------------------------------
% local_info(+,+,+,+,-,+)
% local_info(Sg,Mode,Sv,Info,Type)
% Get info valid at entry or exit (depending on Type - entry/exit) of Sg from 
% (trust) directives or builtins. If there is none, the call fails.
%-----------------------------------------------------------------------------

local_info(urlp(_),Mode,Sg,Sv,ASub,Type):-
	get_info(trusted,pred,Type,Sg,Info0),
	info_to_asub(Mode,_Kind,Info0,Sv,ASub).
local_info(crlp(_),Mode,Sg,Sv,ASub,Type):-
	get_info(trusted,pred,Type,Sg,Info0),
	info_to_asub(Mode,_Kind,Info0,Sv,ASub).
local_info(_Ann,_Mode,Sg,_Sv,Info,Type):-
	functor(Sg,F,A),
	functor(Copy,F,A),
	type_of_goal(builtin(Builtin),Copy),
	builtin_mode(Builtin,Type), !,
	args_info(0,A,Copy,Sg,Info,global([],[],[])).
local_info(_Ann,Mode,Sg,Sv,Info,Type):-
	get_info(trusted,pred,Type,Sg,Info0),
	info_to_asub(Mode,_Kind,Info0,Sv,ASub),
	abs_int_to_global_info(ASub,_HFv,Mode,Info).

args_info(N,N,_Term1,_Term2,Info,Info):- !.
args_info(N1,N,Term1,Term2,Info,Tail):- 
	N2 is N1+1,
	arg(N2,Term1,Arg1),
	arg(N2,Term2,Arg2),
	varset(Arg2,Vars),
	arg_info(Arg1,Vars,Info,NewInfo),
	args_info(N2,N,Term1,Term2,NewInfo,Tail).

arg_info(g,Vars,global(Pos,Neg,Imp),global(TailPos,Neg,Imp)):-
	ground_conds(Vars,Pos,TailPos).
arg_info(f,Vars,global(Pos,Neg,Imp),global(Pos,TailNeg,Imp)):-
	not_ground_conds(Vars,Neg,TailNeg).
arg_info(?,_Vars,Info,Info).

%----------------------------------------------------------------------------
% builtin_omode/1 - the table of builtins info
%----------------------------------------------------------------------------

% builtin_omode(G):- builtin_mode(G,(exit)).

builtin_mode('abolish'(g,g),(exit)).
builtin_mode('absolute_file_name'(g,g),(exit)).
builtin_mode('arg'(g,?,?),(exit)).
builtin_mode('assert'(?,g),(exit)).
builtin_mode('asserta'(?,g),(exit)).
builtin_mode('assertz'(?,g),(exit)).
builtin_mode('atom'(g),(exit)).
builtin_mode('atomic'(g),(exit)).
builtin_mode('character_count'(g,g),(exit)).
builtin_mode('clause'(?,?,g),(exit)).
builtin_mode('close'(g),(exit)).
builtin_mode('compare'(g,?,?),(exit)).
builtin_mode('compile'(g),(exit)).
builtin_mode('consult'(g),(exit)).
builtin_mode('current_atom'(g),(exit)).
builtin_mode('current_input'(g),(exit)).
builtin_mode('current_key'(g,?),(exit)).
builtin_mode('current_op'(g,g,g),(exit)).
builtin_mode('current_output'(g),(exit)).
builtin_mode('current_predicate'(g,?),(exit)).
builtin_mode('current_stream'(g,g,g),(exit)).
builtin_mode('depth'(g),(exit)).
builtin_mode('ensure_loaded'(g),(exit)).
builtin_mode('erase'(g),(exit)).
builtin_mode('float'(g),(exit)).
builtin_mode('flush_output'(g),(exit)).
builtin_mode('foreign'(g,g,g),(exit)).
builtin_mode('foreign_file'(g,g),(exit)).
builtin_mode('format'(g,?),(exit)).
builtin_mode('format'(g,g,?),(exit)).
builtin_mode('functor'(?,g,g),(exit)).
builtin_mode('gcguide'(g,g,g),(exit)).
builtin_mode('get'(g),(exit)).
builtin_mode('get'(g,g),(exit)).
builtin_mode('get0'(g),(exit)).
builtin_mode('get0'(g,g),(exit)).
builtin_mode('help'(g),(exit)).
builtin_mode('instance'(g,?),(exit)).
builtin_mode('integer'(g),(exit)).
builtin_mode('is'(g,g),(exit)).
builtin_mode('leash'(g),(exit)).
builtin_mode('length'(?,g),(exit)).
builtin_mode('library_directory'(g),(exit)).
builtin_mode('line_count'(g,g),(exit)).
builtin_mode('line_position'(g,g),(exit)).
builtin_mode('lisp_apply'(g,g,g),(exit)).
builtin_mode('listing'(g),(exit)).
builtin_mode('load_foreign_files'(g,g),(exit)).
builtin_mode('manual'(g),(exit)).
builtin_mode('maxdepth'(g),(exit)).
builtin_mode('name'(g,g),(exit)).

builtin_mode('ground'(g),(exit)).      %%  Added by MCL
builtin_mode('indep'(?, ?),(exit)).    %%  12 - 91
builtin_mode('indep'(?),(exit)).       %%

builtin_mode('nl'(g),(exit)).
builtin_mode('no_style_check'(g),(exit)).
builtin_mode('nospy'(g),(exit)).
builtin_mode('nonvar'(?),(exit)).
builtin_mode('number'(g),(exit)).
builtin_mode('numbervars'(g,g,g),(exit)).
builtin_mode('op'(g,g,g),(exit)).
builtin_mode('open'(g,g,g),(exit)).
builtin_mode('open_null_stream'(g),(exit)).
builtin_mode('predicate_property'(?,g),(exit)).
builtin_mode('print'(g,?),(exit)).
builtin_mode('prolog_flag'(g,g,g),(exit)).
builtin_mode('prompt'(g,g),(exit)).
builtin_mode('put'(g),(exit)).
builtin_mode('put'(g,g),(exit)).
builtin_mode('read'(g,?),(exit)).
builtin_mode('recorda'(?,?,g),(exit)).
builtin_mode('recorded'(?,?,g),(exit)).
builtin_mode('recordz'(?,?,g),(exit)).
builtin_mode('restore'(g),(exit)).
builtin_mode('save'(g),(exit)).
builtin_mode('save'(g,g),(exit)).
builtin_mode('save_program'(g),(exit)).
builtin_mode('see'(g),(exit)).
builtin_mode('seeing'(g),(exit)).
builtin_mode('set_input'(g),(exit)).
builtin_mode('set_output'(g),(exit)).
builtin_mode('skip'(g),(exit)).
builtin_mode('skip'(g,g),(exit)).
builtin_mode('source_file'(g),(exit)).
builtin_mode('source_file'(?,g),(exit)).
builtin_mode('spy'(g),(exit)).
builtin_mode('statistics'(g,g),(exit)).
builtin_mode('stream_code'(g,g),(exit)).
builtin_mode('stream_position'(g,g,g),(exit)).
builtin_mode('style_check'(g),(exit)).
builtin_mode('tab'(g),(exit)).
builtin_mode('tab'(g,g),(exit)).
builtin_mode('tell'(g),(exit)).
builtin_mode('telling'(g),(exit)).
builtin_mode('ttyget'(g),(exit)).
builtin_mode('ttyget1'(g),(exit)).
builtin_mode('ttyput'(g),(exit)).
builtin_mode('ttyskip'(g),(exit)).
builtin_mode('ttytab'(g),(exit)).
builtin_mode('unix'(g),(exit)).
builtin_mode('unknown'(g,g),(exit)).
builtin_mode('var'(f),(exit)).
builtin_mode('version'(g),(exit)).
builtin_mode('vms'(g),(exit)).
builtin_mode('write'(g,?),(exit)).
builtin_mode('write_canonical'(g,?),(exit)).
builtin_mode('writeq'(g,?),(exit)).
builtin_mode('<'(g,g),(exit)):- language(lp).
builtin_mode('=<'(g,g),(exit)):- language(lp).
builtin_mode('>'(g,g),(exit)):- language(lp).
builtin_mode('>='(g,g),(exit)):- language(lp).
builtin_mode('=:='(g,g),(exit)).
% SICStus3 (ISO)
builtin_mode('=\\='(g,g),(exit)).
% SICStus2.x
% builtin_mode('=\='(g,g),(exit)).
%%% FOR CLP
builtin_mode('ctime'(g),(exit)):- language(clp).
builtin_mode('floor'(g,g),(exit)):- language(clp).
builtin_mode('inf'(g,g),(exit)):- language(clp).

%-----------------------------------------------------------------------------
% body_with_info(+,+,+,+,+,-,-)
% body_with_info(Body,HvFv,Ann,Mode,AbsInt,Dict,NewBody)
% body_with_info/7 - transform body and gather all available info
% A body (Goal,...) is transformed into a list [node(Id,Vars,Grain,Info)|...]
% Info is a pair of the local and the global analysis info as:
% If siap, Info is global/3 (or false/0)
% If nsiap, then Info is (Call,Succ) with Succ free for lp
% If both, Info is (CallInfo,Call,Succ) with CallInfo global/3 term
% corresponding to Call, and Succ free for lp
% Call and Succ (if not free) are wrapped around a functor made with AbsInt
% If siap and lp, then it is always (shfr(Call),_)
% Grain is granularity info, nodes are renamed without duplicates, and
% names are collected in a dictionary, so that Goal can be recovered from Id
% For the last atom of the clause, no global Info is required 
%-----------------------------------------------------------------------------
% If atom is not going to be parallelized, we could avoid getting the info
%  unless this info is useful for another goal worth parallelizing!

body_with_info(Body,HvFv,Ann,Mode,AbsInt,Dict,NBody):-
	Ann =.. [_Anno|[Iap]],
	language(Lang),
	body_with_info_(Body,HvFv,Lang,Ann,Mode,Iap,_,AbsInt,0,DictArgs,NBody),
	Dict =.. [dict|DictArgs].

body_with_info_((A,B),HvFv,Lang,Ann,Mode,Iap,PA,AbsInt,N,[A|Sgs],[NA|NB]):- !,
	atom_info(A,Ann,HvFv,Lang,Mode,Iap,PA,PB,AbsInt,N,NN,NA),
	body_with_info_(B,HvFv,Lang,Ann,Mode,Iap,PB,AbsInt,NN,Sgs,NB).
body_with_info_(A,HvFv,Lang,Ann,Mode,Iap,PA,AbsInt,N,[A],[NA]):-
	atom_info_last(A,HvFv,Lang,Ann,Mode,Iap,PA,AbsInt,N,NA).

atom_info((A:K),urlp(_),HvFv,Lang,_Mode,Iap,PA,PB,AbsInt,N,NN,
          node(NN,AVars,Grain,Info)):-
	varset(A,AVars),
	some_global_info(Iap,Lang,PA,PB,urlp(_),AbsInt,AVars,K,HvFv,Info),
	granul_info(A,Grain),
	NN is N+1.
atom_info((A:K),crlp(_),HvFv,Lang,_Mode,Iap,PA,PB,AbsInt,N,NN,
          node(NN,AVars,Grain,Info)):-
	varset(A,AVars),
	some_global_info(Iap,Lang,PA,PB,crlp(_),AbsInt,AVars,K,
                         HvFv,Info),
	granul_info(A,Grain),
	NN is N+1.
atom_info((!),_Ann,_HvFv,Lang,Mode,Iap,PA,PA,_AbsInt,N,NN,
          node(NN,[],Grain,Info)):-
	null_global_info(Iap,Lang,Mode,[],Info),
	granul_info((!),Grain),
	NN is N+1.
atom_info((A:K),Ann,HvFv,Lang,_Mode,Iap,PA,PB,AbsInt,N,NN,
          node(NN,AVars,Grain,Info)):-
	varset(A,AVars),
	some_global_info(Iap,Lang,PA,PB,Ann,AbsInt,AVars,K,
                         HvFv,Info),
	granul_info(A,Grain),
	NN is N+1.

atom_info_last((A:K),urlp(_),HvFv,Lang,Mode,Iap,PA,AbsInt,N,
               node(NN,AVars,Grain,Info)):-
	varset(A,AVars),
	last_global_info(Iap,urlp(_),Mode,Lang,PA,AbsInt,K,HvFv,AVars,
                         Info),
	granul_info(A,Grain),
	NN is N+1.
atom_info_last((A:K),crlp(_),HvFv,Lang,Mode,Iap,PA,AbsInt,N,
               node(NN,AVars,Grain,Info)):-
	varset(A,AVars),
	last_global_info(Iap,crlp(_),Mode,Lang,PA,AbsInt,K,HvFv,AVars,
                         Info),
	granul_info(A,Grain),
	NN is N+1.
atom_info_last((!),_HvFv,Lang,_Ann,Mode,Iap,Info,_AbsInt,N,
               node(NN,[],Grain,Info)):-
	null_global_info(Iap,Lang,Mode,[],Info),
	granul_info((!),Grain),
	NN is N+1.
atom_info_last((A:K),HvFv,Lang,Ann,Mode,Iap,PA,AbsInt,N,
               node(NN,AVars,Grain,Info)):-
	varset(A,AVars),
	last_global_info(Iap,Ann,Mode,Lang,PA,AbsInt,K,HvFv,AVars,Info),
	granul_info(A,Grain),
	NN is N+1.

null_global_info(nsiap,lp,Mode,Qv,Info):- unknown_entry(Mode,Qv,Info).
null_global_info(nsiap,clp,_Mode,_Qv,top).
null_global_info(siap,_,_,_,global([],[],[])).
null_global_info(both,lp,_,_,(global([],[],[]),top,top)).
null_global_info(both,clp,_,_,(global([],[],[]),top,top)).

last_global_info(siap,urlp(_),_,_Lang,PA,AbsInt,K,HvFv,AVars,Info):- !,
	some_global_info(siap,lp,PA,_,urlp(_),AbsInt,AVars,K,HvFv,Info).
last_global_info(siap,crlp(_),_,_Lang,PA,AbsInt,K,HvFv,AVars,Info):- !,
	some_global_info(siap,lp,PA,_,crlp(_),AbsInt,AVars,K,HvFv,Info).
last_global_info(siap,_Ann,Mode,Lang,_PA,_AbsInt,_K,_HvFv,AVars,
                 Info):-
        !,
	null_global_info(siap,Lang,Mode,AVars,Info).
last_global_info(Iap,Ann,_,lp,PA,AbsInt,K,HvFv,AVars,Info):-
	some_global_info(Iap,lp,PA,_,Ann,AbsInt,AVars,K,HvFv,Info).
last_global_info(Iap,Ann,_,clp,PA,AbsInt,K,HvFv,AVars,Info):-
	some_global_info(Iap,clp,PA,PB,Ann,AbsInt,AVars,K,HvFv,Info),
	get_last_global_info(K,HvFv,Ann,AbsInt,PB).

some_global_info(siap,_lp,_PA,_PB,Ann,AbsInt,_,K,HvFv,Info):-
	get_local_info(Ann,K,HvFv,LocalInfo),
	get_absint_info(K,HvFv,AbsInt,ASub),
	(
	    ( Ann = urlp(_) ; Ann = crlp(_) ) ->
	      Info = LocalInfo
	;
	    abs_int_to_global_info(ASub,HvFv,AbsInt,AiInfo),
	    update_info(LocalInfo,AiInfo,Info)
	).
%% some_global_info(siap,clp,_PA,_PB,AbsInt,_,K,HvFv,Info):-
%% 	get_absint_info(K,HvFv,AbsInt,ASub),
%% 	abs_int_to_global_info(ASub,HvFv,AbsInt,Info).

% change shfr by mshare
some_global_info(nsiap,lp,Call,_PB,_Ann,AbsInt,AVars,K,HvFv,(mshare(Call),_)):-
	get_absint_info_nsiap_lp(K,HvFv,AVars,AbsInt,Call).
some_global_info(nsiap,clp,Call,Succ,_Ann,AbsInt,_,K,HvFv,(Call,Succ)):-
	get_absint_info(K,HvFv,AbsInt,ASub),
	Call=..[AbsInt|[ASub]].
some_global_info(both,lp,Call,_PB,Ann,AbsInt,_,K,HvFv,(Info,Call,_)):-
	get_local_info(Ann,K,HvFv,LocalInfo),
	get_absint_info(K,HvFv,AbsInt,ASub),
	Call=..[AbsInt|[ASub]],
	(
	    ( Ann = urlp(_) ; Ann = crlp(_) ) ->
	      Info = LocalInfo
	;
	    abs_int_to_global_info(ASub,HvFv,AbsInt,AiInfo),
	    update_info(LocalInfo,AiInfo,Info)
	).
some_global_info(both,clp,Call,Succ,_Ann,AbsInt,_,K,HvFv,(Info,Call,Succ)):-
	get_absint_info(K,HvFv,AbsInt,ASub),
	Call=..[AbsInt|[ASub]],
	abs_int_to_global_info(ASub,HvFv,AbsInt,Info).

get_last_global_info(_BadK,HvFv,_Ann,AbsInt,ASub):-
	current_fact(lastgoal(K)),
	get_absint_info(K,HvFv,AbsInt,ASub).

get_local_info(Ann,Key,HvFv,Info):-
	current_fact(local_info(Key,HvFv,TmpInfo)), !,
	(
	    ( Ann = urlp(_) ; Ann = crlp(_) ) ->
	      Info = TmpInfo
	;
	    sort_info(TmpInfo,Info)
	).
get_local_info(_Ann,_Key,_HvFv,global([],[],[])).

sort_info(global(Pos,Neg,Imp),global(Pos_s,Neg_s,Imp_s)):-
	sort(Pos,Pos_s),
	sort(Neg,Neg_s),
	sort(Imp,Imp_s).
sort_info((mshare(Pos,Neg),_),(mshare(Pos_s,Neg_s),_)):-
	sort(Pos,Pos_s),
	sort(Neg,Neg_s).

% transform a "node" into its original goal

atom_goal(node(N,_,_,_),Dict,Goal):- arg(N,Dict,Goal).

%-----------------------------------------------------------------------------
% abs_int_to_global_info(+,+,+,-)
% abs_int_to_global_info(ASub,HvFv,AbsInt,GlobalInfo)
% abs_int_to_global_info/4 - builds "global" info for each abstract domain
% GlobalInfo in the output is closed
%-----------------------------------------------------------------------------

abs_int_to_global_info(top,_,_,global([],[],[])):- !.
abs_int_to_global_info('$bottom',_,_,global([],[],[])):- !.
abs_int_to_global_info(('$bottom','$bottom'),_,_,global([],[],[])):- !.
%% abs_int_to_global_info('$bottom',_,_,false):- !.
%% abs_int_to_global_info(('$bottom','$bottom'),_,_,false):- !.
abs_int_to_global_info(ASub,HvFv,AbsInt,GlobalInfo):-
	global_info(AbsInt,ASub,HvFv,Pos,Neg,Imp), !,
	sort_info(global(Pos,Neg,Imp),TmpInfo),
	close_info(TmpInfo,GlobalInfo).

%-----------------------------------------------------------------------------
% granul_info(+,-)
% granul_info(Sg,Grain)
% very very naive granularity info (and side-effects) for a single goal
% Grain=0 for builtins, Grain=-1 for seff and Grain=1 for any other goal
% Meta-calls inherit granularity of the called goal
%-----------------------------------------------------------------------------

granul_info(Sg,Grain):- var(Sg), !, Grain = -1.
granul_info(!,Grain):- !, Grain = -1.
granul_info(Sg,Grain):-
	get_info(seff,pred,_Key,Sg,pure),
	!,
	( type_of_goal(builtin(Type),Sg),
	  small(Type)
	-> Grain = 0
	 ; Grain = -1  % it is a side-effect
	).
granul_info(_,1).

small(_). % pending

% an atom is not worth parallelizing if Grain<1
sequential_atom(node(_,_,N,_)):- N<1, !.
sequential_atom(node(_,_,_,Info)):- bottom(Info).

bottom(false).
% change shfr by mshare
bottom((mshare('$bottom'),_)).
bottom((mshare([]),_)).
bottom((false,_,_)).

% an atom is side-effect if Grain=-1

% seff_atom(node(_,_,N,_)):- N<0.

%------------------------------------------------------------------------
% update_info(Info0,Info1,Info)
% update_info(+,+,-)
% update_info/3 - update the "global" info Info0 with new Info1
% If something in Info0 is not consistent with something in Info1, we
% discard that of Info0 - because it can only happen for "downward closed"
% information, like if not(ground(X))\in Info0 and ground(X)\in Info1
% Each remaining fact of Info0 is then added to Info1 and closed, 
% yielding Info.
%------------------------------------------------------------------------

update_info(global(Pos0,Neg0,Imp0),Info1,Info):-
	Info1 = global(Pos1,Neg1,_Imp1), !,
	not_inconsistent(Pos0,Neg1,Pos2),
	not_inconsistent(Neg0,Pos1,Neg2),
	merge_info(global(Pos2,Neg2,Imp0),Info1,Info).
update_info(_Info0,_Info1,false).

not_inconsistent([],_,[]).
not_inconsistent([C|Cs],Info,NewCs):-
	inconsistent_dont_know(C,Info), !,
	not_inconsistent(Cs,Info,NewCs).
not_inconsistent([C|Cs],Info,[C|NewCs]):-
	not_inconsistent(Cs,Info,NewCs).

% here we consider that adding a new fact cannot make the whole info
% inconsistent, is this true?
% both infos must be already closed!

merge_info(global(Pos0,Neg0,Imp0),global(Pos1,Neg1,Imp1),Info):-
	merge(Pos0,Pos1,Pos2),
	merge(Neg0,Neg1,Neg2),
	merge(Imp0,Imp1,Imp2),
	close_info(global(Pos2,Neg2,Imp2),Info).

%-----------------------------------------------------------------------------
% graph(+,+,+,-)
% graph(Nodes,Mode,Iap,CDG)
% graph/4 - makes a cdg
% Nodes is a list [node(Id,Vars,Grain,Info)|...]
% CDG is a list [edge(Vertex1,Vertex2,[Cond|...])|...]
% for a dependency of Vertex2 on Vertex1 labelled with conditions
% Conditions depend on the Iap notion
% Vertex is vertex(Id,info(Grain,Info)) 
%-----------------------------------------------------------------------------

graph(X,Mode,Iap,Y):- cdgraph(X,Mode,Iap,Y,[]).

cdgraph([],_Mode,_Iap,TCDG,TCDG).
cdgraph([_],_Mode,_Iap,TCDG,TCDG).
cdgraph([X,Y|Rest],Mode,Iap,CDG,TCDG):-
	cdgraph0([Y|Rest],X,Mode,Iap,CDG,CDG1),
	cdgraph([Y|Rest],Mode,Iap,CDG1,TCDG).

cdgraph0([Y,Z|Rest],X,Mode,Iap,CDG,TCDG):- !,
	cdgraph0([Y],X,Mode,Iap,CDG,CDG1),
	cdgraph0([Z|Rest],X,Mode,Iap,CDG1,TCDG).
cdgraph0([Y],X,Mode,Iap,CDG,Tail):-
	vertex(X,XVars,InfoX,VX),
	vertex(Y,YVars,InfoY,VY),
	condition(XVars,YVars,Mode,InfoX,InfoY,Iap,Conds),
	no_edge_if_no_cond(Conds,VX,VY,CDG,Tail).

no_edge_if_no_cond(conds([],[],_),_VX,_VY,CDG,CDG):- !.
no_edge_if_no_cond(Conds,VX,VY,[edge(VX,VY,Conds)|CDG],CDG).

% get the vertices which correspond to a body

compute_vertices([],[]).
compute_vertices([X|Body],[Vx|Vertices]):-
	vertex(X,_,_,Vx),
	compute_vertices(Body,Vertices).

% transform a "node" into a "vertex"

vertex(node(Id,Vars,Grain,Info),Vars,Info,vertex(Id,info(Grain,InfoO))):-
	translate_info(Info,InfoO).

translate_info((Info,_Call,_Succ),Info).
translate_info((_Call,_Succ),top).
translate_info(Info,Info).

vertex_info(vertex(Id,info(Grain,Info)),Id,Grain,Info).

% transform a "vertex" into its original goal

vertex_goal(vertex(N,_),Dict,Goal):- arg(N,Dict,Goal).

% transform a number of "vertex" into their original goals

vertex_goals([],_,[]).
vertex_goals([X|Xs],Dict,[G|Gs]):-
	vertex_goal(X,Dict,G),
	vertex_goals(Xs,Dict,Gs).

%-----------------------------------------------------------------------------
% collapse_dep(+,+,+,-)
% collapse_dep(Nodes,Mode,Iap,Deps)
% collapse_dep/4 - makes a list of dependencies upon nodes
% Nodes is a list [node(Id,Vars,Grain,Info)|...]
% Deps is a list [dep(Vertex0,[to(Vertex1,[Cond,...])|...])|...]
% for a dependency of Vertex1 (...n) on Vertex0 labelled with conditions
% Conditions depend on Iap notion
%-----------------------------------------------------------------------------

collapse_dep([],_Mode,_Iap,[]).
collapse_dep([X|Body],Mode,Iap,[dep(Vx,Deps)|Collapsed]):-
	vertex(X,XVars,Info,Vx),
	collapse_for_goal(Body,XVars,Info,Mode,Iap,Deps),
	collapse_dep(Body,Mode,Iap,Collapsed).

collapse_for_goal([],_XVars,_Info,_Mode,_Iap,[]).
collapse_for_goal([Y|Body],XVars,InfoX,Mode,Iap,[to(Vy,Conds)|Deps]):-
	vertex(Y,YVars,InfoY,Vy),
	condition(XVars,YVars,Mode,InfoX,InfoY,Iap,Conds),
	collapse_for_goal(Body,XVars,InfoX,Mode,Iap,Deps).

% a "vertex" in a "dep" is not worth parallelizing if its "node" is not

sequential_dep_vertex(dep(Vertex,_)):-
	vertex(Node,_,_,Vertex),
	sequential_atom(Node).

% transform a "vertex" in a "dep" into its original goal

dep_vertex_goal(dep(Vertex,_),Dict,Vertex,G):- 
	vertex_goal(Vertex,Dict,Goal),
	( Goal =.. [':',G,_K] ; G = Goal).

%-----------------------------------------------------------------------------
% condition(+,+,+,+,+,+,-)
% condition(PVars,QVars,Mode,InfoP,InfoQ,Iap,Conds)
% condition/7 - makes the condition for IAP (siap/nsiap/both)
% Conds is the condition so that Q is independent of P (based on their vars)
% Includes simplification of conditions based on "global" Info
% For Conds we use the term conds(Ground,Indep,Negated) - all sorted!
%-----------------------------------------------------------------------------
% If it is dead-code, do not parallelize (this should be done before!)

condition(_PVars,_QVars,_Mode,false,_InfoQ,_Iap,false):- !.
condition(_PVars,_QVars,_Mode,_InfoP,false,_Iap,false):- !.
condition(PVars,QVars,Mode,InfoP,InfoQ,Iap,Conds):-
	nsiap_condition(Iap,Mode,PVars,QVars,InfoP,InfoQ,Info,NSiapConds),
	siap_condition(Iap,NSiapConds,PVars,QVars,TmpConds),
	simplify_condition(TmpConds,Info,Conds,_).

nsiap_condition(siap,_Mode,_PVars,_QVars,InfoP,_InfoQ,InfoP,_Conds).
nsiap_condition(nsiap,Mode,PVars,QVars,(CallP,SuccP),(CallQ,SuccQ),_Info,
                Conds):-
	nsiap_condition0(CallP,SuccP,PVars,Mode,CallQ,SuccQ,QVars,Conds).
nsiap_condition(both,Mode,PVars,QVars,(Info,CallP,SuccP),(_,CallQ,SuccQ),
                Info,Conds):-
	nsiap_condition0(CallP,SuccP,PVars,Mode,CallQ,SuccQ,QVars,Conds).

/* ?????????????
siap_condition(_Iap,false,_PVars,_QVars,InfoP,_InfoQ,InfoP,_Conds).
*/
siap_condition(both,conds([],[],Neg),_PVars,_QVars,conds([],[],Neg)):- !.
siap_condition(nsiap,TmpConds,_PVars,_QVars,TmpConds):- !.
siap_condition(_Iap,_NSiapConds,PVars,QVars,Conds):-
	siap_condition0(PVars,QVars,Conds).

siap_condition0([],_,conds([],[],[])):- !. 
siap_condition0(_,[],conds([],[],[])):- !.
siap_condition0(Xs,Ys,conds(Ground,Indep,[])):-
	ord_intersection(Xs,Ys,Gvars),
	ord_subtract(Xs,Gvars,Xvars),
	ord_subtract(Ys,Gvars,Yvars),
	ground_conds(Gvars,Ground,[]),
	indep_conds(Xvars,Yvars,Indep_u,[]),
	sort(Indep_u,Indep).

%------------------------------------------------------------------------
% nsiap_condition(CallP,SuccP,PVars,CallQ,SuccQ,QVars,Conds)
% NSIAP conditions depend on each abstract domain
% For LP, domain is always shfr indicating that URLP can be used
%------------------------------------------------------------------------

% change shfr by mshare
nsiap_condition0(mshare(CallP),_SuccP,PVars,Mode,mshare(CallQ),_SuccQ,QVars,
                 Conds):-
	absint_nodes(Mode,CallP,CallQ,PVars,QVars,G1,G2),
	( compute_indep0(G1,[G2],[_],[]) ->
	  Conds=conds([],[],[]) ; Conds=false ).
nsiap_condition0(fr(_CallP),_SuccP,PVars,_Mode,_CallQ,fr(as(Old,New)),QVars,
                 Conds):-
	!,
	absint_nodes(fr,Old,New,PVars,QVars,Vars,FreeVars),
        ( Vars == FreeVars -> Conds=conds([],[],[]) ; Conds=false ).
nsiap_condition0(fd((_,Pre)),_SuccP,PVars,_Mode,_CallQ,fd((Pos,_)),QVars,
                 Conds):- !,
	absint_nodes(fd,Pre,Pos,PVars,QVars,NonGround,FreeVars),
        ( NonGround == FreeVars -> Conds=conds([],[],[]) ; Conds=false ).
nsiap_condition0(_CallP,_SuccP,_PVars,_Mode,_CallQ,_SuccQ,_QVars,false).

%------------------------------------------------------------------------
% ground_imply_indep_simplified(+,+,-)
% ground_imply_indep_simplified(Pos,Facts,SimpFacts)
% Simplifies indep Facts accordingly to ground(X) -> indep(X,Any) for 
% known (positive) facts about groundness
%------------------------------------------------------------------------

ground_imply_indep_simplified([],Facts,Facts):- !.
ground_imply_indep_simplified(Pos,Facts,SimpFacts):-
	ground_imply_indep_simplified0(Facts,Pos,SimpFacts).

ground_imply_indep_simplified0([],_Pos,[]).
ground_imply_indep_simplified0([Fact|Facts],Pos,SimpFacts):-
	subtract_if_member_indep(Pos,Fact,SimpFacts,TailFacts),
	ground_imply_indep_simplified0(Facts,Pos,TailFacts).

subtract_if_member_indep(Pos,indep(X,_),Facts,Facts):-
	ord_member(ground(X),Pos), !.
subtract_if_member_indep(Pos,indep(_,X),Facts,Facts):-
	ord_member(ground(X),Pos), !.
subtract_if_member_indep(_Pos,Fact,[Fact|Facts],Facts).

%-----------------------------------------------------------------------------
% inconsistent_conds(+,+)
% inconsistent_conds(Conds0,Conds1)
% Succeeds if Conds0 and Conds1 can be inconsistent 
% (Remember that negated facts in conds are disjunctions in fact, so this
%  is no proof of inconsistency, but rather a possiblity of it)
%-----------------------------------------------------------------------------

inconsistent_conds(conds(Ground0,Indep0,_Neg0),conds(_Ground1,_Indep1,Neg1)):-
	append(Ground0,Indep0,List),
	inconsistent_lists(List,Neg1).
inconsistent_conds(conds(_Ground0,_Indep0,Neg0),conds(Ground1,Indep1,_Neg1)):-
	append(Ground1,Indep1,List),
	inconsistent_lists(List,Neg0).

% check inconsistency of a list of positive facts and one of negative ones:

inconsistent_lists([Fact|_],Facts) :-
	inconsistent_pos(Facts,Fact).
inconsistent_lists([_|Facts1],Facts2) :-
	inconsistent_lists(Facts1,Facts2).

% check inconsistency of a (pos/neg) fact against a list of them (neg/pos):

inconsistent_dont_know(not(X),PosFacts):- !,
	inconsistent_neg(PosFacts,not(X)).
inconsistent_dont_know(PosFact,NegFacts):- 
	inconsistent_pos(NegFacts,PosFact).

% check inconsistency of a list of positive facts with a negated fact:

inconsistent_neg([PosFact|_],NegFact) :-
	inconsistent(PosFact,NegFact).
inconsistent_neg([_|PosFacts],NegFact) :-
	inconsistent_neg(PosFacts,NegFact).

% check inconsistency of a list of negated facts with a positive fact:

inconsistent_pos([Fact1|_],Fact2) :-
	inconsistent(Fact2,Fact1).
inconsistent_pos([_|Facts],Fact) :-
	inconsistent_pos(Facts,Fact).

% check inconsistency of a positive fact and a negative fact:

inconsistent(ground(X),not(ground(Y))) :- !,
	X == Y.
inconsistent(ground(X),not(indep(Y,_))) :-
	X == Y,!.
inconsistent(ground(X),not(indep(_,Z))) :-
	X == Z.
inconsistent(indep(X,Y),not(indep(P,Q))) :-
	X == P, Y == Q.

%-----------------------------------------------------------------------------
% goal_conditions(+,+,-,-)
% goal_conditions(Conds,Info,Count,Goals)
% goal_conditions/4 - build Goals to check Conds (for MEL)
% For SIAP these are ground([...]) and indep([[..]...]) 
% For MEL Conds are first simplified and the number of conditions is counted
% In MEL conditions cannot be nil so this counts as 1 cge
%-----------------------------------------------------------------------------
% goal_conditions(+,-)
% goal_conditions(Conds,Goals)
% goal_conditions/2 - build Goals to check Conds (for CDG)
% Goals are as before
% Conditions are not counted (in CDG this is done a posteriori)
% For CDG also negations have to be taken out
% In CDG Conds are already simplified and can be nil
%-----------------------------------------------------------------------------

goal_conditions(Conds,Info,count(0,1,0,G,I),Goal):-
	simplify_condition(Conds,Info,SimpConds,_),
	SimpConds=conds(Ground,Indep,_),
	length(Ground,G),
	length(Indep,I),
	goal_conditions_(SimpConds,Goal).

goal_conditions_(conds(Ground,Indep,_),Goal):-
	peel_ground_conditions(Ground,GList),
	peel_indep_conditions(Indep,IList),
	language(Lang),
	siap_goal_conditions(Lang,GList,IList,Goal).

siap_goal_conditions(lp,GList,IList,Goal):-
	lp_goal_conditions(GList,IList,Goal).
siap_goal_conditions(clp,GList,IList,Goal):-
	clp_goal_conditions(GList,IList,Goal).

lp_goal_conditions([],[],true):- !.
lp_goal_conditions([],Indep,indep(Indep)):- !.
lp_goal_conditions(Ground,[],ground(Ground)):- !.
lp_goal_conditions(Ground,Indep,(ground(Ground),indep(Indep))).

clp_goal_conditions([],[],true):- !.
clp_goal_conditions([],Indep,unlinked(Indep)):- !.
clp_goal_conditions(Ground,[],def(Ground)):- !.
clp_goal_conditions(Ground,Indep,(def(Ground),unlinked(Indep))).

peel_ground_conditions([],[]).
peel_ground_conditions([ground(X)|Facts],[X|Vars]):-
	peel_ground_conditions(Facts,Vars).

peel_indep_conditions([],[]).
peel_indep_conditions([indep(X,Y)|Facts],[[X,Y]|Pairs]):-
	peel_indep_conditions(Facts,Pairs).

%----------------------------------------------------------------------------
% group_indep
%
/*
group_indep1(Ann,Body,NBody):-
	language(clp), !,
	group_indep0(Ann,Body,NBody).
group_indep1(_,Body,Body).

group_indep0(mel(_),Body,NBody):- !,
	group_indep(Body,NBody).
group_indep0(cdg(_),Body,NBody):- !,
	group_indep(Body,NBody).
group_indep0(crlp(_),Body,NBody):- !,
	group_indep(Body,NBody).
group_indep0(_,Body,Body).
*/

group_indep((A,B),(A1,B1)):- !,
	group_indep(A,A1),
	group_indep(B,B1).
group_indep((A&B),(A1&B1)):- !,
	group_indep(A,A1),
	group_indep(B,B1).
group_indep((C->A;B),(C1->A1;B1)):- !,
	group_indep_gnd(C,C1),
	group_indep(A,A1),
	group_indep(B,B1).
% group_indep((C=>A),(C1=>A1)):- !,
% 	group_indep_gnd(C,C1),
% 	group_indep(A,A1).
group_indep((C=>A),(C1->A1;A2)):- !,
	group_indep_gnd(C,C1),
	group_indep(A,A1),
	remove_ampersand(A1,A2).
group_indep(B,B).


remove_ampersand('&'(A,B1), ','(A,B2)) :-
	remove_ampersand(B1,B2),
	!.
remove_ampersand(','(A,B1), ','(A,B2)) :-
	remove_ampersand(B1,B2),
	!.
remove_ampersand(T, C) :- 
	T = '=>'(_,_),
	!,
	group_indep(T, C).
remove_ampersand(A, A).


group_indep_gnd((ground(LG),indep(LI)),(ground(LG),Indep)):-
	group_indep_lv(LI,Indep).
group_indep_gnd((def(LG),unlinked(LI)),(def(LG),Unlinked)):-
	group_unlinked_lv(LI,Unlinked).
group_indep_gnd(indep(LI),Indep):-
	group_indep_lv(LI,Indep).
group_indep_gnd(unlinked(LI),Unlinked):-
	group_unlinked_lv(LI,Unlinked).
group_indep_gnd(ground(LG),ground(LG)).
group_indep_gnd(def(LG),def(LG)).

/* This is not correct!
group_indep_lv([[X,Y]|LI],Indep):-
	group_lv(LI,[X],[Y],GroupA,GroupB,NewLI),
	( GroupA == [X], GroupB == [Y] ->
	  Indep=indep([[X,Y]|LI])
	; NewLI == [] ->
	  Indep=indep(GroupA,GroupB)
	; group_indep_lv(NewLI,Indep0),
	  Indep=(indep(GroupA,GroupB),Indep0)
	).

group_unlinked_lv([[X,Y]|LI],Indep):-
	group_lv(LI,[X],[Y],GroupA,GroupB,NewLI),
	( GroupA == [X], GroupB == [Y] ->
	  Indep=unlinked([[X,Y]|LI])
	; NewLI == [] ->
	  Indep=unlinked(GroupA,GroupB)
	; group_unlinked_lv(NewLI,Indep0),
	  Indep=(unlinked(GroupA,GroupB),Indep0)
	).

group_lv([[X,Y]|Rest],LA,LB,GroupA,GroupB,New):-
	ord_member(X,LA),
	\+(ord_member(Y,LA)), !,
	insert(LB,Y,NewLB),
	group_lv(Rest,LA,NewLB,GroupA,GroupB,New).
group_lv([[X,Y]|Rest],LA,LB,GroupA,GroupB,New):-
	ord_member(X,LB),
	\+(ord_member(Y,LB)), !,
	insert(LA,Y,NewLA),
	group_lv(Rest,NewLA,LB,GroupA,GroupB,New).
group_lv([[X,Y]|Rest],LA,LB,GroupA,GroupB,New):-
	ord_member(Y,LA),
	\+(ord_member(X,LA)), !,
	insert(LB,X,NewLB),
	group_lv(Rest,LA,NewLB,GroupA,GroupB,New).
group_lv([[X,Y]|Rest],LA,LB,GroupA,GroupB,New):-
	ord_member(Y,LB),
	\+(ord_member(X,LB)), !,
	insert(LA,X,NewLA),
	group_lv(Rest,NewLA,LB,GroupA,GroupB,New).
group_lv([Pair|Rest],LA,LB,GroupA,GroupB,[Pair|New]):-
	group_lv(Rest,LA,LB,GroupA,GroupB,New).
group_lv([],LA,LB,LA,LB,[]).
*/

%% This is correct, but a very raw approx.:
group_indep_lv(LI,Indep):-
	pairs2groups(LI,Group0_u),
	sort(Group0_u,Group0),
	group_and_reverse(Group0,Group1_u),
	sort(Group1_u,Group1),
	group_and_reverse(Group1,Group2),
	regroup(Group2,Group,NewLI),
	( Group == [] ->
	  Indep=indep(LI)
	; NewLI == [] ->
	  do_groups_indep(Group,Indep)
	; do_groups_indep(Group,Indep0),
	  Indep=(indep(NewLI),Indep0)
	).

group_unlinked_lv(LI,Indep):-
	pairs2groups(LI,Group0_u),
	sort(Group0_u,Group0),
	group_and_reverse(Group0,Group1_u),
	sort(Group1_u,Group1),
	group_and_reverse(Group1,Group2),
	regroup(Group2,Group,NewLI),
	( Group == [] ->
	  Indep=unlinked(LI)
	; NewLI == [] ->
	  do_groups_unlinked(Group,Indep)
	; do_groups_unlinked(Group,Indep0),
	  Indep=(unlinked(NewLI),Indep0)
	).

pairs2groups([[X,Y]|LI],[([X],[Y])|Groups]):-
	pairs2groups(LI,Groups).
pairs2groups([],[]).

group_and_reverse([(G1a,G2a),(G1b,G2b)|Groups],Grouped):-
	G1a==G1b, !,
	merge(G2a,G2b,G2),
	group_and_reverse([(G1a,G2)|Groups],Grouped).
group_and_reverse([(G1a,G2a),(G1b,G2b)|Groups],[(G2a,G1a)|Grouped]):-
	group_and_reverse([(G1b,G2b)|Groups],Grouped).
group_and_reverse([(G1,G2)],[(G2,G1)]).

regroup([([X],[Y])|Grouped],Groups,[[X,Y]|NewLI]):- !,
	regroup(Grouped,Groups,NewLI).
regroup([G|Grouped],[G|Groups],NewLI):-
	regroup(Grouped,Groups,NewLI).
regroup([],[],[]).

do_groups_indep([(G1,G2)],indep(G1,G2)).
do_groups_indep([(G1,G2)|Groups],(indep(G1,G2),More)):-
	do_groups_indep(Groups,More).

do_groups_unlinked([(G1,G2)],unlinked(G1,G2)).
do_groups_unlinked([(G1,G2)|Groups],(unlinked(G1,G2),More)):-
	do_groups_unlinked(Groups,More).



%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

