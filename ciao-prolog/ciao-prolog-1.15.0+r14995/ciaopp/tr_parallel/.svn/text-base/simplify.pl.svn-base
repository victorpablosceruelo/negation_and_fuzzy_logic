:- module(simplify,
	[
	    simplify_condition/4,
	    close_info/2,
	    negate_info/2
	],
	[]).

:- use_module(annotate, 
	[
	    inconsistent_lists/2,
	    inconsistent_neg/2,
	    inconsistent_pos/2,
	    ground_imply_indep_simplified/3
	]).

:- use_module(library(sets), 
	[
	    insert/3,
	    ord_member/2,
	    ord_subset/2,
	    ord_subtract/3
	]).

:- use_module(library(lists), [append/3]).

:- use_module(library(idlists), [memberchk/2]).


%------------------------------------------------------------------------
% simplify_condition(+,+,-,-)
% simplify_condition(conds(Ground,Indep,Neg),Info,NewConds,NewInfo)
% simplify_condition/5 - simplify the condition for SIAP 
% The positive conditions are simplified based on available "global" 
% Info, which is also updated accordingly
% Neg parts of conds are ignored (because they are disjunctions and we
% do not treat but conjunctions)
% Conditions are added one by one starting with Ground conds, in the
% order in which they come (i.e. sorted - the output is also sorted)
% Regard that this is because some can imply the others: we use the order
% implied by sorting!
%------------------------------------------------------------------------

simplify_condition(false,Info,false,Info):- !.                       % false
simplify_condition(conds([],[],Neg),Info,conds([],[],Neg),Info):- !. % true
simplify_condition(conds(Ground,Indep,_),Info,NewConds,NewInfo):-
	Info = global(_,Neg,_),
	append(Ground,Indep,Pos),
	inconsistent_lists(Pos,Neg), !,
	NewConds = false,
	NewInfo = Info.
simplify_condition(conds(Ground,Indep,Neg),Info,NewConds,NewInfo):-
	Info = global(Pos,_Neg,_Imp),
	ord_subtract(Ground,Pos,UpdatedGround),
	ord_subtract(Indep,Pos,UpdatedIndep),
	ground_imply_indep_simplified(UpdatedGround,UpdatedIndep,SimpleIndep1),
	ground_imply_indep_simplified(Pos,SimpleIndep1,SimpleIndep2),
	add_and_update(UpdatedGround,SimpleIndep2,Neg,Info,NewConds,NewInfo).

add_and_update(false,_Indep,_Neg,Info,false,Info):- !.
add_and_update([],Indep,Neg,Info,NewConds,NewInfo):-
	add_and_update1(Indep,Neg,Info,NewConds,NewInfo).
add_and_update([Fact|Facts],Indep,Neg,Info,NewConds,NewInfo):-
	add_info(Fact,Info,TmpInfo,Flag),
	( Flag = consistent ->
	    simplify_condition(conds(Facts,Indep,Neg),TmpInfo,TmpConds,NewInfo),
	    add_and_update_g0(TmpConds,Fact,NewConds)
	; NewConds = false).

add_and_update1(false,_Neg,Info,false,Info):- !.
add_and_update1([],Neg,Info,conds([],[],Neg),Info).
add_and_update1([Fact|Facts],Neg,Info,NewConds,NewInfo):-
	add_info(Fact,Info,TmpInfo,Flag),
	( Flag = consistent ->
	    simplify_condition(conds([],Facts,Neg),TmpInfo,TmpConds,NewInfo),
	    add_and_update_i0(TmpConds,Fact,NewInfo,NewConds)
	; NewConds = false).

add_and_update_g0(false,_Fact,false).
add_and_update_g0(conds(Facts,Indep,Neg),Fact,conds([Fact|Facts],Indep,Neg)).

add_and_update_i0(false,_Fact,_NewInfo,false).
add_and_update_i0(conds(Ground,Facts,Neg),Fact,global(Pos,_,_),NewConds):-
	Fact = indep(X,Y),
	( (memberchk(ground(X),Ground);memberchk(ground(Y),Ground)) ->
	     NewConds = conds(Ground,Facts,Neg)
	  ; (ord_member(ground(X),Pos) ->
	        NewConds = conds([ground(X)|Ground],Facts,Neg)
	    ; ( ord_member(ground(Y),Pos) ->
	      NewConds = conds([ground(Y)|Ground],Facts,Neg)
	      ;  NewConds = conds(Ground,[Fact|Facts],Neg)
              )
            )
	).

add_info(Fact,global(Pos,Neg,Imp),Info,Flag):-
	insert(Pos,Fact,TmpPos),
	close_info_f(global(TmpPos,Neg,Imp),Info,Flag).

%------------------------------------------------------------------------
% close_info(Info,ClosedInfo)
% close_info(+,-)
% close_info/2 - close "global" Info under implication (->)
% All implied facts are added, no new implications are created
%------------------------------------------------------------------------

close_info(Info,ClosedInfo):-
	close_info_f(Info,ClosedInfo,_).

close_info_f(global(Pos,Neg,Imp),InfoClosed,Flag):-
	close_info_it(Imp,Pos,Neg,NewPos,NewNeg,stop,F,Flag),
	close_info_f_(F,global(NewPos,NewNeg,Imp),InfoClosed,Flag).

close_info_f_(stop,TmpInfoClosed,InfoClosed,Flag):-
	(var(Flag) ->
	    InfoClosed = TmpInfoClosed
	;   InfoClosed = false).
close_info_f_(go,Info,InfoClosed,Flag):-
	close_info_f(Info,InfoClosed,Flag).

close_info_it([I|Is],Pos,Neg,OutPos,OutNeg,F0,F,Flag):-
	close_info0(I,Pos,Neg,NewPos,NewNeg,F0,F1,Flag1),
	( Flag1 = consistent ->
	    close_info_it(Is,NewPos,NewNeg,OutPos,OutNeg,F1,F,Flag)
	; Flag = inconsistent).
close_info_it([],Pos,Neg,Pos,Neg,F,F,_).

close_info0((P->Q),Pos,Neg,NewPos,NewNeg,F0,F,Flag):-
	ord_subset(P,Pos), !,
	close_info1(Q,Pos,Neg,TmpPos,TmpNeg,F0,F1,Flag1),
	( Flag1 = consistent ->
	    close_info2(Q,P,TmpPos,TmpNeg,NewPos,NewNeg,F1,F,Flag)
	; Flag = inconsistent).
close_info0((P->Q),Pos,Neg,NewPos,NewNeg,F0,F,Flag):- 
	close_info2(Q,P,Pos,Neg,NewPos,NewNeg,F0,F,Flag).

close_info1(Q,Pos,Neg,NewPos,NewNeg,F0,F,_):-
	member_pos_or_neg(Q,Pos,Neg), !,
	NewPos=Pos,
	NewNeg=Neg,
	F=F0.
close_info1(Q,Pos,Neg,NewPos,NewNeg,_,go,Flag):-
	( it_is_inconsistent(Q,Pos,Neg) ->
	    Flag = inconsistent
	; insert_pos_or_neg(Q,Pos,Neg,NewPos,NewNeg)).

it_is_inconsistent(not(X),Pos,_) :- !,
	inconsistent_neg(Pos,not(X)).
it_is_inconsistent(Q,_,Neg) :- !,
	inconsistent_pos(Neg,Q).

member_pos_or_neg(Q,Pos,_Neg):- 
	ord_member(Q,Pos),!.
member_pos_or_neg(Q,_Pos,Neg):- 
	ord_member(Q,Neg).

insert_pos_or_neg(not(X),Pos,Neg,NewPos,NewNeg):- !,
	insert(Neg,not(X),NewNeg),
	NewPos = Pos.
insert_pos_or_neg(X,Pos,Neg,NewPos,Neg):- 
	insert(Pos,X,NewPos).

% this one is too naive, but more than one cond will give disjunctions and
% these are much more complicated to treat

close_info2(Q,[Fact],Pos,Neg,NewPos,NewNeg,F0,F,Flag):-
	negate_info(Q,NotQ),
	member_pos_or_neg(NotQ,Pos,Neg), !,
	negate_info(Fact,NotFact),
	close_info1(NotFact,Pos,Neg,NewPos,NewNeg,F0,F,Flag).
close_info2(_,_,Pos,Neg,Pos,Neg,F,F,_).

negate_info([Fact|Facts],[Fact_neg|Facts_neg]) :- !,
	negate_info(Fact,Fact_neg),
	negate_info(Facts,Facts_neg).
negate_info([],[]):- !.
negate_info(not(C),C):- !.
negate_info(C,not(C)).


%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

