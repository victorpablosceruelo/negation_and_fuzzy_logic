:- module(gran_trans,
	[
	    seq/2,
	    clause_translate_to_internal/2, 
	    translate_to_internal/2,  
	    flat_seq_body/2
	], [andprolog,assertions]).

:- use_module(program(p_unit), [type_of_goal/2]).

%%---------------------------------------------------------------------
% Sequentialize a body.
%%---------------------------------------------------------------------

seq((GoalA,GoalB) , (SeqGoalA,SeqGoalB) ):- !,
	seq(GoalA, SeqGoalA),
	seq(GoalB, SeqGoalB).
seq(( _  => SubGoal ) , SeqSubGoal ):- !,
	seq( SubGoal , SeqSubGoal).
seq((GoalA & GoalB) , (SeqGoalA,SeqGoalB) ):- !,
	seq(GoalA, SeqGoalA),
	seq(GoalB, SeqGoalB).
seq('andprolog_rt:&'(GoalA,GoalB), (SeqGoalA,SeqGoalB) ):- !,
	seq(GoalA, SeqGoalA),
	seq(GoalB, SeqGoalB).
% 	SeqGoalA1 = $(SeqGoalA,_,_),
% 	SeqGoalB1 = $(SeqGoalB,_,_).
seq(( _ -> _ ; ElseGoal ) ,SeqElseGoal ):- !,
	seq(ElseGoal,SeqElseGoal).
seq(Literal, InternalLiteral):- 
	translate_to_internal(Literal, InternalLiteral).

%%---------------------------------------------------------------------
% Transforms a conjunction of goals in any structure into the structure
% (Literal , RestOfGoals). 
%%---------------------------------------------------------------------

flat_seq_body( (( A , B ) , C) ,FlatBody):-
	!,
	flat_seq_body( A , FA ),
	flat_seq_body( B , FB ),
	flat_seq_body( C , FC ),
	app_goals(FB,FC,AG),
	app_goals(FA,AG,FlatBody).

flat_seq_body( ( Lit , Body) , (Lit , FlatBody) ):- !,
	flat_seq_body( Body , FlatBody ).

flat_seq_body( Lit , Lit ):- !.

%%---------------------------------------------------------------------
% Appends  two conjunctions of goals of the form (Literal , RestOfGoals).
%%---------------------------------------------------------------------

app_goals( (Lit , Body) , Goals , ( Lit , AppGoals) ):- !,
	app_goals(Body,Goals,AppGoals).

app_goals( Lit , Goals , ( Lit , Goals) ):- !.

clause_translate_to_internal(Clause, InternalClause):-
	Clause = (H :- B),
	!,
	body_translate_to_internal(B,InternalBody),
	InternalClause = (H :- InternalBody).
clause_translate_to_internal(Clause, Clause).


body_translate_to_internal((A,B),(NA,NB)):-
	!,
	body_translate_to_internal(A,NA),
	body_translate_to_internal(B,NB).
body_translate_to_internal((A&B),(NA&NB)):-
	body_translate_to_internal(A,NA),
	body_translate_to_internal(B,NB).
body_translate_to_internal('andprolog_rt:&'(A,B),(NA&NB)):-!,
	body_translate_to_internal(A,NA),
	body_translate_to_internal(B,NB).
body_translate_to_internal((A->B;C),(NA->NB;NC)):-
	!,
	body_translate_to_internal(A,NA),
	body_translate_to_internal(B,NB),
	body_translate_to_internal(C,NC).
body_translate_to_internal((A->B),(NA->NB)):-
	!,
	body_translate_to_internal(A,NA),
	body_translate_to_internal(B,NB).
body_translate_to_internal((A=>B),(NA=>NB)):-
	!,
	body_translate_to_internal(A,NA),
	body_translate_to_internal(B,NB).
body_translate_to_internal(!,!):- !.
body_translate_to_internal(\+($(A,_,_)):_,\+(A)):- !.
body_translate_to_internal('hiord_rt:call'($(A,_,_)):_,NBody):- !,
	NBody = 'hiord_rt:call'(A).
body_translate_to_internal('aggregates:bagof'(Var,$(A,_,_),List):_Id,NBody):-!,
	NBody = 'aggregates:bagof'(Var,A,List).
body_translate_to_internal('aggregates:setof'(Var,$(A,_,_),List):_Id,NBody):-!,
	NBody = 'aggregates:setof'(Var,A,List).
body_translate_to_internal('aggregates:findall'(Var,$(A,_,_),List):_Id,NBody):-!,
	NBody = 'aggregates:findall'(Var,A,List).
body_translate_to_internal(A,IA):-
        translate_to_internal(A, IA).


translate_to_internal(Literal, InternalLiteral):- 
	type_of_goal(builtin(InternalLiteral),Literal),
	!.
translate_to_internal(Literal, NegInternalGoal):- 
	type_of_goal(metapred(not(G),_Meta), Literal),
	G = $(Term,_Go,_Ty),
	!,
	translate_to_internal(Term, InternalGoal),
	functor(Literal,F,1),
	functor(InternalLiteral,F,1),
	arg(1,InternalLiteral,InternalGoal),
	push_neg(InternalLiteral, NegInternalGoal).
translate_to_internal($(Literal,_,_), Literal):- !.
translate_to_internal(Literal, Literal).


push_neg(\+(\+(X)), Y):- !, push_neg(X, Y).
push_neg(\+(X), Y):- !, translate_negation(X, Y). 
push_neg(X, X).


translate_negation(X, Y):- 
	neg_test(X, Y),
	!.
translate_negation(X, X). 


neg_test(==(X, Y),  \==(X, Y)).
neg_test(\==(X, Y), ==(X, Y)).
neg_test(=(X, Y),   \==(X, Y)).
neg_test(=\=(X, Y), =:=(X, Y)).
neg_test(=:=(X, Y), =\=(X, Y)).
neg_test(<(X, Y),   >=(X, Y)).
neg_test(>(X, Y),   =<(X, Y)).
neg_test(=<(X, Y),  >(X, Y)).
neg_test(>=(X, Y),  <(X, Y)).

