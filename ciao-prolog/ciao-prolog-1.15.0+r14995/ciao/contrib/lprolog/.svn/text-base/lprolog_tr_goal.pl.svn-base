:- module(lprolog_tr_goal, [trans_goal/9]).

:- use_package(hiord).

:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(write)).
:- use_module(lprolog_tr).
:- use_module(lprolog_tr_aux).

:- multifile(is_dynamic/2).
:- data is_dynamic/2.

%-------------------------------------------------------------------------------%
%
% trans_goal(+Goal, -CiaoGoal, +I, +Ein, -Eout, +VLin, -VLout, +TopLvl, +Q)
%
% Goal - the original goal in the extended prolog syntax
%
% CiaoGoal - translated goal
%
% I - prolog variable that will be bound to the current quantifier index at
% runtime
%
% Q - last encountered quantifier (sigma or pi)
%
% the first of these handle the basic logical operators and essentially just
% recursively translate any sub-goals, then reassemble them with the same
% operator, theading through the environment and such
%
%-------------------------------------------------------------------------------%

% conjunction (as ',') - "Goal1, Goal2"
trans_goal(','(Goal1, Goal2), CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	!,
	trans_goal(Goal1, CiaoGoal1, I, Ein, E, VLin, VL, TopLvl, Q),
	trans_goal(Goal2, CiaoGoal2, I, E, Eout, VL, VLout, TopLvl, Q),
	CiaoGoal =.. [',', CiaoGoal1, CiaoGoal2].

% if/then/else. "Cond -> Goal1 ; Goal2"
% note, the order of these three (if/then/else, disjunction, if/then)
% is important to get them to prase properly
trans_goal(';'(Left, IfFalse), CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
    Left =.. ['->', Cond, IfTrue],
	!,
	% TODO is it correct to carry the environment through from the condition to the body?
	trans_goal(Cond, NewCond, I, Ein, E, VLin, VL1, TopLvl, Q),
	trans_goal(IfTrue, NewIfTrue, I, E, Eout, VL1, VL2, TopLvl, Q),
	trans_goal(IfFalse, NewIfFalse, I, Ein, Eout, VL2, VLout, TopLvl, Q),
    NewLeft =.. ['->', NewCond, NewIfTrue],
    CiaoGoal =.. [';', NewLeft, NewIfFalse].

% disjunction (as ';') - "Goal1 ; Goal2"
trans_goal(','(Goal1, Goal2), CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	!,
	trans_goal(Goal1, CiaoGoal1, I, Ein, Eout, VLin, VL, TopLvl, Q),
	trans_goal(Goal2, CiaoGoal2, I, Ein, Eout, VL, VLout, TopLvl, Q),
	CiaoGoal =.. [';', CiaoGoal1, CiaoGoal2].

% if/then with no "else". "Cond -> Goal"
trans_goal('->'(Cond, Goal), CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	!,
	trans_goal(Cond, NewCond, I, Ein, E, VLin, VL, TopLvl, Q),
	trans_goal(Goal, NewBody, I, E, Eout, VL, VLout, TopLvl, Q),
	CiaoGoal =.. ['->', NewCond, NewBody].

% cut
trans_goal(!, !, _I, E, E, VL, VL, _TopLvl, _Q).

% true
trans_goal(true, true, _I, E, E, VL, VL, _TopLvl, _Q).



% the remaining are goals that need to be handled speciall for lambda prolog the
% term is deconstructed and the pieces passed off to specific predicates to
% handle them

% explicit unifcation
% this is really very much like "trans_call_goal"
% TODO: this needs some work
trans_goal('='(Term1, Term2), CiaoGoal, _I, Ein, Eout, VLin, VLout, TopLvl, _Q) :-
	!,
	build_ho_term(Term1, HOTerm1, BuildList1, VLin, VL, TopLvl),
	build_ho_term(Term2, HOTerm2, BuildList2, VL, VLout, TopLvl),
	UnifyGoal =.. [ho_unify, HOTerm1, HOTerm2, Ein, Eout],
	list_concat([BuildList1, BuildList2, [UnifyGoal]], GoalList),
	sequence_to_list(CiaoGoal, GoalList).


% implication goal. term of the form "Body => Head"
trans_goal('=>'(Body, Head), CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	!,
	trans_implication_goal(Body, Head, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q).

% universal quantifier. term of the form "pi ID \ Goal", where ID is the
% quantified variable
trans_goal(pi(Goal), CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	Goal =.. [\, ID, InnerGoal], !,
	trans_quantifier_goal(pi, ID, InnerGoal, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q).

% existential quantifier. term of the form "sigma ID \ Goal", where ID is the
% quantified variabler
trans_goal(sigma(Goal), CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	Goal =.. [\, ID, InnerGoal], !,
	trans_quantifier_goal(sigma, ID, InnerGoal, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q).

% call goal. terms of the form "Functor(Arg ... Arg)", "Term @ Term", or
% "call(Functor, Args)" the "call(Functor, Args) version gets inserted by the
% "hiord" Ciao module where the function of a term is a prolog variable
trans_goal(Goal, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	(	Goal =.. [@, Functor | Args] ;
		Goal =.. [call, Functor | Args] ;
		Goal =.. [Functor | Args]), !,
	trans_call_goal(Functor, Args, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q).



%%%%%%%%%%%%%%%%%%%%%%%%%%  translating complex goals  %%%%%%%%%%%%%%%%%%%%%%%%%%


%-------------------------------------------------------------------------------%
%
% trans_call_goal(+Functor, +Args, +CiaoGoal, +I, +Ein, -Eout, +VLin, -VLout, +TopLvl, +Q).
%
%-------------------------------------------------------------------------------%

% functors _not_ to process
ho(nl).
ho(display).

% this seems to be a necessary hack. otherwise the '.' causes errors.
trans_call_goal(., [], [.], _I, E, E, VL, VL, _TopLvl, _Q) :- !.

trans_call_goal(Functor, Args, CiaoGoal, _I, E, E, VL, VL, _TopLvl, _Q) :-
	% TODO need a more sophisticated way to handle
	% preds that shouldn't be processed
	ho(Functor), !,
	CiaoGoal =.. [Functor | Args ].

trans_call_goal(F, Args, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, _Q) :-
	get_functor(F, Functor, VLin, VL, TopLvl),
	trans_args(Args, ArgRefs, ArgTerms, VL, VLout, TopLvl),
	make_call(Functor, ArgRefs, I, Ein, Eout, CallGoal),
	append(ArgTerms, CallGoal, GoalList),
	sequence_to_list(CiaoGoal, GoalList).

make_call(Functor, Args, I, Ein, Eout, [CallGoal]) :- 
	var(Functor), !, 
	% it's an HORef, so we need a solve
	CallGoal =.. [solve, Functor, Args, I, Ein, Eout].

make_call(Functor, Args, I, Ein, Eout, [CallGoal]) :- 
	append(Args, [I, Ein, Eout], NewArgs),
	CallGoal =.. [Functor | NewArgs].

trans_args(Args, NewArgs, ArgTerms, VL1, VL2, TopLvl) :-
	trans_args_(Args, NewArgs, [], ArgTerms, VL1, VL2, TopLvl).

trans_args_([], [], ATs, ATs, VL, VL, _TopLvl).

trans_args_([Arg|Args], [NewArg|NewArgs], ATsin, ATsout, VLin, VLout, TopLvl) :-
	build_ho_term(Arg, NewArg, BuildList, VLin, VL, TopLvl),
	append(ATsin, BuildList, ATs),
	trans_args_(Args, NewArgs, ATs, ATsout, VL, VLout, TopLvl).



%-------------------------------------------------------------------------------%
%
% trans_quantifier_goal(+Quantifier, +ID, +InnerGoal, +I, +Ein, -Eout, +VLin, -VLout, +TopLvl, +Q).
%
% TODO: the implementation of VarList should allow for not carrying along the
% local variables here (only the top level ones).
%
%-------------------------------------------------------------------------------%

trans_quantifier_goal(pi, ID, Goal, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, sigma) :-
	!, 
	IncGoal = [I1 is I + 1],
	var(HORef),
	new_const_name(ID, Sym),
	VL = [uqv(ID, Sym, HORef) | VLin],
	% note strange ordering over env here. process inner goal
	% first now, but it comes _after_ init_terms in the final code.
	trans_goal(Goal, InnerGoal, I1, E, Eout, VL, VL2, TopLvl, pi),
	% TODO is the the best way to filter out v(ID, HORef) from VarListOut
	% originally i had two lists, one for local and one for "global" (things
	% carried along). i might go back to that.
	find_x(ID, VL2, X),
	delete(VL2, X, VLout),
	init_terms([X], VarTerms, I1, Ein, E, TopLvl),
	list_concat([IncGoal, VarTerms, [InnerGoal]], GoalList),
	sequence_to_list(CiaoGoal, GoalList).

% TODO: these to really are almost the same. should combine them somehow
trans_quantifier_goal(Quant, ID, Goal, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, _Q) :-
	var(HORef),
	new_const_name(ID, Sym),
	(Quant = pi -> VL = [uqv(ID, Sym, HORef) | VLin] ; VL = [eqv(ID, HORef) | VLin]),
	trans_goal(Goal, InnerGoal, I, E, Eout, VL, VL2, TopLvl, Quant),
	% TODO is the the best way to filter out v(ID, HORef) from VarListOut
	find_x(ID, VL2, X),
	delete(VL2, X, VLout),
	init_terms([X], VarTerms, I, Ein, E, TopLvl),
	list_concat([VarTerms, [InnerGoal]], GoalList),
	sequence_to_list(CiaoGoal, GoalList).

% (hopefully) temporary hack used in setting up for variable init
% in trans_quantifier_goal
find_x(ID, [uqv(ID, Sym, HORef) | _Rest], uqv(ID, Sym, HORef)) :- !.
find_x(ID, [tc(ID, Sym, HORef, TRef) | _Rest], tc(ID, Sym, HORef, TRef)) :- !.
find_x(ID, [eqv(ID, HORef) | _Rest], eqv(ID, HORef)) :- !.
find_x(ID, [tv(ID, Sym, HORef, TRef) | _Rest], tv(ID, Sym, HORef, TRef)) :- !.
find_x(ID, [_ | Rest], X) :- find_x(ID, Rest, X).



%-------------------------------------------------------------------------------%
%
% trans_conditional_goal(+Body, +Head, +CiaoGoal, +I, +Ein, -Eout, +VLin, -VLout, +TopLvl, +Q)
%
%-------------------------------------------------------------------------------%

trans_implication_goal(Body, Head, CiaoGoal, I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	trans_goal(Head, HeadGoal, I, Ein, Eout, VLin, VL, TopLvl, Q),
	trans_impl_body(Body, BodyClauses, VL, VLout),
	asserts(BodyClauses, AssertGoal),
	retracts(BodyClauses, RetractGoal),
	list_concat([
		AssertGoal,
		[HeadGoal],
		RetractGoal
		],
		GoalList),
	sequence_to_list(CiaoGoal, GoalList).


trans_impl_body((Head :- Goal), [CiaoClause], VLin, VLout) :- !,
	trans_clause((Head :- Goal), [CiaoClause], VLin, VLout, false),
	CiaoClause = (NewHead :- _NewBody),
	functor(NewHead, F, N),
	asserta_fact(is_dynamic(F, N)).

trans_impl_body('&'(Goal1, Goal2), GoalList, VLin, VLout) :- !,
	trans_impl_body(Goal1, Goal1Clause, VLin, VL),
	trans_impl_body(Goal2, Goal2Clause, VL, VLout),
    append(Goal1Clause, Goal2Clause, GoalList).

trans_impl_body(Term, [CiaoGoal], VLin, VLout) :-
	trans_impl_body((Term :- true), [CiaoGoal], VLin, VLout).

asserts([], []).
% TODO: add debug message on backtracking
asserts([Clause|Clauses], [asserta(Clause), undo(retract(Clause)) | Asserts]) :-
	asserts(Clauses,  Asserts).

% TODO: add debug message on backtracking
retracts([], []).
retracts([Clause|Clauses], [retract(Clause), undo(asserta(Clause)) | Retracts]) :-
	retracts(Clauses,  Retracts).
