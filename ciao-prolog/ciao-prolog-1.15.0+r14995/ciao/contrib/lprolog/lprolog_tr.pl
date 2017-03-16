:- module(lprolog_tr, [sentence_trans/3,trans_clause/5]).

:- use_package(hiord).

:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(lprolog_tr_aux).
:- use_module(lprolog_tr_goal).
:- use_module(engine(attributes)).

:- multifile const_name/1.
:- data const_name/1.
:- multifile(is_dynamic/2).
:- data is_dynamic/2.
:- data hold_clause/1.
:- data hold_decl/1.

sentence_trans(0, [], _Module) :- !,
	% clean up an data lying around
	retractall_fact(is_dynamic(_,_)),
	retractall_fact(hold_clause(_)),
	retractall_fact(hold_decl(_)),
	retractall_fact(const_name(_)),
	fail.

sentence_trans(end_of_file, Program, _Module) :- !,
	% Once the whole "sentence" phase is done, dump out all the translated code. 
	% Strictly speaking, it's probably not necessary to do it like this, but this
	% seems to avoid some annoying compiler warnings.
	
	% This first part is the definition of the "solve" predicate. Since it uses
	% "call" if it were in a separate module, you would have to declare the
	% called predicates (which we don't know at this point). At least, i
	% haven't found a way to get it to work properly.  If nothing else, it
	% would be good to put this in a separate file, so it can be better
	% commented.
	%
	% TODO: added error cases (malformed and non-const)
	SolveDef = [
(solve(HORef, Args, I, EnvIn, EnvOut) :-
	get_attribute(HORef, Ref), 
	solve_(Ref, Args, I, EnvIn, EnvOut)),
(solve_(app(FunctorRef, TermArgs), Args, I, EnvIn, EnvOut) :- !, 
	get_attribute(FunctorRef, const(Functor, _N)),
	solve_app(Functor, TermArgs, Args, I, EnvIn, EnvOut)),
(solve_(const(Functor, _N), Args, I, EnvIn, EnvOut) :- !, 
	append(Args, [I, EnvIn, EnvOut], ArgList),
	Pred =.. [Functor | ArgList], 
	call(Pred)),
(solve_app('&', [T1, T2], Args, I, EnvIn, EnvOut) :- !, 
	solve(T1, Args, I, EnvIn, E), 
	solve(T2, Args, I, E, EnvOut)),
(solve_app(';', [T1, T2], Args, I, EnvIn, EnvOut) :- !, 
	solve(T1, Args, I, EnvIn, E)
	; solve(T2, Args, I, E, EnvOut)),
(solve_app(sigma, [LambdaRef], [], I, EnvIn, EnvOut) :- !, 
	deref(Lambda, LambdaRef, EnvIn),
	new_atom(Arg),
	new_ref(var(Arg, I), VarRef),
	new_ref(app(LambdaRef, [VarRef]), AppRef),
	new_atom(A),
	new_ref(var(A,1), ReducedRef),
	ho_unify(AppRef, ReducedRef, EnvIn, E),
	deref(ReducedRef, RR, EnvIn),
	solve(RR, Args, I, E, EnvOut)),
(solve_app(sigma, [Lambda], [Arg|Args], I, EnvIn, EnvOut) :- !, 
	deref(Lambda, LambdaRef, EnvIn),
	new_ref(app(LambdaRef, [Arg]), AppRef),
	new_atom(A),
	new_ref(var(A,1), ReducedRef),
	ho_unify(AppRef, ReducedRef, EnvIn, E),
	deref(ReducedRef, RR, EnvIn),
	solve(RR, Args, I, E, EnvOut)),
(solve_app(Functor, TermArgs, [], I, EnvIn, EnvOut) :- !, 
	append(TermArgs, [I, EnvIn, EnvOut], ArgList),
	Pred =.. [Functor | ArgList], 
	call(Pred))
			],
	(setof(':-'(dynamic(F/N)), is_dynamic(F, N), DynamicDecls) -> true ; DynamicDecls = []),
	(bagof(':-'(Decl), hold_decl(Decl), Decls) -> true ; Decls = []),
	(bagof(Clause, hold_clause(Clause), Clauses) -> true ; Clauses = []),
	list_concat([Decls, DynamicDecls, SolveDef, Clauses, [end_of_file]], Program),
	% if you want it to print out the translated code, uncomment this
	% (i really should use a flag or something)
	%write('%=============== NEW PROGRAM ===============%'), nl,
	%portray_clauses(Program), nl.
	true.


sentence_trans(:-(Decl), [:-(Decl)], _Module) :- !,
	assertz_fact(hold_decl(Decl)).
%fail.

sentence_trans((Head :- Body), ClauseList, Module) :-
	!, 
	process_clause((Head :- Body), ClauseList, Module).

sentence_trans(Term, ClauseList, Module) :- 
	!, 
	process_clause((Term :- true), ClauseList, Module).


process_clause((Head :- Body), [], _Module) :-
	trans_clause((Head :- Body), [Clause], [], _VLout, true),
	assertz_fact(hold_clause(Clause)).


portray_clauses([]).
portray_clauses([Clause | Clauses]) :-
	portray_clause(Clause),
	portray_clauses(Clauses).


%%%%%%%%%%%%%%%%%%%%%  translating clauses (the real work)  %%%%%%%%%%%%%%%%%%%%%

% special handling for "main" - don't process the head
% TODO: should have a way to get command line args 
% in to the program as HO terms
trans_clause((Head :- Body), [(Head :- NewBody)], VLin, VLout, TopLvl) :-
	functor(Head, main, _), !,
	numbervars(Body, 0, _N),
	trans_body(Body, NewGoals, I, E, _E, VLin, VLout, TopLvl),
	init_terms(VLout, VarTerms, I, Ein, E, TopLvl),
	list_concat([[new_env(I,Ein)], VarTerms, NewGoals], List),
	trim_true(List, BodyTerms),
	sequence_to_list(NewBody, BodyTerms).


%-------------------------------------------------------------------------------%
%
% trans_clause(+Clause, -NewClauses, +VLin, -VLout, +TopLvl).
%
% Clause - a clause in the extended Ciao Prolog syntax
%
% VLin - a list of variables in scope at the current level. 
%
% TopLvl - boolean flag. "true" if the clause being translated is at the top
% level in the list of program clauses.  "false" if it is in a goal of another
% clause.
%
% NewClauses - a list of Ciao Prolog clauses
%
% VLout - an updated list of variables, including any newly discovered free
% variables.
%
% see "find_ref" in lprolog_tr_aux.pl for a description of the terms in VarList
%
%-------------------------------------------------------------------------------%

trans_clause((Head :- Body), [(NewHead :- NewBody)], VLin, VLout, TopLvl) :-
	% i convert all the Prolog variables to contstants, since in the end,
	% everything ends up as an attributed variable, and this avoids some of the
	% pain and misery that comes with trying to manipulate logic variables as
	% objects.
	numbervars((Head :- Body), 0, _N),
	trans_head(Head, Functor, NewArgs, ArgTerms, E2, E3, VLin, VL2, TopLvl),
	trans_body(Body, NewGoals, I, E3, E4, VL2, VLout, TopLvl),
	init_terms(VLout, VarTerms, I, E1, E2, TopLvl),
	append(NewArgs, [I, E1, E4], FinalArgs),
	NewHead =.. [Functor | FinalArgs],
	list_concat([VarTerms, ArgTerms, NewGoals], List),
	trim_true(List, BodyTerms),
	sequence_to_list(NewBody, BodyTerms).


%%%%%%%%%%%%%%%%%%%%%%%%%  translating the clause head  %%%%%%%%%%%%%%%%%%%%%%%%%

%-------------------------------------------------------------------------------%
%
% trans_head(+Head, -Functor, -NewArgs, -ArgTerms, +Ein, -Eout, +VLin, -VLout, +TopLvl).
%
% Pred - term of the form name(Arg ... Args)
%
% Functor - the actual Prolog constant to use as the functor for the translated clause
%
% NewArgs - list of distinct Prolog variable that will become the args of the new clause
%
% ArgTerms - list of Prolog terms to construct (at runtime) higher order
% references for first order terms in the Args of NewHead, plus any necessary
% calls to "pattern unify" 
%
% Ein - Prolog variable for the (runtime) list of "tied" variables in the
% environment passed _in_ when the new clause is called
%
% Eout - Prolog variable for the new environment passed out of the clause.
% This will be made the same as the final environment returned from the
% translated body.
%
%-------------------------------------------------------------------------------%

trans_head(Head, Functor, NewArgs, ArgTerms, Ein, Eout, VLin, VLout, TopLvl) :-
	Head =.. [F | Args],
	% TODO: give an error if Functor can't be resovled to a constant or
	% universally quanitfied variable
	find_const_symbol(F, Functor, VLin, VL, TopLvl),
	trans_args(Args, NewArgs, ArgTerms, Ein, Eout, VL, VLout, TopLvl).

%-------------------------------------------------------------------------------%
%
% trans_args(+Args, -NewArgs, -ArgTerms, +Ein, -Eout, +VLin, -VLout, +TopLvl).
%
% Args - a list of terms in the extened syntax
%
% NewArgs - a list of distinct Prolog variables to be the arguments in the new
% created head
%
% ArgTerms - list of Prolog terms to construct (at runtime) higher order
% references for first order terms in the Args of NewHead, plus any necessary
% calls to "pattern unify" 
%
% calls "trans_arg" over each Arg in Args, threading the environment, variable
% list, and argument list through each call
%
%-------------------------------------------------------------------------------%

trans_args(Args, NewArgs, ArgTerms, Ein, Eout, VLin, VLout, TopLvl) :-
	trans_args_(Args, NewArgs, [], ArgTerms, [], Ein, Eout, VLin, VLout, TopLvl).

trans_args_([], [], ATs, ATs, _VA, E, E, VL, VL, _TopLvl).

trans_args_([Arg|Args], [NewArg|NewArgs], ATs1, ATs2, VAL, Ein, Eout, VLin, VLout, TopLvl) :-
	trans_arg(Arg, NewArg, ATs1, ATs, VAL, VALout, Ein, E, VLin, VL, TopLvl),
	trans_args_(Args, NewArgs, ATs, ATs2, VALout, E, Eout, VL, VLout, TopLvl).

%-------------------------------------------------------------------------------%
%
% trans_arg(+Arg, +NewArg, +VarArgsIn, -VarArgsOut, +ArgTermsIn, -ArgTermsOut,
% 				+Ein, -Eout, +VLin, -VLout, +TopLvl).
%
% Arg - the original arg to the clause, in the extened syntax. may be a Prolog
% variable or a term
%
% NewArg - a Prolog variable that will replace the original argement in the
% head of the translated clause
%
% VarArgsIn - a list of any Prolog variables seen as arguments (so repeated
% ones can be treated appropriately)
%
% VarArgsOut - updated list of Prolog variables seen as arguments
%
% ArgTermsIn/ArgTermsOut - a list of Prolog goals to construct, if necessary, a higher
% order representatino of the original argument, plus a call to "pattern_unify"
% to invoke HO unification with the actual argument passed at runtime. The goals
% are appended to the goals in ArgTermsIn
%
%-------------------------------------------------------------------------------%

trans_arg('$VAR'(N), NewArg, ATs, ATs, VAL, ['$VAR'(N)|VAL], E, E, VLin, VLout, true) :-
	nocontainsx(VAL, '$VAR'(N)), !,
	% mostly so the compiler won't yell at us
	var(HORef),
	VLout = [fv('$VAR'(N), NewArg, HORef, true) | VLin].

trans_arg('$VAR'(N), NewArg, ATs, ATs, VAL, ['$VAR'(N)|VAL], E, E, VLin, VLout, false) :-
	nocontainsx(VAL, '$VAR'(N)), 
	% TODO: explain this hackish workaround. trying to avoind problem when 
	% quantifier bound var is prepresented by a prolog var, but then appears in
	% the head of a clause in an impl goal. 
	% this call to find_ref is a hackish way to find out if the prolog var in the head
	% is really free (it comes back as v(_,_,_)) or not.
	find_ref('$VAR'(N), _HORef, VLin, [v(_, _, _) | _], false), !,
	% mostly so the compiler won't yell at us
	var(HORef),
	VLout = [fv('$VAR'(N), NewArg, HORef, false) | VLin].


trans_arg(Arg, NewArg, ATs1, ATs2, VAL, VAL, Ein, Eout, VLin, VLout, TopLvl) :-
	find_ref(Arg, HORef, VLin, VLout, TopLvl), !,
	append(ATs1, [ho_unify(NewArg, HORef, Ein, Eout)], ATs2).

trans_arg(Arg, NewArg, ATs1, ATs2, VAL, VAL, Ein, Eout, VLin, VLout, TopLvl) :-
	build_ho_term(Arg, HORef, BuildList, VLin, VLout, TopLvl),
	list_concat([ATs1, BuildList, [ho_unify(NewArg, HORef, Ein, Eout)]], ATs2).



%%%%%%%%%%%%%%%%%%%%%%%%%  translating the clause body  %%%%%%%%%%%%%%%%%%%%%%%%%

trans_body(Body, NewGoalList, I, Ein, Eout, VLin, VLout, TopLvl) :-
	sequence_to_list(Body, GoalList),
	trans_goals(GoalList, NewGoalList, I, Ein, Eout, VLin, VLout, TopLvl, sigma).


trans_goals([], [], _I, E, E, VL, VL, _TopLvl, _Q).

trans_goals([Goal|Goals], [NewGoal|NewGoals], I, Ein, Eout, VLin, VLout, TopLvl, Q) :-
	trans_goal(Goal, NewGoal, I, Ein, E, VLin, VL, TopLvl, Q),
	trans_goals(Goals, NewGoals, I, E, Eout, VL, VLout, TopLvl, Q).

