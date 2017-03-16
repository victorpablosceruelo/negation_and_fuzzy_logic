:- module(lprolog_tr_aux, [build_ho_term/6, find_ref/5, get_functor/5, find_const_symbol/5, init_terms/6, error_msg/1, trim_true/2, new_const_name/2], []).

:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(messages)).
:- use_module(library(prolog_sys)).
:- use_module(library(terms), [atom_concat/2]).

:- data const_name/1.
:- multifile const_name/1.


%-------------------------------------------------------------------------------%
%
% build_ho_term(+Term, -HORef, -BuildList, +VLin, -VLout, +TopLvl)
%
% Term - a first order prolog term
%
% HORef - a prolog variable that will (at runtime) be built in to the higher
% order representation of Term by the terms in CiaoGoal
%
% BuildList - list of Prolog goals to construct (at runtime) higher order
% references for first order term
%
%-------------------------------------------------------------------------------%

% term is a numeric constant
% TODO: we'll need to decide how to handle numbers and arithmetic at some point
build_ho_term(Term, HORef, BuildList, VL, VL, _TopLvl) :-
	number(Term), !,
	BuildList = [new_ref(const(Term, 0), HORef)].

% term is a lambda construct
% TODO: currently this doesn't "collapse" nested lambda terms. hnorm handles
% this at runtime, but it might be more efficient to do it here eventually.
build_ho_term(Term, HORef, BuildList, VLin, VLout, TopLvl) :-
	Term =.. [\, ID, InnerTerm], !,
	VL = [db(ID) | VLin],
	build_ho_term(InnerTerm, InnerRef, InnerBuildList, VL, VLout, TopLvl), 
	LambdaGoal = new_ref(lam(1, InnerRef), HORef),
	append(InnerBuildList, [LambdaGoal], BuildList).

% atomic terms
build_ho_term('$VAR'(N), HORef, BuildList, VLin, VLout, TopLvl) :- !,
	find_ref_('$VAR'(N), HORef, BuildList, 1, VLin, VLout, TopLvl).

build_ho_term(Term, HORef, BuildList, VLin, VLout, TopLvl) :- 
	atom(Term), !,
	find_ref_(Term, HORef, BuildList, 1, VLin, VLout, TopLvl).


% term is a compound term, possibly with a variable head, or function
% application note: this should correctly handle quantifiers (pi/sigma) without
% a special case, since they should be parsed as pi(Term) and sigma(Term)
build_ho_term(Term, HORef, BuildList, VLin, VLout, TopLvl) :-
	(	Term =.. [@, Functor | Args] ;
		Term =.. [call, Functor | Args] ;
		Term =.. [Functor | Args]), !,
	build_ho_term(Functor, FunctorRef, FunctorBL, VLin, VL, TopLvl),
	build_ho_term_args(Args, ArgRefs, ArgBL, VL, VLout, TopLvl),
	AppGoal = new_ref(app(FunctorRef, ArgRefs), HORef),
	list_concat([FunctorBL, ArgBL, [AppGoal]], BuildList).


%-------------------------------------------------------------------------------%
%
% build_ho_term_args(+Args, -HORefs, -BuildList, +VLin, -VLout, +TopLvl)
%
% HORefs - a list of prolog variable that will (at runtime) be built in to the higher
% order representation of Term by the terms in CiaoGoal
%
% BuildList - list of Prolog goals to construct (at runtime) higher order
% references for first order term
%
%
% calls "construct_term" over each Term in Args (for an application) threading
% the variable list through each call.
%
%-------------------------------------------------------------------------------%

build_ho_term_args([], [], [], VL, VL, _TopLvl).

build_ho_term_args([Arg | Args], [HORef | HORefs], BuildList, VLin, VLout, TopLvl) :-
	build_ho_term(Arg, HORef, BL1, VLin, VL, TopLvl),
	build_ho_term_args(Args, HORefs, BL2, VL, VLout, TopLvl),
	append(BL1, BL2, BuildList).


%-------------------------------------------------------------------------------%
%
% find_ref_(+ID, -HORef, -CiaoGoals, +N, +VLin, -VLout, +TopLvl)
%
% ID - a prolog variable or constant naming a lambda prolog variable in the
% extended syntax.
%
% HORef - prolog variable for the higher order term that will represent the
% variable at runtime
%
% CiaoGoal - a list of terms to construct a higher order term for HORef.
% Usually empty - only used for DeBruin indices.
%
% N - a counter for creating DeBruin index terms. initially 1, incremented with
% each pass over a "db(_)" term in VarListIn.
%
% VLin - list of name/variable mappings in order of increasing scope
% 
% VLout - updated variable list, reflecting any necessary "conversions" of
% variable to "tied" variables
%
% this steps through the elements of VarListIn looking for a previously defined
% HO term that matches ID.  if a match isn't found, then a new refenence is
% added.  terms in the list keep track of the original ID used, as well as the
% prolog variable (HORef) that will be the HO reprentation at runtime. later,
% variable_init will process this list to create the runtime terms, using the
% same HORef.
%
% the list is built up as we recursively descend in to terms, so scope is
% maintained automatically.  (well, currently with a bit of additional sloppy
% hacking, but that will be fixed later) this predicate also handles
% identifying "tied variables"
%
% terms in VarList are of the form:
%
% "v(ID, HORef TopLvl)" for top level free Prolog variables (treated as existentially
% quantified at the outtermost scope).
%
% c(ID, HORef TopLvl) for top level Prolog constants (treated as universally
% quantified at the outtermost scope).
%
% "db(ID)" for lambda bound variables that will be represented with a DeBruin
% index
%
% "fv(ID, Arg, HORef, TopLvl)" for top level free Prolog variables in the head
% of a clause (Arg is the variable to be used in the arg position in the head)
%
% "eqv(ID, HORef)" and "uqv(ID, Sym, HORef)" for quantifier bound variables,
% where ID is a Prolog variable or constant, and HORef is a Prolog variable.
% For universally quantified variables, Sym is the unique constant identifying
% the term.
%
% "tv(ID, HORef, Key, TRef)" and tc(ID, HORef, TRef) for "tied" variables.
% HORef is the logic variable used to represent the term in the "outer"
% context, the TRef is in side the implication goal. Key will be instantiated
% at runtime with the unique constant to identify a tied variable in the
% environment. For constants, the unique ID is used instead.
%
%-------------------------------------------------------------------------------%

% exported version never called in context of a term, 
% so it doesn't have to worry about debruin indices
find_ref(Arg, HORef, VLin, VLout, TopLvl) :-
	find_ref_(Arg, HORef, _, 0, VLin, VLout, TopLvl).

% ID wasn't found, so insert it.
% logic variable
find_ref_('$VAR'(N), HORef, [], _DB, [], [v('$VAR'(N), HORef, true)], true) :- !.

% for now, treat any prolog variable in an implication goal as shared
% (this will result in some variables being treated as shared when unecessary)
find_ref_('$VAR'(N), TRef, [], _DB, [], [tv('$VAR'(N), HORef, Key, TRef)], false) :- 
	!, var(Key), var(HORef).

% prolog constant
find_ref_(ID, HORef,  [], _DB, [], [c(ID, HORef, TopLvl)], TopLvl) :- atom(ID), !.

% lambda bound variable
find_ref_(ID, HORef, CiaoGoal, N, [db(ID) | Rest], [db(ID) | Rest], _TopLvl) :- !,
	CiaoGoal = [new_ref(dB(N), HORef)].

% top level free vars
find_ref_(ID, HORef, [], _DB, [fv(ID, Arg, HORef, TopLvl) | Rest], [fv(ID,  Arg, HORef, TopLvl) | Rest], TopLvl) :- !.

% top level constant
find_ref_(ID, HORef, [], _DB, [c(ID, HORef, TopLvl) | Rest], [c(ID, HORef, TopLvl) | Rest], TopLvl) :- !.

% top level variable
find_ref_(ID, HORef, [], _DB, [v(ID, HORef, TopLvl) | Rest], [v(ID, HORef, TopLvl) | Rest], TopLvl) :- !.

% quantifier bound variable
find_ref_(ID, HORef, [], _DB, [uqv(ID, Sym, HORef) | Rest], [uqv(ID, Sym, HORef) | Rest], true) :- !.
find_ref_(ID, HORef, [], _DB, [eqv(ID, HORef) | Rest], [eqv(ID, HORef) | Rest], true) :- !.

% quantifier bound variables when found in an impl goal become tied
%find_ref_(ID, HORef, [], _DB, [uqv(ID, Sym, HORef) | Rest], [tc(ID, Sym, HORef, TRef) | Rest], false) :- var(TRef), !.
find_ref_(ID, TRef, [], _DB, [uqv(ID, Sym, HORef) | Rest], [tc(ID, Sym, HORef, TRef) | Rest], false) :- var(TRef), write('======== HERE ========='), nl, !.
find_ref_(ID, TRef, [], _DB, [eqv(ID, HORef) | Rest], [tv(ID, HORef, Key, TRef) | Rest], false) :- var(Key), !.

% tied vars
find_ref_(ID, HORef, [], _DB, [tc(ID, HORef, TRef) | Rest], [tc(ID, HORef, TRef) | Rest], true) :- !.
find_ref_(ID, HORef, [], _DB, [tv(ID, HORef, Key, TRef) | Rest], [tv(ID, HORef, Key, TRef) | Rest], true) :- !.

% should be able to treat tied constants normally, i hope
%find_ref_(ID, HORef, [], _DB, [tc(ID, HORef, TRef) | Rest], [tc(ID, HORef, TRef) | Rest], false) :- !.
find_ref_(ID, TRef, [], _DB, [tc(ID, HORef, TRef) | Rest], [tc(ID, HORef, TRef) | Rest], false) :- !.
find_ref_(ID, TRef, [], _DB, [tv(ID, HORef, Key, TRef) | Rest], [tv(ID, HORef, Key, TRef) | Rest], false) :- !.

% lambda bound variable, but not _this_ one
% note: important to keep the db(N)
find_ref_(ID, HORef, CiaoGoal, N, [db(X) | Rest1], [db(X) | Rest2], TopLvl) :- !,
	N1 is N + 1,
	find_ref_(ID, HORef, CiaoGoal, N1, Rest1, Rest2, TopLvl).

% not found, keep looking
find_ref_(ID, HORef, CiaoGoal, N, [IdRef | Rest1], [IdRef| Rest2], TopLvl) :- !,
	find_ref_(ID, HORef, CiaoGoal, N, Rest1, Rest2, TopLvl).


% used by trans_call_goal to find the term to use as the functor of the of the goal. 
% might be a simple comple time constant of an HO term.
get_functor(F, Functor, VLin, VLout, TopLvl) :-
	find_const_symbol(F, Functor, VLin, VLout, TopLvl), !.

get_functor(F, Functor, VLin, VLout, TopLvl) :-
	% relatively inefficient, since we end up traversing the
	% var list again
	find_ref(F, Functor, VLin, VLout, TopLvl).


% used by trans_head to find the Prolog constant to use as the name of the
% translated clause. this might be a universally quantified bound variable, so
% we nee the constant symbol associated with it
find_const_symbol(Term, Atom, VLin, VLout, TopLvl) :-
	find_const(Term, Atom, VLin, VLout, TopLvl), !.

find_const_symbol(Term, Term, VL, VL, _TopLvl) :- atom(Term).


find_const(ID, ID, [c(ID, HORef, TopLvl) | Rest], [c(ID, HORef, TopLvl) | Rest], TopLvl) :- !.
find_const(ID, Sym, [uqv(ID, Sym, HORef) | Rest], [uqv(ID, Sym, HORef) | Rest], true) :- !.
find_const(ID, Sym, [uqv(ID, Sym, HORef) | Rest], [tc(ID, Sym, HORef, TRef) | Rest], false) :- !,
	var(TRef).
find_const(ID, Sym, [tc(ID, Sym, HORef, TRef) | Rest], [tc(ID, Sym, HORef, TRef) | Rest], _TopLvl) :- !.
find_const(Term, ID, [X | Rest1], [X | Rest2], TopLvl) :- 
	find_const(Term, ID, Rest1, Rest2, TopLvl).

%-------------------------------------------------------------------------------%
%
% init_term(+Var, -TermList, +I, +Ein, -Eout, +TopLvl)
%
% term from the var list for which to generate initialization code (see above)
%
% I - prolog variable that will be bound to the current quantifier index at
% runtime
%
% TermList - list of terms to construct (at runtime) the higher order
% representation of variables.  "tied" variables are either built and inserted
% in to the environment, or gotten from the environment, depending on the value
% of TopLevel.
%
% Ein - prolog variable for the runtime environment of "tied" variables. will
% be threaded through any calls to "get_env" and "put_env"
%
%-------------------------------------------------------------------------------%

init_terms([], [], _I, E, E, _TopLvl).

init_terms([Var|Vars], VarTerms, I, Ein, Eout, TopLvl) :-
	init_term(Var, TermList, I, Ein, E, TopLvl),
	init_terms(Vars, TermList2, I, E, Eout, TopLvl),
	% TODO: try to make this tail recursive
	append(TermList, TermList2, VarTerms).

%init_term(Var, TermList, I, Ein, Eout, TopLvl).
init_term(c(ID, HORef, TopLvl), [new_ref(const(ID, 1), HORef)], _I, E, E, TopLvl) :- !.
init_term(c(_ID, _HORef, _TopLvl), [], _I, E, E, true).
init_term(c(_ID, _HORef, _TopLvl), [], _I, E, E, false).

init_term(v(_ID, HORef, TopLvl), [new_var_name(A), new_ref(var(A, I), HORef)], I, E, E, TopLvl) :- !.
init_term(v(_ID, _HORef, _TopLvl), [], _I, E, E, true).
init_term(v(_ID, _HORef, _TopLvl), [], _I, E, E, false).

%quantified variables
init_term(eqv(_ID, HORef), [new_var_name(A), new_ref(var(A, I), HORef)], I, E, E, _TopLvl).
init_term(uqv(_ID, Sym, HORef), [new_ref(const(Sym, I), HORef)], I, E, E, _TopLvl).

%tied (quantified) variables
init_term(tv(_ID, HORef, Key, _TRef), GoalList, I, Ein, Eout, true) :-
	GoalList = [new_atom(Key), 
					new_ref(var(Key, I), ERef),
					put_env(Key, ERef, Ein, Eout),
					new_ref(tvar(Key), HORef)
					].
init_term(tc(_ID, Sym, HORef, _TRef), GoalList, I, Ein, Eout, true) :-
    GoalList = [
                    new_ref(const(Sym, I), ERef),
					put_env(Sym, ERef, Ein, Eout),
					new_ref(tvar(Sym), HORef)
                    ].

init_term(tv(_ID, _HORef, Key, TRef), [new_ref(tvar(Key), TRef)], _I, E, E, false).
init_term(tc(_ID, Sym, _HORef, TRef), [new_ref(tvar(Sym), TRef)], _I, E, E, false).


% free variable from head was never "tied". unifying the HORef used
% to reference this var in the body with the arg standing in in the head
% will make them the same in the output code
init_term(fv(_ID, X, X, _TopLvl), [], _I, E, E, true).
init_term(fv(_ID, X, X, _TopLvl), [], _I, E, E, false).

% debruin indices
init_term(db(_ID), [], _I, E, E, _TopLvl).



%-------------------------------------------------------------------------------%
%%%%%%%%%%%%%%%%%%%%%%%% utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO: add line number of error if possible/applicable
error_msg(Msg) :-
	error_message(Msg), nl.


trim_true([], []).
trim_true([Term], [Term]).
trim_true([Term, true], [Term]) :- !.
trim_true([Term1, Term2 | Rest], [Term1 | Trimmed]) :-
	trim_true([Term2 | Rest], Trimmed).


% TODO: could make this a flag or something to use prettier name
% or just new_atom
new_const_name('$VAR'(_N), Name) :- !,
	new_const_name_('$uqv', Name).

new_const_name(Base, Name) :-
	new_const_name_(Base, Name).


new_const_name_(Base, Name) :-
	const_name(Base), !,
	% slow and grossly inefficient.
	make_new_const_name(Base, 1, Name).

new_const_name_(Base, Base) :-
	assertz_fact(const_name(Base)).


% make_new_const_name(Base, N, Name) :-
%     atom_concat([Base, '$'], Appended),
%     ( const_name(Appended) ->
%         N1 is N + 1,
%         make_new_const_name(Base, N1, Name)
%     ;	assertz_fact(const_name(Appended)),
%         Name = Appended
%     ).

make_new_const_name(Base, N, Name) :-
	atom_concat([Base, '$'], A),
	AltName =.. [A, N],
	( const_name(AltName) ->
		N1 is N + 1,
		make_new_const_name(Base, N1, Name)
	;	assertz_fact(const_name(AltName)),
		Name = AltName
	).
