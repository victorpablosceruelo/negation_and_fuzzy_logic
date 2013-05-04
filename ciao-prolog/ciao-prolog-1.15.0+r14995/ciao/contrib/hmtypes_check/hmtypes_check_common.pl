% Template module for HM type checking
%
% This pseudo-module requires the definition of the following
% predicates:
%
%   hmt__constructor_fact(+Term, -Constructor, +Mod)
%   hmt__constructor_info(?Term, ?Type, ?Args, ?Types, +Mod).
%   hmt__signature(?Call,?Types,+EnvIn,-EnvOut,+Mod).
%   hmt__basic_normalize(+Alias,-Type, +Mod)

% (factored out from Tom's type_check)
% --jfmc

:- use_module(library(when), [when/2]).
:- use_module(library(iso_misc), [unify_with_occurs_check/2]).
:- use_module(library(write), [portray_clause/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (exported)
participating_predicate(Head,Mod) :-
	functor(Head,Name,Arity),
	functor(Test,Name,Arity),
	% todo: use hmt__signature_fact/3
 	hmt__signature(Test,_,Mod,[],_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (exported)	

:- use_module(library(lists), [append/3, length/2, reverse/2]).

type_check_term(Term,ExpectedType,Mod,EnvIn,EnvOut) :-
	normalize_type(ExpectedType,Mod,NormalExpectedType),
	type_check_term_(Term,NormalExpectedType,Mod,EnvIn,EnvOut).

type_check_term_(Term,ExpectedType,Mod,EnvIn,EnvOut) :-
	var(Term), !,
	( lookup_eq(EnvIn,Term,EnvType) ->
		( equate_types(ExpectedType,Mod,EnvType) ->
			true
		;
			term_type_error(Term,ExpectedType,EnvType,Mod)
		),
		EnvIn = EnvOut
	;	
		EnvOut = [Term-ExpectedType|EnvIn]
	).
type_check_term_(Term,Type,Mod,EnvIn,EnvOut) :-
	get_atomic_type(Term,Type0), !,
	( equate_types(Type,Mod,Type0) ->
		EnvIn = EnvOut
	;
		term_type_error(Term,Type0,Type,Mod)
	).
type_check_term_(Term,Type,Mod,EnvIn,EnvOut) :-
	Type == atm, !, % Replaced 'string' by 'atm' --JFMC
	( atom(Term) -> EnvIn = EnvOut
	; term_type_error(Term,unknown_type,Type,Mod)
	).
type_check_term_(_Term,Type,_Mod,EnvIn,EnvOut) :-
	Type == any, !,
	EnvIn = EnvOut.
type_check_term_(Term,Type,Mod,EnvIn,EnvOut) :-
	nonvar(Type),
	Type =.. [pred|ArgTypes], !,
	extend_env(DummyArgs, ArgTypes, EnvIn2, EnvIn),
	get_pa_fullgoal(Term, DummyArgs, Goal),
	type_check_control(Goal,top,Mod,EnvIn2,EnvOut).
%
type_check_term_(Term,Type,Mod,EnvIn,EnvOut) :-
	% hmt__constructor(Term,Type,Mod,EnvIn,EnvOut), !.
%	normalize_type(Type,Mod,NormalType),
	functor_constraint(Term,Type,Mod,Args,Types), !,
	type_check_terms(Args,Types,Mod,EnvIn,EnvOut).
%
type_check_term_(Term,Type,Mod,_EnvIn,_EnvOut) :-
	term_type_error(Term,unknown_type,Type,Mod).

type_check_terms([],[],_Mod,Env,Env).
type_check_terms([Term|Terms],[Type|Types],Mod,Env1,Env3) :-
	type_check_term(Term,Type,Mod,Env1,Env2),
	type_check_terms(Terms,Types,Mod,Env2,Env3).

% (exported)	
term_type_error(Term,ExpectedType,EnvType,Mod) :-
	throw(error(type_error(Term,ExpectedType,EnvType,Mod))).

extend_env([], [], EnvIn, EnvIn) :- !.
extend_env([A|As], [T|Ts], [A-T|EnvIn], EnvIn2) :-
	extend_env(As, Ts, EnvIn, EnvIn2).

get_pa_fullgoal(Term, DummyArgs, Goal) :-
	% TODO: This ignores the module system
	% treatment of predicate abstractions in Ciao
	% (just for runtime)
	Term = '$:'(Term0), nonvar(Term0),
	Term0 = 'PA'(A, B, C),
	A =.. [Functor|_],
	B =.. [_|DummyArgs],
	C =.. [_|FullArgs],
	Goal =.. [Functor|FullArgs].
get_pa_fullgoal(Term, DummyArgs, Goal) :- !,
	Term =.. [Functor|Args],
	add_args(Args,DummyArgs,FullArgs),
	Goal =.. [Functor|FullArgs].

% TODO: This uses the (weird) Ciao argument application order!
add_args(Args0, [A0|Args1], [A0|Args]) :- !,
	append(Args0, Args1, Args).
add_args(Args0, Args1, Args) :- !,
	append(Args0, Args1, Args).

% ---------------------------------------------------------------------------
% Type of some atomic terms

get_atomic_type(Term, Type) :- integer(Term), !, Type = integer.
get_atomic_type(Term, Type) :- float(Term), !, Type = float.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% context ::=
%	  top
%	| ctx(Parent, Name, Before, After)
%       | lconj(parent, right)
%	| rconj(parent, left)
%	| ldisj(parent, right)
%	| rdisj(parent, left)
%	| cond(parent, then)
%	| then(parent, cond)
%	| once(parent)
%	| findall(parent, pattern, result)

% env == list(pair(var-type))

% TODO: Problem: this uses fake meta-predicate information! 
%
%       Move translation as a clause translation so that I can read
%       the real meta-pred (or move as a special goal translation with
%       a global logical env...).
%
%       Ideally, one may want to redefine the problem in a similar way
%       than analysis are formulated for basic blocks in imperative
%       programming (e.g.  forward dataflow analysis on a call-flow
%       graph, etc.)
%
%       See:
%         http://www.haskell.org/haskellwiki/Curry-Howard-Lambek_correspondence
%
% (JFMC)

%% type_check_control(+goal,+context,+mod,+env,-env) {{{
type_check_control(G,Context,Mod,Env1,Env2) :- var(G), !,
	type_check_control(call(G),Context,Mod,Env1,Env2).
%
type_check_control(Goal,Context,Mod,Env0,Env2) :-
	% Check meta-arguments
	( functor(Goal, N, A),
	  functor(Meta, N, A),
	  meta_args(Meta) ->
	    Meta =.. [_|MetaArgs],
	    Goal =.. [_|Args],
	    type_check_meta_args(MetaArgs, Args, N, Context, Mod, Env0, Env1)
	; Env1 = Env0
	),
        % Then type check the arguments
	catch(
		( type_check_goal(Goal,Mod,Env1,Env2,Warnings,[]) ->
			maplist(control_warning(context(Goal,Context)),Warnings)	
		;
			term_type_error(?,?,?,Mod)
		),
		error(type_error(Term,ExpType,InfType,_)),
		control_type_error(type_error(Term,ExpType,InfType,Mod),Goal,Context)
	).

% TODO: What about predicate abstractions?
meta_args((goal,goal)).
meta_args((goal;goal)).
meta_args((goal->goal)).
meta_args(once(goal)).
meta_args(findall(?,goal,?)).
meta_args(catch(goal,?,goal)).
% incomplete...

% Check the type of goal meta-arguments
type_check_meta_args(MTs, As, N, Context, Mod, Env0, Env) :-
	type_check_meta_args_(MTs, As, [], N, Context, Mod, Env0, Env).

type_check_meta_args_([], [], _, _, _, _, Env, Env).
type_check_meta_args_([MT|MTs], [A|As], BeforeRev, N, Context, Mod, Env0, Env) :-
	( MT = goal ->
	    Context2 = ctx(Context, N, BeforeRev, As),
	    type_check_control(A,Context2,Mod,Env0,Env1)
	; Env1 = Env0
	),
	BeforeRev2 = [A|BeforeRev],
	type_check_meta_args_(MTs, As, BeforeRev2, N, Context, Mod, Env1, Env).

control_type_error(Error,Goal,Context) :-
	throw(error(Error,context(Goal,Context))).
% }}}

% TODO: This seems like a type class! (JFMC)
%     The assertion for this could be:
%
%       :- pred X > Y :: num(T), call(T,X) ?
%     or 
%       :- pred X > Y :: (num(T) ==> call(T,X)) ?
% 
%     I am not sure about what operator should I use: conjunction or
%     implication (take ==> as the real implication)?  (depends that
%     on the quantifier for T?) I should have a look at the
%     Curry-Howard correspondence with more detail.

numeric_type(Type, Mod) :-
	when(nonvar(Type),check_numeric_type(Type, Mod)).

check_numeric_type(integer, _Mod) :- !.
check_numeric_type(float, _Mod) :- !.
check_numeric_type(Other, Mod) :- 
	term_type_error(some_arithmetic_expression, a_numeric_type, Other, Mod).
% }}}	

% type_check_goal(+goal,+env,-env,-warnings,+warnings_tail) {{{
type_check_goal((X is Y),Mod,Env1,Env3,W,W) :- !,
	numeric_type(Type, Mod),
	type_check_term(X,Type,Mod,Env1,Env2),
	type_check_expression(Y,Type,Mod,Env2,Env3).
% These are builtins that need special treatment in the type domain
type_check_goal(arg(I,_,_),Mod,Env1,Env2,W,W) :- !,
	% TODO: This could be improved
	type_check_term(I,integer,Mod,Env1,Env2).
type_check_goal(functor(Term,Functor,I),Mod,Env1,Env3,W,W) :- !,
	type_check_term(I,integer,Mod,Env1,Env2),
	( nonvar(I) -> I >= 0 ; true ),
	type_check_term(Term,Type,Mod,Env2,Env3),
	( nonvar(Functor) ->
		atomic(Functor),
		( nonvar(I), nonvar(Type) ->
			functor(Dummy,Functor,I),
			hmt__constructor(Dummy,Type,Mod,[],_Env)
		;
			true
		)	
	;
		true % ignore functor
	).
%
type_check_goal(Goal,Mod,Env1,Env3,W,W) :- functor(Goal, call, N0), N0 > 0, !,
	Goal =.. [_,PA|Args],
	N is N0 - 1,
	functor(PAType, pred, N),
	PAType =.. [_|ArgTypes],
	type_check_term(PA,PAType,Mod,Env1,Env2),
	type_check_terms(Args,ArgTypes,Mod,Env2,Env3).
%
type_check_goal((Goal :: Signature), Mod, Env1, Env3,W,W) :- !,
	/* first take into account the predicate signatures
	   if one exists 				    */
	( participating_predicate(Goal,Mod) ->
	    hmt__signature(Goal,_,Mod,Env1,Env2)
	; Env2 = Env1
	),
	functor(Goal,F,A),
	functor(Signature,F,A),
	Goal      =.. [_|Args],
	Signature =.. [_|Types],
	type_check_terms(Args,Types,Mod,Env2,Env3).	
type_check_goal(Goal :< Signature, Mod,Env1, Env3,W,W) :- !,
	/* first take into account the predicate signatures
	   if one exists 				    */
	( participating_predicate(Goal,Mod) ->
	    hmt__signature(Goal,_,Mod,Env1,Env2)
	; Env2 = Env1
	),
	functor(Goal,F,A),
	functor(Signature,F,A),
	Goal      =.. [_|Args],
	Signature =.. [_|Types],
	type_check_terms(Args,Types,Mod,Env2,Env3).	
% TODO: Does this needs dependent types (Type is both argument and a type)? JFMC
type_check_goal(any_to_type(A1,A2,Type),Mod,Env1,Env3,W,W) :- !,
	type_check_term(A1,any,Mod,Env1,Env2),
	type_check_term(A2,Type,Mod,Env2,Env3).
%
type_check_goal(Goal,Mod,Env1,Env2,W,W) :- 
	participating_predicate(Goal,Mod), !,
	hmt__signature(Goal,_,Mod,Env1,Env2).
%
type_check_goal(Goal,_Mod,Env1,Env2,W,RW) :-
	/* all other predicates are simply ignored */
	Warning = unknown_predicate_call(Goal),
 	W = [Warning|RW],		
	Env1 = Env2.
% }}}

% type_check_expression(+expression,+type,+mod,+env,-env) {{{
type_check_expression(Exp,Type,Mod,Env1,Env2) :-
	var(Exp), !,
	type_check_term(Exp,Type,Mod,Env1,Env2).
%type_check_expression(random,Type,Env1,Env1) :- !,  % NOTE: only supported by Yap
%	equate_types(Type,Mod,float).
type_check_expression((-Exp),Type,Mod,Env1,Env2) :- !,
	type_check_expression(Exp,Type,Mod,Env1,Env2).	
type_check_expression((\Exp),Type,Mod,Env1,Env2) :- !,
	equate_types(Type,Mod,integer),
	type_check_expression(Exp,Type,Mod,Env1,Env2).	
type_check_expression(abs(Exp),Type,Mod,Env1,Env2) :- !,
	type_check_expression(Exp,Type,Mod,Env1,Env2).	
type_check_expression(log(Exp),Type,Mod,Env1,Env2) :- !,
	equate_types(Type,Mod,float),
	type_check_expression(Exp,Type,Mod,Env1,Env2).	
type_check_expression(integer(Exp),Type,Mod,Env1,Env2) :- !,
	equate_types(Type,Mod,integer), % explicit conversion from float to integer
	type_check_expression(Exp,float,Mod,Env1,Env2).	
type_check_expression(sign(Exp),Type,Mod,Env1,Env2) :- !,
	type_check_expression(Exp,Type,Mod,Env1,Env2).	
type_check_expression((Exp1+Exp2),Type,Mod,Env1,Env3) :- !,
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1-Exp2),Type,Mod,Env1,Env3) :- !, 
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1*Exp2),Type,Mod,Env1,Env3) :- !,
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1/Exp2),Type,Mod,Env1,Env3) :- !,
	equate_types(Type,Mod,float),
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1//Exp2),Type,Mod,Env1,Env3) :- !,
	equate_types(Type,Mod,integer),
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1**Exp2),Type,Mod,Env1,Env3) :- !,
	equate_types(Type,Mod,float),
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1 mod Exp2),Type,Mod,Env1,Env3) :- !, 
	equate_types(Type,Mod,integer),
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression(min(Exp1,Exp2),Type,Mod,Env1,Env3) :- !, 
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression(max(Exp1,Exp2),Type,Mod,Env1,Env3) :- !, 
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1 >> Exp2),Type,Mod,Env1,Env3) :- !, 
	equate_types(Type,Mod,integer),
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1 << Exp2),Type,Mod,Env1,Env3) :- !, 
	equate_types(Type,Mod,integer),
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1 /\ Exp2),Type,Mod,Env1,Env3) :- !, 
	equate_types(Type,Mod,integer),
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression((Exp1 \/ Exp2),Type,Mod,Env1,Env3) :- !, 
	equate_types(Type,Mod,integer),
	type_check_expression(Exp1,Type,Mod,Env1,Env2),	
	type_check_expression(Exp2,Type,Mod,Env2,Env3).	
type_check_expression(Exp,Type,Mod,Env1,Env2) :-
	/* catch all */
	type_check_term(Exp,Type,Mod,Env1,Env2).
% }}}

unify_args([],[],_Mod,Env,Env).
unify_args([X|Xs],[Y|Ys],Mod,Env1,Env3) :-
	type_check_goal((X = Y), Mod, Env1, Env2,_,[]),
	unify_args(Xs,Ys,Mod,Env2,Env3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
normalize_type(Type0,Mod,Type2) :-
	( nonvar(Type0), hmt__basic_normalize(Type0,Type1,Mod) ->
		normalize_type(Type1,Mod,Type2)
	;
		Type2 = Type0
	).

% (exported)	
equate_types(Type1,Mod,Type2) :-
	( nonvar(Type1), nonvar(Type2) ->
		normalize_type(Type1,Mod,NType1),
		normalize_type(Type2,Mod,NType2),
		functor(NType1,Functor,Arity),
		functor(NType2,Functor,Arity),
		NType1 =.. [_|Args1],
		NType2 =.. [_|Args2],
		maplist(equate_types(Mod),Args1,Args2)
	;
		unify_with_occurs_check(Type1,Type2)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Utility {{{

lookup_eq([K - V | KVs],Key,Value) :-
        ( K == Key ->
                V = Value
        ;
                lookup_eq(KVs,Key,Value)
        ).
snd_of_pairs([],[]).
snd_of_pairs([_-Y|XYs],[Y|Ys]) :-
        snd_of_pairs(XYs,Ys).
% }}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% BUGS FOUND {{{
% ==========
%
% In rbtrees.pl (Vitor Santos Costa)
%	
%	:- pred rb_lookupall(K,V,rbtree(K,V)).
%	:- pred lookupall(K,V,tree(K,V)).
%	:- pred lookupall(cmp,K,V,tree(K,V)).
%	
%	rb_lookupall(Key, Val, t(_,Tree)) :-
%		lookupall(Key, Val, Tree).
%	
%	lookupall(_, _, black(nil,_,_,nil)) :- !, fail.
%	lookupall(Key, Val, Tree) :-
%		getkey(Tree,KA),		% arg(2,Tree,KA),
%		compare(Cmp,KA,Key),
%		lookupall(Cmp,Key,Val,Tree).
%	
%	lookupall(>, K, V, Tree) :-
%		getright(Tree,NTree),		% arg(4,Tree,NTree),
%		rb_lookupall(K, V, NTree).	% BUG !!!
%	lookupall(=, _, V, Tree) :-
%		getvalue(Tree,V),		% arg(3,Tree,V).
%	lookupall(=, K, V, Tree) :-
%		getleft(Tree,NTree),		% arg(1,Tree,NTree),
%		lookupall(K, V, NTree).
%	lookupall(<, K, V, Tree) :-
%		getleft(Tree,NTree),		% arg(1,Tree,NTree),
%		lookupall(K, V, NTree).
%
% }}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%  NOTES {{{ 
%  =====
%
% In rbtrees.pl (Vitor Santos Costa)
%  * cannot share Nil in old and new tree:
% 
%	:- pred rb_clone(rbtree(K,_V1),rbtree(K,V2),list(pair(K,V2))).
%	rb_clone(t(_Nil1,T),t(Nil2,NT),Ns) :-
%		new(Nil2),
%		clone(T,NT,Ns,[]).	
%
% }}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% ERROR MESSAGES (compile and runtime)

control_warning(Warning,Context) :- % note: Ciao inserts first implicit argument in the second position!
	control_warning_(Warning,Context).

control_warning_(unknown_predicate_call(Call), context(Source,Context)) :-
	format('TYPE WARNING: call to unknown predicate `~w\'\n',[Call]),
	format('    Possible Fixes: - add type annotation `::\' to call\n',[]),
	format('                    - replace with call to other predicate\n',[]),
	assemble_marked_body(Context,'HERE'(Source),MarkedClause),
	portray_clause(MarkedClause).

% assemble_marked_body(+context,+goal,-goal) {{{
assemble_marked_body(top,Acc,Body) :- Body = Acc.
assemble_marked_body(ctx(Context,Name,BeforeRev,After),Acc,Body) :-
	reverse(BeforeRev, Before),
	append(Before, [Acc|After], Args),
	NAcc =.. [Name|Args],
	assemble_marked_body(Context,NAcc,Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (maplist/n family for Ciao)
:- push_prolog_flag(multi_arity_warnings, off).

:- meta_predicate maplist(pred(1), ?).
:- meta_predicate maplist(pred(2), ?, ?).
maplist(P, As) :- maplist_(As, P).
maplist_([], _).
maplist_([A|As], P) :- P(A), maplist_(As, P).

maplist(P, As, Bs) :- maplist_(As, Bs, P).
maplist_([], [], _).
maplist_([A|As], [B|Bs], P) :- P(A, B), maplist_(As, Bs, P).

:- pop_prolog_flag(multi_arity_warnings).
