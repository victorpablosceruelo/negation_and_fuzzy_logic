:- module(hmtypes_check_tr, [hmtypes_sentence/3, hmtypes_goal/3], [dcg, assertions]).
:- use_package(hiord). % required by httypes_check_common

:- include(library(hmtypes_check(hmtypes_check_ops))).

:- use_module(library(assertions(assrt_lib)), [assertion_read/9, assertion_body/7]).
%		assertion_body/7, comps_to_goal/3, comps_to_goal/4]).

:- use_module(library(terms_check), [variant/2]).
:- use_module(library(write), [portray_clause/1, numbervars/3, write/1]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(format), [format/2]).
:- use_module(library(sort), [sort/2]).

:- use_module(library(lists)).

% ---------------------------------------------------------------------------
:- doc(section, "The type db at compile time").

% hmt__trusted_predicate(?P, ?N, +Mod)
:- data hmt__trusted_predicate/3.
% hmt__constructor_fact(+Term, -Constructor, +Mod)
:- data hmt__constructor_fact/3.
% hmt__constructor_info(+Term, +Type, -Args, -Types, +Mod)
:- data hmt__constructor_info/5.
% hmt__signature_fact(+Call, -Extra, -Signature, +Mod)
:- data hmt__signature_fact/4.
% hmt__basic_normalize(+Alias, -Type, Mod)
:- data hmt__basic_normalize/3.

cleanup_typedb(Mod) :-
	retractall_fact(hmt__trusted_predicate(_,_,Mod)),
	retractall_fact(hmt__constructor_fact(_,_,Mod)),
	retractall_fact(hmt__constructor_info(_,_,_,_,Mod)),
	retractall_fact(hmt__signature_fact(_,_,_,Mod)),
	retractall_fact(hmt__basic_normalize(_,_,Mod)).

% ---------------------------------------------------------------------------
:- doc(section, "(Compile time) instance of the type check algorithm").

:- include(library(hmtypes_check(hmtypes_check_common))).
:- include(library(hmtypes_check(functor_constraint))).
% (required definition for 'hmtypes_check_common')
hmt__constructor(Term, ExpectedType, Mod, EnvIn, EnvOut) :-
	current_fact(hmt__constructor_fact(Term, Constructor, Mod)),
	!,
	( equate_types(ExpectedType,Type, Mod) ->
	    true
	; term_type_error(Term,ExpectedType,Type,Mod)
	),
	Term        =.. [_|Args],
	Constructor =.. [_|Types],
	hmt__args_body(Args, Types, Mod, EnvIn, EnvOut).
	
% (required definition for 'hmtypes_check_common')
hmt__signature(Call, Signature, Mod, EnvIn, EnvOut) :-
	current_fact(hmt__signature_fact(Call, Extra, Signature, Mod)),
	!,
	functor(Signature,Name,Arity),
	functor(Call,Name,Arity),
	Call      =.. [_|Args],
	Signature =.. [_|Types],
	hmt__extra(Extra, Mod, EnvIn, EnvIn2),
	hmt__args_body(Args, Types, Mod, EnvIn2, EnvOut).

hmt__extra(none, _Mod, EnvIn, EnvIn).
hmt__extra(equate_types(Type, Other), Mod, EnvIn, EnvIn) :-
	equate_types(Type, Mod, Other).
hmt__extra(numeric(Type), Mod, EnvIn, EnvIn) :-
	numeric_type(Type, Mod).

hmt__args_body([],[],_Mod,Env,Env).
hmt__args_body([Term|Terms],[Type|Types],Mod,EnvIn,EnvOut) :-
	type_check_term(Term,Type,Mod,EnvIn,Env),
	hmt__args_body(Terms,Types,Mod,Env,EnvOut).

% ---------------------------------------------------------------------------

% clause_to_check(-Clause, -Mod).
:- data clause_to_check/2.

% Clauses that define type information at runtime
% type_clause(-Clause, -Mod).
:- data type_clause/2.

% ===========================================================================
:- doc(section, "Type checker options").

:- data type_check_opt/2.

set_option(Opt, Mod) :-
	functor(Opt, F, N),
	functor(Opt0, F, N),
	retractall_fact(type_check_opt(Opt0, Mod)),
	assertz_fact(type_check_opt(Opt, Mod)).

cleanup_options(Mod) :-
	retractall_fact(type_check_opt(_, Mod)).

% TODO: a hack... options are losts before goal/clause expansion
type_checking_runtime(_Mod) :- !. % TODO: hack
%type_checking_runtime(Mod) :- type_check_opt(runtime(on), Mod), !.
type_checking_verbose(Mod) :- type_check_opt(verbose(on), Mod), !.
type_checking(Mod)    :- type_check_opt(check(on), Mod), !.

initial_options(Mod) :-
	cleanup_options(Mod), % TODO: ideally this should not be necessary
	% TODO: use prolog flags?
	set_option(runtime(off), Mod),
	set_option(check(on), Mod),
	set_option(verbose(on), Mod).

handle_options(List, Mod) :- maplist(handle_option(Mod),List).

handle_option(verbose(Flag), Mod) :- !, set_option(verbose(Flag), Mod).
handle_option(runtime(Flag), Mod) :- !, set_option(runtime(Flag), Mod).
handle_option(check(Flag), Mod)   :- !, set_option(check(Flag), Mod).
handle_option(Other, _)	     :- format('Unsupported type checker option `~w\'.\n',[Other]).

% ===========================================================================
% Goal expansion {{{
hmtypes_goal(UnsafeCall :: Signature, SafeCall, Mod) :- !,
	( type_checking_runtime(Mod) ->
	    % Already at end_of_file when this code get executed.
	    % prolog_load_context(file,File),
	    % prolog_load_context(term_position,'$stream_position'(_, LineNumber, _, _, _)),
	    % format('Expanding annotation in ~w at line ~w\n',[File,LineNumber]),
	    functor(UnsafeCall,F,A),
	    functor(Signature,F,A),	
	    UnsafeCall =.. [_|Args],
	    Signature  =.. [_|Types],
	    compile_args_body(Args,Types,Mod,Guard,[],_),
	    SafeCall = ( UnsafeCall, ( Guard -> true ; throw(runtime_type_error(UnsafeCall)) ) )
	; SafeCall = UnsafeCall
	).
hmtypes_goal(UnsafeCall :< _Signature, Call, _Mod) :- !,
	Call = UnsafeCall.
hmtypes_goal(type_to_any(X,Y), (X = Y), _Mod).
hmtypes_goal(any_to_type(X,Y,Type), Goal, Mod) :-
	( type_checking_runtime(Mod) ->
	    Goal = (hmtypes_check_rt:type_check_term(X,Type,Mod,[],_), X=Y)
	; Goal = (X = Y)
	).
% }}}

% ===========================================================================
% Sentence translation

hmtypes_sentence(0, [], Mod) :- !,
	% initialization of the translation for module Mod
	cleanup_typedb(Mod),
	initial_options(Mod).
hmtypes_sentence((:- hm_type_check_options(Options)), [], Mod) :- !,
	handle_options(Options, Mod).
hmtypes_sentence((:- runtime_hm_type_check(Flag)),[], Mod) :- !,
	%writeln(type_checking_runtime(Flag)),
 	handle_option(runtime(Flag), Mod).	
hmtypes_sentence((:- trust_hm_pred Signature), Clauses, Mod) :- !,
	Clauses = [SignatureClause],
	signature_clause(Signature,Mod,SignatureClause),
	signature_extra(Signature, _, Signature2),
	functor(Signature2,P,N),
	assertz_fact(hmt__trusted_predicate(P, N, Mod)).
hmtypes_sentence((:- _), _Mod, _) :- !, fail. % ignore the rest of declarations
% ---
% TODO: This part is ugly: post process clauses later!
hmtypes_sentence(end_of_file, Clauses2, Mod) :- !,
	type_check_file(Mod, ClausesA, Ok),
	gen_runtime_clauses(Mod, ClausesB),
	list_concat([ClausesA, ClausesB, [end_of_file]], Clauses2),
	cleanup_options(Mod),
	cleanup_typedb(Mod),
	% todo: is there any way to stop compilation here if Ok is not
        % 'yes'? (module_error/0 affects the next compilation phase) (jfmc)
	Ok = yes.
hmtypes_sentence(Clause, NClause, Mod) :-
	type_checking(Mod),
	clause_head(Clause, Head),
	functor(Head, F, A),
	pred_def_type(F, A, Mod),
	!,
%	display(user_error, tycla___(Clause)), nl(user_error),
	typedef_from_clause(Clause, Def),
	add_hm_type(Def, Clauses, Mod),
	( member(C, Clauses),
	    assertz_fact(type_clause(C, Mod)),
	    fail
	; true
	),
%	display(user_error, tydef___(Def)), nl(user_error),
	% Keep the original clause (to avoid warnings...)
	NClause = Clause. 
hmtypes_sentence(Clause, NClause, Mod) :-
	type_checking(Mod),
	clause_head(Clause, Head),
	functor(Head, F, A),
	pred_def_welltyped(F, A, Mod),
	!,
	( participating_predicate(Head, Mod) ->
	    NClause = []
	; % not yet participating? process assertion
	  signature_from_assertion(F, A, Mod, Sig2),
	  signature_clause(Sig2,Mod,NClause)
	),
	assertz_fact(clause_to_check(Clause, Mod)).
% }}}

check_type_assertion(Signature, Prevdef, M) :-
	functor(Signature, F, A0),
	A is A0 + 1,
	functor(Pred0, F, A),
	( assertion_read(Pred0, M, Status, Type, ABody, Dict, _S, _LB, _LE),
	    display('[assertion for type] '),
	    write(Prevdef),
	    nl,
	    wa(Pred0, Status, Type, ABody, Dict, M),
	    fail
	; true %display(type__no_assertion_for(Signature)), nl
	).

pred_def_type(F, A, Mod) :-
	pred_has_gp(prop, hmtype(_), F, A, Mod).

pred_def_welltyped(F, A, Mod) :-
	pred_has_gp(pred, hmtyped(_), F, A, Mod).

% TODO: This is very innefficient
% Check if a given predicate has a global property
pred_has_gp(Type, Prop, F, A, Mod) :-
	functor(Pred0, F, A),
	( assertion_read(Pred0, Mod, _Status, Type, ABody, _Dict, _S, _LB, _LE),
	  assertion_body(_PD,_DP,_CP,_AP,GP,_CO,ABody),
	  member(Prop, GP) ->
	    true
	; fail
	).

wa(PD,Status,Type,Body,_Dict,M) :-
	assertion_body(PD,DP,CP,AP,GP,CO,Body),
	io_aux:message(['(in module ',M,':)']),
	io_aux:message([':- ',Status,' ',Type,' ',PD,
                       ' :: ',DP,' : ',CP,' => ',AP,' + ',GP,' # ',CO]).

add_hm_type(alias(Alias, Type), Clauses, Mod) :-
	% Add a type alias (type Name is normalized to type Type)
	Clauses = [Clause],
	basic_normalize_clause(Alias,Type,Mod,Clause).
add_hm_type(constructor(Name, Constructors), Clauses, Mod) :-
	% Add constructors for a type
	( \+ \+ ( numbervars(Name, 0, _), ground(Constructors) ) ->
	    constructor_clauses(Constructors, Name, Mod, Clauses, [])
	; format("ERROR: invalid TYPE definition~w\n\tType definitions must be range-restricted!\n",
	         [(:- hm_type Name ---> Constructors)]),
	  Clauses = []
	).

constructor_clauses((A;B), Type, Mod) --> !,
	constructor_clauses(A, Type, Mod),
	constructor_clauses(B, Type, Mod).
constructor_clauses(Constructor, Type, Mod) -->
	% Assert in the type db
	{ assertz_fact(hmt__constructor_fact(Constructor, Type, Mod)) },
	% Generate code
	{ functor(Constructor, Name, Arity),
	  functor(Term, Name, Arity),
	  Term        =.. [_|Args],
	  Constructor =.. [_|Types],
	  compile_args_body(Args,Types,Mod,Body,EnvIn,EnvOut)
	},
	[ ( '$hmt__constructor'(Term, ExpectedType, Mod, EnvIn, EnvOut) :-
		( hmtypes_check_rt:equate_types(ExpectedType,Type,Mod) ->
			true
		;
			hmtypes_check_rt:term_type_error(Term,ExpectedType,Type,Mod)
		),
		Body
	  )
	],
	constructor_info_clause(Constructor, Type, Mod).

constructor_info_clause(Constructor, Type, Mod) -->
	% Assert in the type db
	{ functor(Constructor, Name, Arity),
	  functor(Term, Name, Arity),
	  Term        =.. [_|Args],
	  Constructor =.. [_|Types],
	  assertz_fact(hmt__constructor_info(Term, Type, Args, Types, Mod))
	},
	% Generate code	      
	['$hmt__constructor_info'(Term, Type, Args, Types, Mod)].

compile_extra(none, _Mod, Body, Body).
compile_extra(equate_types(Type, Other), Mod, Body, Body0) :-
	Body = (hmtypes_check_rt:equate_types(Type,Mod,Other),Body0).
compile_extra(numeric(Type), Mod, Body, Body0) :-
	Body = (hmtypes_check_rt:numeric_type(Type,Mod),Body0).
				
compile_args_body([],[],_Mod,true,Env,Env).
compile_args_body([Term|Terms],[Type|Types],Mod,(hmtypes_check_rt:type_check_term(Term,Type,Mod,EnvIn,Env),Body),EnvIn,EnvOut) :-
	compile_args_body(Terms,Types,Mod,Body,Env,EnvOut).

signature_clause(Signature, Mod, Clause) :-
	( check_signature(Signature, Mod) ->
		signature_clause_(Signature, Mod, Clause)
	;
		true
	).

signature_clause_(Signature0, Mod, Clause) :-
	signature_extra(Signature0, Extra, Signature),
	functor(Signature,Name,Arity),
	functor(Call,Name,Arity),
	% Assert in the type db
	assertz_fact(hmt__signature_fact(Call, Extra, Signature, Mod)),
	% Generate code
	Call      =.. [_|Args],
	Signature =.. [_|Types],
	compile_extra(Extra, Mod, Body, Body0),
	compile_args_body(Args,Types,Mod,Body0,EnvIn,EnvOut),
	Clause = ( '$hmt__signature'(Call, Types,Mod,EnvIn,EnvOut) :- Body ).

check_signature(Signature, _Mod) :- var(Signature), !, fail.
check_signature(Signature0, Mod) :-
	signature_extra(Signature0, _Extra, Signature),
	functor(Signature,Name,Arity),
	functor(Prototype,Name,Arity),
	( current_fact(hmt__signature_fact(Prototype, _, _, Mod)) ->
		duplicate_signature_error(Signature),
		fail
	;
		true
	).

signature_extra(extra(Extra, Signature), Extra, Signature) :- !. 
signature_extra(Signature, none, Signature).

% HM-like signatures from assertions
% Note: see @pred{typedef_from_clause/2}
signature_from_assertion(Name, Arity, Mod, Signature) :-
	functor(Pred0,Name,Arity),
	( assertion_read(Pred0, Mod, _Status, Type, ABody, _Dict, _S, _LB, _LE),
	  Type = pred,
	  assertion_body(PD,DP,_CP,_AP,_GP,_CO,ABody) ->
	    PD = Signature,
	    scan_dp(DP)
	; true
	).

scan_dp([]).
scan_dp([X|Xs]) :- scan_body(X, _), scan_dp(Xs).

% HM-like type definitions from clauses
% Note:
%   - the language to define type definitions may be more restrictive than that
%     defining regular types.
%   - this predicate may be buggy!
typedef_from_clause((H :- B), Typedef) :- !,
	typedef_from_clause_(H, B, Typedef).
typedef_from_clause(H, Typedef) :-
	typedef_from_clause_(H, true, Typedef).

typedef_from_clause_(H, B, Typedef) :-
	% Recognize a type constructor
	%   h(T(X_1,...,X_n),X_1,...X_n) :- ..., p_k(Xi,Y_k), ...
	%   ===>
        %   :- type h(Xi) ---> T(..., p_k, ...)
        %
	% (there can exist more than one type constructor)
	% TODO: Compare with GADT in functional programming
	H =.. [N,T|Xs],
	Name =.. [N|Xs],
	nonvar(T), Constructor = T,
	scan_body(B, Extra),
	display(ext(Extra)), nl,
	!,
	Typedef = constructor(Name, Constructor).
typedef_from_clause_(H, B, Typedef) :-
	% Recognize a type alias
	%   h(R,X_1,...X_n) :- ..., p(R,...), ...
	%   ===>
        %   :- type h(Xi) == p(...)
	% TODO: There should be just one type alias! Give errors otherwise
	H =.. [N,T|Xs],
	Name =.. [N|Xs],
	var(T), Type = T,
	scan_body(B, _),
	!,
	Typedef = alias(Name, Type).
typedef_from_clause_(H, B, _Typedef) :-
	format("ERROR: invalid clause TYPE definition~w\n",
	       [(H :- B)]).

% Scan the body of a type definition to build the type term
% TODO: This is an ugly hack that binds results with types; which gives good results
%       for trivial cases but fails to identify wrong types
scan_body(A, _) :- var(A), !, fail.
scan_body((_A ; _B), _) :- !, fail. % not supported yet
scan_body((A, B), Extra) :- !, scan_body(A, Extra), scan_body(B, Extra).
scan_body(true, _) :- !.
scan_body(call(T, X), _) :- !, X = T.
scan_body(T = T2, Extra) :- var(T), !, Extra = equate_types(T, T2).
scan_body(T2 = T, Extra) :- var(T), !, Extra = equate_types(T, T2).
scan_body(G, _) :-
	G =.. [Tn|XsR],
%	append(Xs, [R], XsR),
	XsR = [R|Xs],
	T =.. [Tn|Xs],
	R = T.

% ===========================================================================

gen_runtime_clauses(Mod, Clauses) :-
	findall(Clause, retract_fact(type_clause(Clause, Mod)), Clauses).

% ===========================================================================
:- use_module(library(compiler(c_itf_internal)), [module_error/0]).

type_check_file(Mod, NClauses, Ok) :-
	findall(Clause,retract_fact(clause_to_check(Clause, Mod)),Clauses),
	( type_checking(Mod) ->
	    type_check_clauses(Clauses, Mod, Stats),
	    ( type_checking_runtime(Mod) ->
	        transform_clauses(Clauses, Mod, NClauses)
	    ; NClauses = Clauses
	    ),
	    final_message(Stats, Mod),
	    Ok = yes
	; NClauses = [],
	  Ok = no
	).

type_check_clauses(Clauses, Mod, Stats) :-
	init_stats(Stats0),
	type_check_clauses_(Clauses, Mod, Stats0, Stats).

type_check_clauses_([], _Mod, Stats, Stats).
type_check_clauses_([Clause|Clauses], Mod, Stats0, Stats) :-
	catch(
		( type_check_clause(Clause, Mod), 
		  inc_ok_stats(Stats0,Stats1)
		)
	     ,  type_error
	     ,  ( format('TYPE ERROR in clause: \n\n',[]),
	  	  portray_clause(Clause),
		  inc_error_stats(Stats0,Stats1)
		)
	     ),
	type_check_clauses_(Clauses, Mod, Stats1, Stats).

type_check_clause((:- _), _Mod) :- !,
	true. % ignoring
type_check_clause((Head :- Body), Mod) :- !,
	functor(Head,P,N),
	( hmt__trusted_predicate(P, N, Mod) ->
	    true
	; type_check_clause_main(Head, Body, Mod)
	).
type_check_clause(Head, Mod) :- 
	type_check_clause((Head :- true), Mod).

type_check_clause_main(Head, Body, Mod) :-
	Env0 = [], 
        /* check the head */ 
        catch(
		hmt__signature(Head,ArgTypes,Mod,Env0,Env1),
		error(type_error(Term,Exp,Inf,_)),
		(head_error(Term,Exp,Inf,Head,Body), fail)
	),
	/* check the body */
	catch( 
	        type_check_control(Body,ctx(top, ':-', [Head], []),Mod,Env1,_Env2),
		error(type_error(Term,Exp,Inf,_),Context),
		(control_error(Term,Exp,Inf,Mod,Context), fail)
	),
	/* check whether Head is variant of signature */
	functor(Head,Name,Arity),
	functor(Prototype,Name,Arity),
	hmt__signature(Prototype,ProtoTypes,Mod,Env0,_ProtoEnv),
	( variant(ArgTypes, ProtoTypes) -> % =@=/2 in SWI
	    true
	; InfSig =.. [Name|ArgTypes],
	  DecSig =.. [Name|ProtoTypes],
	  less_polymorphic_error((:- hm_pred InfSig),
	  			 (:- hm_pred DecSig)), 
	  throw(type_error)
	). 

% {{{
final_message(tc_stats(E,T), Mod) :-
	( T > 0, type_checking_verbose(Mod) -> 
	    write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl,
	    format('% Type checking for module `~w\' done:\n%\tfound type errors in ~w out of ~w clauses.\n',[Mod,E,T]),
	    ( E == 0 ->
	        write('%\tWell-typed code can\'t go wrong!'), nl
	    ; true
	    ),
	    write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl
	; true
	).

init_stats(tc_stats(0,0)).

inc_error_stats(tc_stats(E,T),tc_stats(NE,NT)) :-
	NE is E + 1,
	NT is T + 1.

inc_ok_stats(tc_stats(E,T),tc_stats(E,NT)) :-
	NT is T + 1.
% }}}

% ===========================================================================

clause_head((Head :- _), Head0) :- !, Head = Head0.
clause_head(Head, Head).

% ===========================================================================
% Transform clauses: 	{{{
%
% 	p(T1,...,Tn) :- B.
%
% becomes: 
%
%	p(X1,...,Xn) :- CHECKS, p'(X1,...,Xn).	(* one for each predicate *)
%
%	p'(T1,...,Tn) :- B'.
% 
% where all calls to type safe predicates have been replaced in B to get B'

transform_clauses(Clauses,Mod,NClauses) :-
	wrappers(Clauses,Mod,NClauses).

wrappers(Clauses,Mod,NClauses) :-
	findall(FA,(member(Clause,Clauses),
	            clause_head(Clause,Head),
		    functor(Head,F,A),
		    FA = F/A),FAs0),
	sort(FAs0,FAs),
	maplist(wrapper_clause(Mod),FAs,WrapperClauses),
	maplist(tc_clause(Mod),Clauses,TcClauses),
	append(WrapperClauses,TcClauses,NClauses).

% PORTRAY_CLAUSE bug?
%
% 6 ?- functor(Head,p,2), Clause = (Head :- signature(Head)), portray_clause(Clause).
% p(_, _) :-
%         signature(p(_, _)).

wrapper_clause(F/A,Mod,Clause) :-
	functor(Head,F,A),
	tc_head(Head, Mod, Call),
	Clause = (Head :- '$hmt__signature'(Head,_,Mod,[],_), Call).

tc_clause((Head :- Body), Mod, (TcHead :- TcBody)) :- !,
	tc_head(Head, Mod, TcHead),
	tc_body(Body, Mod, TcBody).
tc_clause(Head, Mod, TcHead) :-
	tc_head(Head, Mod, TcHead).

tc_head(Head, _Mod, TcHead) :-
	Head =.. [F|Args],
	atom_concat('$tc_',F,NF),
	TcHead =.. [NF|Args].

% TODO: generalize
tc_body(Var,_Mod,TcBody) :- var(Var), !, TcBody = Var.
tc_body((G1,G2),Mod,TcBody) :- !,
	TcBody = (TcG1,TcG2),
	tc_body(G1,Mod,TcG1),
	tc_body(G2,Mod,TcG2).
tc_body((G1;G2),Mod,TcBody) :- !,
	TcBody = (TcG1;TcG2),
	tc_body(G1,Mod,TcG1),
	tc_body(G2,Mod,TcG2).
tc_body((G1->G2),Mod,TcBody) :- !,
	TcBody = (TcG1 -> TcG2),
	tc_body(G1,Mod,TcG1),
	tc_body(G2,Mod,TcG2).
tc_body(Body, Mod, TcBody) :-
	functor(Body,P,N),
	\+ hmt__trusted_predicate(P, N, Mod),
	participating_predicate(Body, Mod), !,
	tc_head(Body, Mod, TcBody).
tc_body(Body, _Mod, TcBody) :-
	TcBody = Body.

% }}}

% ===========================================================================

basic_normalize_clause(Alias,Type,Mod,Clause) :-
	% Assert in the type db
	assertz_fact(hmt__basic_normalize(Alias, Type, Mod)),
	% Generate code
	Clause = '$hmt__basic_normalize'(Alias,Type,Mod).

% ===========================================================================
:- doc(section, "Error Messages").

head_error(Term,ExpType,InfType,Head,Body) :-
	( numbervars([ExpType, Term, InfType, Head, Body], 0, _),
	  format('TYPE ERROR: expected type `~w\' for term `~w\'\n',[ExpType,Term]),
	  format('            inferred type `~w\'\n',[InfType]),
	  format('\tin head `~w\' of clause:\n',[Head]),
	  portray_clause((Head :- Body)),
	  fail
	; true
	).

less_polymorphic_error(InfSig,DecSig) :-
	( numbervars([InfSig, DecSig], 0, _),
	  %numbervars(InfSig,0,_),
	  %numbervars(DecSig,0,_),
	  format('TYPE ERROR: Inferred signature is less polymorphic than declared signature.\n',[]), 
	  format('            inferred signature `~p\'\n',[InfSig]),
	  format('            declared signature `~p\'\n',[DecSig]),
	  fail
	; true
	).

duplicate_signature_error(Signature) :-
	( numbervars([Signature], 0, _),
	  format('TYPE ERROR: Predicate already has a signature.\n',[]), 
	  format('            duplicate signature `~w\'\n',[Signature]),
	  format('            Ignoring duplicate signature.\n',[]),
	  fail
        ; true
        ).

% control_error(+term,+type,+type,+module,+context) {{{ 
control_error(Term,ExpType,InfType,Mod,context(Source,Context)) :-
	mark_error(Mod),
	assemble_marked_body(Context,'*HERE*'(Source),MarkedClause),
	( numbervars([ExpType, Term, InfType, Source, MarkedClause], 0, _),
	  format('TYPE ERROR: expected type `~w\' for term `~w\'\n',[ExpType,Term]),
	  format('            inferred type `~w\'\n',[InfType]),
	  format('\tin goal:\n\t\t ~w\n\tin clause:\n',[Source]),
	  portray_clause(MarkedClause),
	  fail
	; true
	).
% }}}

:- use_module(library(compiler(c_itf_internal)), [module_error/1]).
mark_error(_Mod) :-
	% TODO: error is not propagated to c_itf_internal correctly
	( current_fact(module_error) ->
	    true
	; assertz_fact(module_error)
	).
