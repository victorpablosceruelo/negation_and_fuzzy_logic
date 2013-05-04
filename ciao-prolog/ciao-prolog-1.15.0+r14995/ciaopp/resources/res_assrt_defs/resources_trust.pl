:- module(resources_trust,
	    [
		apply_resource_assertion/7,
		min_gen_form/3,
		max_gen_form/3,
%		regtypes
		res_assrt_t/1
	    ],
	    [assertions,
	     resources(inferres_decl),
	     library(resdefs(resources_decl)),
	     % regtypes, (already in inferres_decl)
	     basicmodes, api(ciaopp_api)]).

:- doc(title,  "Trust assertions processing library for resources").
:- doc(author, "Jorge Navas").
:- doc(author, "Edison Mera").

:- doc(module, " This module defines predicates which process the
trust assertions for resource properties.").

:- use_module(library(hiordlib)).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(resources(resources_basic)).
:- use_module(ilciao(resources_java), [get_java_cost/3]).

:- use_module(resources(res_assrt_defs(resources_lib)),
	    [
		get_comp_assrt/3,
		get_head_cost_assrt/5,
		get_cost_assrt/5,
		trust_default/3,
		get_literal_cost_assrt/5
	    ]).

:- use_module(library(messages), [warning_message/2]).
:- use_module(resources(algebraic_res(maxmin_res)),
	    [max_expr/3, min_expr/3]).
:- use_module(resources(algebraic_res(normal_form_res)),  [normal_form/2]).
:- use_module(resources(algebraic_res(general_form_res)), [general_form/2]).
:- use_module(library(aggregates)).
:- use_module(resources(top_res(utility_res)),
	    [vector_multiply/3, vector_addition/2]).

:- check pred apply_resource_assertion/7 + not_fails.

:- regtype res_assrt_t/1 # "Resource assertion types.".

res_assrt_t(cost).
res_assrt_t(head).
res_assrt_t(literal).

% type_to_kind(cost,    usage).
% type_to_kind(head,    delta).
% type_to_kind(literal, call ).

:- pred apply_resource_assertion(+Resources, +Type, ?BT, +Approx,
	    +LitInfo, ?Expr0, -Expr) :: res_assrt_t(Type) % rtcheck -- EMM
#

"@var{Expr0} is a vector of @var{Approx}-bound resource expressions
associated with each resource defined in @var{Resources} for the
@var{LitInfo} predicate and obtained from user assertions of type
@var{Type}.".

apply_resource_assertion(Resources, Type, BT, Approx, LitInfo, Expr0, Expr) :-
	( var(Expr0) ->
	    apply_trusted_resources0(Resources, Type, BT, Approx, LitInfo,
		Expr0)
	;
	    apply_trusted_resources(Resources, Type, BT, Approx, LitInfo,
		Expr0, Expr)
	),
	!.

% :- pred apply_trusted_resource(+ res_assrt_t(Type), +Approx, +LitInfo, +Res,
% 	    +Expr0, -Expr) # "@var{Expr} is the resource expression of
% 	type @var{Type} for @var{Head} predicate and @var{Res}
% 	resource, when analysis results @var{Expr0} are
% 	available.".

% apply_trusted_resource(Type, Approx, LitInfo, Res, Expr0, Expr) :-
% 	trusted_resource(Expr0, Type, Approx, Res, LitInfo, Expr1),
% 	apply_glb_resource_inferred(Approx, LitInfo, Expr0, Expr1, Expr).

:- pred apply_trusted_resources(+Resources, +Type, ?BT, +Approx,
	    +LitInfo, +Exprs0, -Exprs) : ( list(Resources, resource),
	    res_assrt_t(Type) ).
apply_trusted_resources([], _Type, _BT, _Approx, _LitInfo, [], []).
apply_trusted_resources([Resource|Resources], Type, BT, Approx, LitInfo,
	    [LitTime0|LitTimes0], [LitTime|LitTimes]) :-
	current_pp_flag(prog_lang, ciao), !,
	trusted_resource(LitTime0, Type, BT, Approx, Resource, LitInfo,
	    LitTime1),
	apply_glb_resource_inferred(Approx, LitInfo, LitTime0, LitTime1,
	    LitTime),
	apply_trusted_resources(Resources, Type, BT, Approx, LitInfo,
	    LitTimes0, LitTimes).
apply_trusted_resources([Resource|Resources], Type, BT, Approx, LitInfo,
	    [LitTime0|LitTimes0], [LitTime|LitTimes]) :-
	current_pp_flag(prog_lang, java), !,
	trusted_java_resource(Type, Resource, LitInfo, LitTime1),
	apply_glb_resource_inferred(Approx, LitInfo, LitTime0, LitTime1,
	    LitTime),
	apply_trusted_resources(Resources, Type, BT, Approx, LitInfo,
	    LitTimes0, LitTimes).

:- pred apply_trusted_resource0(+Res, +Type, ?BT, +Approx, +LitInfo,
	    -Expr) : res_assrt_t(Type) + not_fails % rtcheck -- EMM
# "@var{Expr} is the resource expression of type
	@var{Type} for @var{LitInfo} predicate and @var{Res} resource,
	when no analysis results are available.".

apply_trusted_resource0(Resource, Type, BT, Approx, LitInfo, Expr) :-
	current_pp_flag(prog_lang, ciao), !,
	trusted_resource([], Type, BT, Approx, Resource, LitInfo, Expr).
apply_trusted_resource0(Resource, Type, _, _, LitInfo, Expr) :-
	current_pp_flag(prog_lang, java), !,
	trusted_java_resource(Type, Resource, LitInfo, Expr).

:- pred apply_trusted_resources0(+Resources, +Type, ?BT, +Approx,
	    +LitInfo, -Exprs) : res_assrt_t(Type) + not_fails. % rtcheck -- EMM

apply_trusted_resources0(Resources, Type, BT, Approx, LitInfo, Exprs) :-
	map(Resources, apply_trusted_resource0(Type, BT, Approx, LitInfo),
	    Exprs).

:- use_module(resources(res_assrt_defs(infertime_lib))).

:- pred trusted_resource(+Expr0, +Type, ?BT, +Approx, +Res,
	    +LitInfo, -Expr) : res_assrt_t(Type) % fixed using rtcheck -- EMM
# "Gets the resource expression for @var{Res}
	of type @var{Approx} given by the user (which we ''trust'')
	which is applicable to @var{LitInfo}".

trusted_resource(_Exprs, Type, _BT, Approx, Resource, LitInfo, Expr) :-
	\+ compound_resource(Resource, _),
	!,
	trusted_resource_(Type, Approx, Resource, LitInfo, Expr).
trusted_resource(Exprs, Type, BT, Approx, AllResource, LitInfo, Expr) :-
	compound_resource(AllResource, Resources),
	!,
	trusted_compound_resource(Exprs, Type, BT, Approx, AllResource,
	    Resources, LitInfo, Expr).
trusted_resource(Exprs, Type, _BT, Approx, Resource, LitInfo, Expr) :-
	litinfo_get_lit(LitInfo, Lit),
	default_cost(Exprs, Type, Approx, Resource, Lit, Expr).

trusted_compound_resource(_, Type, _BT, Approx, AllResource, _Resource,
	    LitInfo, Expr) :-
	trusted_resource__(Type, Approx, AllResource, LitInfo, Head, Exprs),
	Exprs \== [],
	!,
	apply_glb_resource_inferred_each(Exprs, Type, Approx, AllResource,
	    Head, Expr).

trusted_compound_resource(Exprs0, Type, BT, Approx,
	    AllResource, Resources, LitInfo, Expr) :-
	findall(Expr0,
	    (
		member(Resource, Resources),
		trusted_resource(Exprs0, Type, BT, Approx, Resource, LitInfo,
		    Expr0)
	    ),
	    Exprs),
	(
	    get_platform(BT, Platform),
	    platform_constants(Platform, AllResource, Approx, Constants) ->
	    vector_multiply(Constants, Exprs, ExprS)
	;
	    vector_addition(Exprs, ExprS) % Asume a simple sumatory
	),
	litinfo_get_lit(LitInfo, Head),
	apply_glb_resource_inferred_each([ExprS], Type, Approx, AllResource,
	    Head, Expr),
	!.

trusted_resource_(Type, Approx, Resource, LitInfo, Expr) :-
	trusted_resource__(Type, Approx, Resource, LitInfo, Head, Exprs),
	apply_glb_resource_inferred_each(Exprs, Type, Approx, Resource, Head,
	    Expr),
	!.

trusted_resource__(Type, Approx, Res, LitInfo, Head, Exprs) :-
	litinfo_get_lit(LitInfo, Head),
	get_comp_assrt(Head, trust, Comps),
% 	functor(Head,F,A),
% 	get_mod_pred(F,_,F0),
% 	functor(Head0,F0,A),
	do_get_resource_assrt(Type, Comps, Approx, LitInfo, Res, Exprs).

do_get_resource_assrt(head, Comps, Approx, LitInfo, Res, Exprs) :- !,
	get_head_cost_assrt(Comps, Approx, LitInfo, Res, Exprs).
do_get_resource_assrt(literal, Comps, Approx, LitInfo, Res, Exprs) :- !,
	get_literal_cost_assrt(Comps, Approx, LitInfo, Res, Exprs).
do_get_resource_assrt(cost, Comps, Approx, LitInfo, Res, Exprs) :- !,
	get_cost_assrt(Comps, Approx, LitInfo, Res, Exprs).

trusted_java_resource(head, _,        _,       0) :- !. % no delta function applicable in java analysis
trusted_java_resource(_,    Resource, LitInfo, Expr) :-
	litinfo_get_key(LitInfo, ClauseKey),
	litinfo_get_litnum(LitInfo, Num),
	( Num == 0 ->
	    ( unmake_clause_key(ClauseKey, _) ->
% ClauseKey is of the form (p/A/B) (all block methods)
% User needs to define (at least) annotations for builtins
% and external calls.
		get_module_pred(ClauseKey, _, Key),
		get_java_cost(Resource, Key, Expr)
	    ;
% ClauseKey is of the form (p/A)   
% Builtins and external calls initialized to infinity (no code available)
		Expr = inf
% atom_concat([ClauseKey,'/','1'],Key),
% get_java_cost(Resource,Key,Expr)
	    )
	;
% 'p/A/ClKey/LitKey'   % no literal function 
	    atom_number(ANum, Num),
	    atom_concat(ClauseKey,  '/',  ClauseKey0),
	    atom_concat(ClauseKey0, ANum, LitKey),
	    get_module_pred(LitKey, _, Key),
	    get_java_cost(Resource, Key, Expr)
	).
trusted_java_resource(_, Resource, LitInfo, _) :-
	litinfo_get_key(LitInfo, LitKey),
	error_message(
	    "There is no cost function for the resource ~q and literal ~q",
	    [Resource, LitKey]).

% java_builtin_or_external(LitInfo):-
% 	litinfo_get_lit(LitInfo,Lit),
% 	functor(Lit,F,_),
% 	get_module_pred(F,M,_),!,
% 	java_module(M).

% java_module(blt).
% java_module('java.lang').
% java_module('java.util').

get_module_pred(Key, Mod, Pred) :-
	atom_concat(X,   Pred, Key),
	atom_concat(Mod, ':',  X).

% unmake_literal_key(Key,[F2,A3,A2,A1]):-
% 	unmake_pred_key(Key,[F,A1]),
% 	unmake_clause_key(F,[F2,A2,A3]).

unmake_clause_key(Key, [F2, A1, A2]) :-
	unmake_pred_key(Key, [F1, A1]),
	unmake_pred_key(F1,  [F2, A2]).

unmake_pred_key(Key, [F, A]) :-
	atom_concat(X, AN,  Key),
	atom_concat(F, '/', X),
	atom_number(AN, A).


:- pred apply_glb_resource_inferred(+Approx, +Head, +Expr0,
	    +Expr1, -Expr) # % rtcheck -- EMM
"@var{Expr} is the result of applying the glb
	operation to the expression @var{Expr0} and @var{Expr1}
	corresponding to predicate @var{Head}. @var{Approx} is the
	type of approximation in the resource analysis:
	@tt{ub},@tt{lb}, or @tt{o},".

apply_glb_resource_inferred(ub, Head, Expr0, Expr1, Expr2) :- !,
	apply_glb_resource_inferred_(Head, Expr0, Expr1, Expr2).
apply_glb_resource_inferred(lb, Head, Expr0, Expr1, Expr2) :-
	max_gen_form(Expr0, Expr1, Expr), !,
	( Expr == bot ->
	    warning_message(
		"invalid trust for ~w:~n    ~w~n analysis infers:~n   ~w", [
		    Head, Expr1, Expr0]),
	    Expr2 = Expr1
	;
	    Expr2 = Expr
	).

apply_glb_resource_inferred(o, Head, Expr0, Expr1, Expr2) :- !,
	apply_glb_resource_inferred_(Head, Expr0, Expr1, Expr2).
apply_glb_resource_inferred_(Head, Expr0, Expr1, Expr2) :-
	min_gen_form(Expr0, Expr1, Expr), !,
%  min_gen_form($(2,1)+1,6,$(2,1)+7)  !!
	( Expr == bot ->
	    warning_message(
		"invalid trust for ~w:~n    ~w~n analysis infers:~n   ~w", [
		    Head, Expr1, Expr0]),
	    Expr2 = Expr1
	;
	    Expr2 = Expr
	).

min_gen_form(Ex1, Ex2, GfEx) :-
	normal_form(Ex1, NfEx1),
	normal_form(Ex2, NfEx2),
	min_expr(NfEx1, NfEx2, NfEx),
	general_form(NfEx, GfEx).

max_gen_form(Ex1, Ex2, GfEx) :-
	normal_form(Ex1, NfEx1),
	normal_form(Ex2, NfEx2),
	max_expr(NfEx1, NfEx2, NfEx),
	general_form(NfEx, GfEx).


:- true pred apply_glb_resource_inferred_each(+Expr, +Type,
	    +Approx, +Resource, +Head, -Expr0) : res_assrt_t(Type)
# "@var{Expr0} is the 
	result of applying the glb operation to each expression of the
	list @var{Expr} corresponding to predicate @var{Head}. @var{Approx}
	is the type of approximation in the resource analysis
	@var{Resource}: @tt{ub},@tt{lb}, or @tt{o}.". % rtcheck -- EMM

apply_glb_resource_inferred_each(Exprs, Type, Approx, Resource, Head, Expr) :-
	default_cost(Exprs, Type, Approx, Resource, Head, Expr0),
	apply_glb_resource_inferred_each_(Exprs, Approx, Resource, Head,
	    Expr0, Expr).

apply_glb_resource_inferred_each_([],            _Approx, _Resource, _Head,
	    Expr,  Expr).
apply_glb_resource_inferred_each_([Expr0|Exprs], Approx,  Resource,  Head,
	    Expr1, Expr) :-
	apply_glb_resource_inferred(Approx, Head, Expr0, Expr1, Expr2),
	apply_glb_resource_inferred_each_(Exprs, Approx, Resource, Head,
	    Expr2, Expr).

default_cost([], cost, Approx, Resource, Head, Expr) :-
	trust_default(Approx, Resource, Expr),
	show_message(note, "Using default ~w(~w,~w,~w) in ~w",
	    [cost, Approx, Resource, Expr, Head]),
	!.
default_cost(_Exprs, _Type, Approx, _Resource, _Head, Expr) :-
	approx_bottom(Approx, Expr).
