:- module(resources_lib,
	    [get_all_resources/1,
		current_resources_by_approx/2,
		get_resources_by_approx/2,
		get_comp_assrt/3,
		get_head_cost_assrt/5,
		get_cost_assrt/5,
		get_literal_cost_assrt/5,
		is_defined_resource/3,
		load_resources_modules/0,
		cleanup_resources_db/0,
		get_measures_assrt/2,
		get_modes_assrt/2,
		remove_module_name/2,
		trust_default/3,
% Granularity Analysis
		get_gran_resources/1
	    ],
	    [assertions,
	     resources(inferres_decl),
	     library(resdefs(resources_decl)), basicmodes,
	     % regtypes, (already in inferres_decl)
	     api(ciaopp_api)]).


:- doc(title,  "Resource assertions processing library").
:- doc(author, "Jorge Navas").
:- doc(author, "Edison Mera").

:- doc(module, "This module defines some predicates which are useful
            for writing programs which process resource assertions.").

:- doc(bug, "1. get_comp_assrt/3 should be called just
                   once. Currently, it is called for each assertion (very
                   inefficient).").

:- doc(bug, "When * assertions be eliminated, this part can be
	          simplified, because it is not necessary to add the module
	          qualifier to the cost function.").

:- use_module(library(messages), [warning_message/2]).
:- use_module(library(terms)).
:- use_module(library(sort)).
:- use_module(library(aggregates),           [findall/3]).
:- use_module(library(lists),                [list_concat/2]).
:- use_module(program(itf_base_db),          [curr_file/2]).
:- use_module(library(compiler),             [use_module/1]).
:- use_module(library(lists),                [append/3, length/2]).
:- use_module(program(clause_db),            [source_clause/3]).
:- use_module(ciaopp(preprocess_flags),      [current_pp_flag/2]).
:- use_module(library(resdefs(rescostfunc)), [compact_cf/3]).
:- use_module(resources(res_assrt_defs(infertime_lib))).
:- use_module(resources(resources_basic)).
:- use_module(ilciao(resources_java), [get_java_resources/1]).
:- use_module(resources(res_assrt_defs(resources_lib_basic))).
:- use_module(resources(init_res(trusted_res))).

:- pred get_all_resources_modules(ResourceModules) :: list(gnd) # "This
	predicate returns all modules containing the modules that defines
	the cost functions dinamically.  We apply sort to avoid duplicates".

get_all_resources_modules(ResourceModules) :-
	findall(R, source_clause(_Key, directive(load_resource_module(R)),
		_Dict), ResourceModules0),
	sort(ResourceModules0, ResourceModules).

trust_default(Approx, Resource, Cost) :-
	source_clause(_Key, directive(trust_default(+(Defaults0))), _Dict),
	get_default(Defaults0, cost(Approx, Resource, Cost)).

:- pred load_resources_modules # "Loads dinamically all the resource
	modules specified in the module being analyzed, using the directive
	load_resource_module".

load_resources_modules :-
	get_all_resources_modules(ResourceModules),
	list(ResourceModules, process_resource_module).

process_resource_module(ResourceModule) :-
	curr_file(CurrFile, _),
	absolute_file_name(CurrFile, '_opt', '.pl', '.', _, _, BaseDir),
	absolute_file_name(ResourceModule, '_opt', '.pl', BaseDir, FileName,
	    FileBase, Dir),
	use_module(FileName),
	atom_concat([Dir, '/', Module], FileBase),
	(init_resource(Module) -> true ; true).

:- pred get_all_resources(R) :: list(resource) # "This predicate returns all
resources in @var{R} defined in a module previously loaded.".

get_all_resources(Ress) :-
	current_pp_flag(prog_lang, ciao), !,
	findall(R, resource(R), Ress0),
	sort(Ress0, Ress).
get_all_resources(Ress) :-
	current_pp_flag(prog_lang, java), !,
	get_java_resources(Ress).

% For granularity analysis
get_gran_resources(Rs) :-
	source_clause(_Key, directive(granularity_resources(Rs_u)), _Dic),
	list(Rs_u, resource),
	sort(Rs_u, Rs).
get_gran_resources([none]).

:- pred current_resources_by_approx(Approximation, Resources) :
	(var * var) => (approx * list(resource)) # "Gives using
	backtracking the list of all @var{Resources} for each
	@var{Approximation}, but only if @var{Resources} is not
	empty.".
% fixed using rtcheck -- EMM
current_resources_by_approx(Approx, Resources) :-
	current_pp_flag(prog_lang, ciao), !,
	approx(Approx),
	get_resources_by_approx(Approx, Resources),
	Resources \== [].
current_resources_by_approx(_, AllResources) :-
	current_pp_flag(prog_lang, java), !,
	get_java_resources(AllResources).

:- pred get_resources_by_approx(Approximation, Resources) :
	(approx * var) => (approx * list(resource)) # "@var{Resources}
	is a list of resources for the specified
	@var{Approximation}.".

get_resources_by_approx(Approx, Resources) :-
	findall(Res, resource_by_approx_prop(Res, Approx), Resources0),
	sort(Resources0, Resources).

resource_by_approx_prop(Res, Approx) :-
	resource(Res),
	current_resource_by_approx_prop(Res, Approx).

head_cost(Approx, Res, Func) :-
	source_clause(_Key, directive(head_cost(Approx, Res, Func)), _Dict).

literal_cost(Approx, Res, Func) :-
	source_clause(_Key, directive(literal_cost(Approx, Res, Func)), _Dict).

comp_assr(Approx, Res, 'resources_props:cost'(_, _, Approx, _, Res, _, _, _)).
comp_assr(Approx, Res, 'resources_props:head_cost'(_, Approx, Res, _)).
comp_assr(Approx, Res, 'resources_props:literal_cost'(_, Approx, Res, _)).

current_resource_by_approx_prop(Res, Approx) :-
	( head_cost(Approx, Res, _) -> true
	; literal_cost(Approx, Res, _) -> true
	;
	    get_assertion(_Pred, as${status => trust, comp =>Comps}),
	    comp_assr(Approx, Res, CompAssr),
	    member(CompAssr, Comps)
	-> true
	;
	    compound_resource(Res, CompRess),
	    list(CompRess, current_resource_by_approx_prop(Approx))
	).

:- pred get_comp_assrt(?Pred, +Status, -Comps) # "This predicate
	returns in @var{Comps} a list of the @tt{''Comp''} assertions
	(conjunction of terms) whose status is defined by @var{Status}
	and associated with the predicate @var{Pred}. @var{Comps}
	contains all @tt{''Comp''} assertions including those from
	different clauses. If @var{Pred} is var then @var{Comps}
	contains all the @tt{''Comp''} assertions of the program. ".

% fixed using rtcheck -- EMM
get_comp_assrt(Pred, Status, Comps) :-
	var(Pred),
	!,
	findall(C, get_assertion(Pred, as${status =>Status, comp =>C}),
	    Comps_list),
	list_concat(Comps_list, Comps),
	assert_assrts_from_directives.
% Pred is expanded with the module name or var !!
get_comp_assrt(Pred, Status, Comps) :-
	findall(C,
	    ( get_assertion(Pred, as${status =>Status, comp =>C}) ;
		get_assertion(Asterisk, as${status =>Status, comp =>C}),
		atom(Asterisk),
		atom_concat(_, ':*', Asterisk) ),
	    Comps_list),
	list_concat(Comps_list, Comps),
	assert_assrts_from_directives.
% Pred is not expanded with the module name or var because it's a builtin !!
%% Review this !!
% :- use_module(ciaopp(p_unit(native.pl))).
% get_comp_assrt(Pred,Status,Comps):-
% 	findall(C,
% 		( get_assertion(Pred,as${status=>Status,comp=>C})),
% 		Comps_list),
% 	list_concat(Comps_list,Comps).

% Pred is expanded !!
:- pred get_head_cost_assrt(+Comps, +Approx, +LitInfo, +Res, -Vals) #
"@var{Vals} is a list of general form expressions indexed by
	the resource @var{Res}, the literal information @var{LitInfo},
	and its approximation @var{Approx} from the
	@tt{head_cost} properties belonging to the list of
	@tt{''Comp''} assertions @var{Comps}. If @var{Pred} is '*'
	then the assertion will be applicable for each predicate
	belonging to the module.".

get_head_cost_assrt(Comps, Approx, LitInfo, Res, Vs) :-
	Pred = '_:*',
	get_resource_assrt_from_directives('resources_props:head_cost',
	    Pred, Res_assrts),
	append(Comps, Res_assrts, Comps0),
	get_resource_assrt(Comps0, 'resources_props:head_cost', Approx,
	    LitInfo, Res, Vs).


% Pred is expanded !!
:- pred get_literal_cost_assrt(+Comps, +Approx, +LitInfo, +Res, -Vals) #
"@var{Vals} is a list of general form expressions indexed by
	the resource @var{Res}, the literal information @var{LitInfo},
	and its approximation @var{Approx} from the @tt{literal_cost}
	properties belonging to the list of @tt{''Comp''} assertions
	@var{Comps}.  If @var{Pred} is '*' then the assertion will be
	applicable for each predicate belonging to the module.".

get_literal_cost_assrt(Comps, Approx, LitInfo, Res, Vs) :-
	Pred = '_:*',
	get_resource_assrt_from_directives('resources_props:literal_cost',
	    Pred, Res_assrts),
	append(Comps, Res_assrts, Comps0),
	get_resource_assrt(Comps0, 'resources_props:literal_cost', Approx,
	    LitInfo, Res, Vs).

get_resource_assrt([],     _,    _,      _,       _,   []).
get_resource_assrt([P|Ps], Type, Approx, LitInfo, Res, Vs) :-
	litinfo_get_lit(LitInfo, Pred),
	(
	    P =.. [Type, Pred0, Approx, Res, Cost_Pred],
	    (Pred0 = '_:*' ; Pred0 = Pred) ->
	    eval_cost_lit(Cost_Pred, LitInfo, Value),
% 	    translate_assrt_function_to_gen_form(Pred, _,
% 		resource(Value),
% 		resource(GenFormProp)),
	    GenFormProp = Value,
	    Vs = [GenFormProp|Vs1]
	;
	    Vs = Vs1
	),
	!,
	get_resource_assrt(Ps, Type, Approx, LitInfo, Res, Vs1).

:- pred eval_cost_lit(+Cost_Pred, +Lit, -Value) # "@var{Cost_Pred} is the
	functor (not expanded with the module name) of a
	meta-predicate which has always two arguments: its
	corresponding clause and a general form expression which is
	the result of evaluating some operation over the clause
	defined by the meta-predicate associated with @var{Cost_Pred}.
	@var{Lit} is the literal (The Head or in the Body) over which
	apply @var{Cost_Pred}, and @var{Value} is the result of such
	evaluation.".

eval_cost_lit(Cost, _, Cost) :-
	num(Cost),
	!.
eval_cost_lit(CostPred, LitInfo, Value) :-
	get_mod_pred(CostPred, _, P),
% 	curr_file(Mod, _),
% 	use_module(Mod),
	Head3 =.. [P, LitInfo, Value],
	call(_:Head3),
	!.
eval_cost_lit(Head, LitInfo, _) :-
	error_message("The second argument of the property head_cost
	or literal_cost corresponding to ~q can not be executed: ~w", [Head,
		LitInfo]).

% Pred is expanded !!
:- pred get_cost_assrt(+Comps, +Approx, +Pred, +Res, -Vals)
# "@var{Vals} is a list of general form expressions indexed by the resource
@var{Res}, the predicate @var{Pred}, and its approximation @var{Approx}
from the @tt{cost} properties belonging to the list of
@tt{''Comp''} assertions @var{Comps}.".

% :- test get_cost_assrt(['resources_props:cost'(*, ub, steps,
% 	resource(0.5*exp(length(A),2)+1.5*length(A)+1) )], ub,
% 	'nrev_assrt:nrev'(A,B), steps, Vals) => ( Vals ==
% 	[resource(0.5*exp($(0,1),2)+1.5* $(0,1)+1)] ) # "Example
% 	converting assrt to gen form.".


get_cost_assrt(Comps, Approx, LitInfo, Res, Vals) :-
	litinfo_get_lit(LitInfo, Lit),
	get_cost_assrt_(Comps, Approx, LitInfo, Lit, Res, Vals).

get_cost_assrt_([],     _,      _,       _,    _,   []).
get_cost_assrt_([P|Ps], Approx, LitInfo, Pred, Res, [GenFormProp|Vs]) :-
	P = 'resources_props:cost'(Pred, _, Approx, _, Res, _, IF, CFN),
	!,
	compact_cf(CFN, IF, CF),
	translate_assrt_function_to_gen_form(Pred, _,
	    resource(CF), resource(GenFormProp)),
	get_cost_assrt_(Ps, Approx, LitInfo, Pred, Res, Vs).
get_cost_assrt_([_|Ps], Approx, LitInfo, Pred, Res, Vs) :-
	!,
	get_cost_assrt_(Ps, Approx, LitInfo, Pred, Res, Vs).

% Head is expanded with the module name
:- pred is_defined_resource(+Head, +Res, +Approx) # "Succeeds if a resource
@var{Res} and type @var{Approx} has been defined by the user for the
predicate @var{Head}.".

is_defined_resource(Head, Res, Approx) :-
	get_comp_assrt(Head, trust, Comps),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 	functor(Head,F,A),
% 	get_mod_pred(F,_,F0),
% 	functor(Head0,F0,A),!,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	is_defined_resource_assrt(Comps, Approx, Res, Head).

% Head is expanded !!
:- pred is_defined_resource_assrt(+Comps, +Approx, +Res, Head).
is_defined_resource_assrt([P|_], Approx, Res, Head) :-
	( P = 'resources_props:head_cost'('_:*', Approx, Res, _)
	; P = 'resources_props:cost'('_:*', _, Approx, _, Res, _, _, _)
	; P = 'resources_props:literal_cost'('_:*', Approx, Res, _)
	; P = 'resources_props:head_cost'(Head, Approx, Res, _)
	; P = 'resources_props:cost'(Head, _, Approx, _, Res, _, _, _)
	; P = 'resources_props:literal_cost'(Head, Approx, Res, _)
	), !.
is_defined_resource_assrt([_|Ps], Approx, Res, Head) :-
	is_defined_resource_assrt(Ps, Approx, Res, Head).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE: perhaps you only need:

% translate_assrt_function_to_gen_form_(Goal,AssrtExp,GenFormExp):-
%         Goal=..[_|Args],
%         create_dict(Args,0,Dic),
% 	rename_assrt_function(AssrtExp,Dic,GenFormExp).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicates for supporting assertions defined through directives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% only for Java analysis
:- data java_mode/2.
:- data java_measure/2.

:- data directives_read/0.

cleanup_resources_db :-
	retractall_fact(java_mode(_, _)),
	retractall_fact(java_measure(_, _)),
	retractall_fact(directives_read).

assert_assrts_from_directives :-
	directives_read, !.
assert_assrts_from_directives :- !,
% head_cost and literal_cost
	( % measures and modes (only for Java analysis)
	    current_pp_flag(prog_lang, java) ->
	    findall(java_measure(F/A, Measures),
		source_clause(_Key, directive(java_measure(F/A, Measures)),
		    _Dict), Measure_assrts),
	    findall(java_mode(F/A, Modes),
		source_clause(_Key, directive(java_mode(F/A, Modes)),
		    _Dict),       Mode_assrts),
	    asserta_all_if_java(Measure_assrts),
	    asserta_all_if_java(Mode_assrts)
	;
	    true
	),
	assertz_fact(directives_read).

current_resource_assrt('resources_props:head_cost', '_:*', Approx, Resource,
	    Cost_Func) :-
	head_cost(Approx, Resource, Cost_Func).
current_resource_assrt('resources_props:literal_cost', '_:*', Approx, Resource,
	    Cost_Func) :-
	literal_cost(Approx, Resource, Cost_Func).

% JNL (12/11/07)
% several measures for the same predicate is allowed.
% several modes for the same predicate NOT.
asserta_all_if_java([]).
asserta_all_if_java([X|Xs]) :-
	assert_if_more_specific(X),
	asserta_all_if_java(Xs).

assert_if_more_specific(java_measure(F/A, Measures_New)) :-
	java_measure(F/A, Measures_Old), !,
	apply_more_specific_measure(Measures_Old, Measures_New, F/A, Measures),
	retract_fact(java_measure(F/A, _)),
	assertz_fact(java_measure(F/A, Measures)).
assert_if_more_specific(java_measure(F/A, Measures)) :-
	assertz_fact(java_measure(F/A, Measures)).

assert_if_more_specific(java_mode(F/A, Modes)) :-
	java_mode(F/A, Modes), !,
	error_message("More than one mode assertions for ~q/~q", [F, A]),
	!, fail.
assert_if_more_specific(java_mode(F/A, Modes)) :-
	assertz_fact(java_mode(F/A, Modes)).

% apply_more_specific_measure(+Measures_Old,+Measures_New,+F/A,-Measures)
% Note this predicate is not the glb of the measures. 

apply_more_specific_measure([],         [],        _,   []).
apply_more_specific_measure([void|Ms0], [_M1|M1s], F/A, [void|Ms]) :-
% warning_message("Downgrading a measure ~q in ~q/~q",[M1,F,A]),
	apply_more_specific_measure(Ms0, M1s, F/A, Ms).
apply_more_specific_measure([_M0|Ms0], [void|M1s], F/A, [void|Ms]) :-
% warning_message("Downgrading a measure ~q in ~q/~q",[M0,F,A]),
	apply_more_specific_measure(Ms0, M1s, F/A, Ms).
apply_more_specific_measure([int|Ms0], [_M1|M1s], F/A, [int|Ms]) :-
	apply_more_specific_measure(Ms0, M1s, F/A, Ms).
apply_more_specific_measure([_|Ms0], [int|M1s], F/A, [int|Ms]) :-
	apply_more_specific_measure(Ms0, M1s, F/A, Ms).
apply_more_specific_measure([M0|Ms0], [M1|M1s], F/A, [M0|Ms]) :-
	M0 == M1, !,
	apply_more_specific_measure(Ms0, M1s, F/A, Ms).
apply_more_specific_measure([M0|Ms0], [M1|M1s], F/A, [M0|Ms]) :-
	error_message("The predicate ~q/~q has incompatible measures ~q \= ~q",
	    [F, A, M0, M1]), !,
	apply_more_specific_measure(Ms0, M1s, F/A, Ms).


get_resource_assrt_from_directives(Type, Pred, Assrts) :-
	findall(Assrt,
	    ( current_resource_assrt(Type, Pred, Approx, Resource,
		    Cost_Func),
		Assrt =.. [Type, Pred, Approx, Resource, Cost_Func] ),
	    Assrts).


:- pred get_measures_assrt(+Pred, -Measures).
% only for getting measures from Java programs
get_measures_assrt(F/A, Measures) :-
	remove_module_name(F, F0),
	assert_assrts_from_directives,
	java_measure(F0/A, Measures),
	check_java_measures(F/A, Measures).

:- pred get_modes_assrt(+Pred, -Modes).
% only for getting modes from Java programs
get_modes_assrt(F/A, Modes) :-
	remove_module_name(F, F0),
	assert_assrts_from_directives,
	java_mode(F0/A, Modes),
	check_java_modes(F/A, Modes).

check_java_measures(_/A, Measures) :-
	length(Measures, A),
	java_measure(Measures).
check_java_measures(F/A, _) :-
	warning_message("Invalid Java measure info for ~q", [F/A]).

check_java_modes(_/A, Modes) :-
	length(Modes, A),
	java_mode(Modes).
check_java_modes(F/A, _) :-
	warning_message("Invalid Java mode info for ~q", [F/A]).

remove_module_name(ModPred, Pred) :-
	atom_concat([_, ':', Pred], ModPred), !.
remove_module_name(Pred, Pred).

:- regtype java_measure/1.
java_measure([]).
java_measure([void|Ms]) :-
	java_measure(Ms).
java_measure([int|Ms]) :-
	java_measure(Ms).
java_measure([size|Ms]) :-
	java_measure(Ms).

:- regtype java_mode/1.
java_mode([]).
java_mode([+|Ms]) :-
	java_mode(Ms).
java_mode([-|Ms]) :-
	java_mode(Ms).
