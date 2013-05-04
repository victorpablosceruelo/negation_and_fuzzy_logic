:- module(resources,
	    [complexity_analysis/5],
	    [assertions,
% regtypes, (already in inferres_decl)
		resources(inferres_decl),
		library(resdefs(resources_decl))]).

% ciao library:
:- use_module(library(messages),
	    [
		error_message/1,
		error_message/2,
		simple_message/2,
		warning_message/2,
		warning_message/1
	    ]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(dynamic),    [retract/1, assert/1]).
:- use_module(library(hiordlib)).
:- use_module(library(llists), [flatten/2]).
% ciaopp library:
:- use_module(ciaopp(preprocess_flags),
	    [set_pp_flag/2, current_pp_flag/2]).
:- use_module(typeslib(regtype_basic_lattice_base), [set_top_type/1]).
:- use_module(infer(infer_db),                      [inferred/3, domain/1]).
:- use_module(infer(infer_dom),                     [knows_of/2]).
:- use_module(infer(vartypes),                      [get_vartype/4]).
:- use_module(spec(s_simpspec),                     [make_atom/2]).
:- use_module(typeslib(type_support),               [closed_var_list/2]).
% own library:
:- use_module(resources(dependency_res(dependency__res)),
	    [dependency_analysis/7]).
:- use_module(resources(determinacy_res(determinacy__res)),
	    [determinacy_analysis/5]).
:- use_module(resources(init_res(gran_init_res)),
	    [cls_init_gran_system/11]).
:- use_module(resources(gran_res(size_rel_res)),
	    [input_arg_size_relation/7]).
:- use_module(resources(size_res(size__res)),         [size_analysis/9]).
:- use_module(resources(time_res),                    [time_analysis/10]).
:- use_module(resources(top_res(error_res)),          [error_message/3]).
:- use_module(resources(solution_res(relation_res)),  [relation_analysis/10]).
:- use_module(resources(solution_res(solution__res)), [solution_analysis/11]).
:- use_module(resources(algebraic_res(compare_res)),  [order_of_function/2]).
:- use_module(resources(res_assrt_defs(resources_lib)),
	    [current_resources_by_approx/2,
		load_resources_modules/0,
		cleanup_resources_db/0,
		get_gran_resources/1
	    ]).
:- use_module(resources(init_res(symtable_res))).

?- error_in_lns(_, _, note, ['Loading resources analysis']).

% for tracing
% :- use_module(resources(dependency_res(trace_ldg)), [trace_ldg/1]).
% :- use_module(resources(dependency_res(trace_adg)), [trace_adg/1]).
% :- use_module(infer(infer)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
%% Only for debugging purposes, uncomment these lines.
%issue_debug_messages(time_res).
%issue_debug_messages(size__res).
%issue_debug_messages(diff_equ_res).
%issue_debug_messages(resources).
%issue_debug_messages(init_res).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------------------------------
:- doc(author, "Pedro L@'{o}pez"||
	    " (based on Caslog, by Nai-Wei Lin)").
:- doc(author, "Edison Mera (recent updates)").

:- doc(bug, "1. Generalize builtins and meta-calls.").
:- doc(bug, "2. Add exact cost analyses.").
:- doc(bug, "3. Use trusts.").


:- doc(module, "This component implements the cost analysis.").

:- doc(appendix, "The (basic) organization of the 
code is as follows (the picture does not show all modules for simplicity):

 @image{resources}

Module @lib{resources} is in charge of interfacing with the rest of CiaoPP.
Modules @lib{dependency}, @lib{determinacy}, @lib{size_rel}, @lib{size},
@lib{solution}, and @lib{time} implement different analyses which are 
instrumental in inferring cost information. The analysis uses mode and measure
information collected previously. It also uses non-failure information
from @lib{infer}. Cost information inferred is stored in @lib{infer_db}.
").

:- push_prolog_flag(multi_arity_warnings, off).

:- use_module(infer(infer_db),     [cleanup_infer_db/1]).
:- use_module(infer(vartypes),     [gather_vartypes/2]).
:- use_module(infer(gather_modes), [gather_modes/4]).
:- use_module(resources(resources_basic)).
:- use_module(resources(resources_register)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hook to dynamic loading of analysis:
:- multifile loaded_analysis/1.
loaded_analysis(Analysis) :- provided_analysis(Analysis).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile analysis/4.

analysis(resources, Cls, Ds, Info) :-
	!,
	analysis_resources(Cls, Ds, Info).

analysis_resources(Cls, Ds, _Info) :-
	analysis_resources_init(Cls, Ds, NewCls, NewDs),
	(
	    current_resources_by_approx(Approx, Resources),
	    verify_nf_if_req(Approx),
	    complexity_analysis(resources, Resources, Approx, NewCls, NewDs),
	    fail
	;
	    true
	).

verify_nf_if_req(ub).
verify_nf_if_req(lb) :-
	( knows_of(not_fails, NFAn), domain(NFAn) -> true
	; warning_message(
		"Failure information not available. "||
		"Skipping lower bound resource analysis.")
	).

analysis_resources_init(Cls, Ds, NewCls, NewDs) :-
	cleanup_infer_db(resources),
	cleanup_infer_db(modes),
	cleanup_infer_db(vartypes),
	cleanup_resources_db,
	gather_vartypes(Cls, _Trusts),
	gather_modes(Cls, Ds, NewCls, NewDs),
	load_resources_modules.

:- pop_prolog_flag(multi_arity_warnings).

:- pred complexity_analysis(+Analysis, +Resources, +Approx, +Clauses, +Dicts) +
	not_fails.

complexity_analysis(Analysis, Resources, Approx, Clauses, Dicts) :-
	current_pp_flag(para_grain, Gr),
	( Gr == none ->
	    complexity_analysis_(Analysis, Resources, Approx, Gr, Clauses,
		Dicts)
	; ( Gr == gr_res ->
		get_gran_resources(Gran_Res_s),
		( Gran_Res_s = [none] ->
		    warning_message(
			"No resources selected for granularity analysis [~q]",
			[Approx])
		;
		    set_pp_flag(para_grain_res, Gran_Res_s),
		    complexity_analysis_(Analysis, Resources, Approx, Gr,
			Clauses, Dicts)
		)
	    ;
		warning_message("The granularity option ~q is not valid", [Gr])
	    )
	),
	!.

is_time_analysis(resources). % EMM

is_size_analysis(size). % EMM

%--------------------------------------------------------------------------

:- check comp complexity_analysis_/6 + not_fails.

complexity_analysis_(Analysis, Resources, Approx, Gr, Clauses, Dicts) :-
	is_time_analysis(Analysis),
	!,
	time_complexity_analysis(Analysis, Resources, Approx, Gr, Clauses,
	    Dicts).
complexity_analysis_(Analysis, Resources, Approx, _Gr, Clauses, Dicts) :-
	is_size_analysis(Analysis),
	term_size_analysis(Analysis, Resources, Approx, Clauses, Dicts).

%--------------------------------------------------------------------------

safe_get_vartype(Key, Goal, Call_VarType, Succ_VarType) :-
	get_vartype(Key, Goal, Call_VarType, Succ_VarType),
	!.
safe_get_vartype(_Key, Goal, Types, Types) :-
	create_worst_case_types(Goal, Types).

create_worst_case_types(Goal, Types) :-
	closed_var_list(Goal, Varlist),
	map(Varlist, assign_term_type_to_variable, Types).

assign_term_type_to_variable(Var, Prop) :-
	( current_pp_flag(prog_lang, ciao) ->
	    set_top_type(F),
	    Prop =.. [F, Var]
	;
	    Prop = Var/top
	).

%--------------------------------------------------------------------------

:- comp time_complexity_analysis/6 + not_fails. % rtcheck -- EMM

:- doc(time_complexity_analysis/6, "Performs the cost analysis of
	the program given as a list, in the fashion determined by the
	@var{Approx} variable. The clause list is in standard
	format but should include @tt{mode} and @tt{measure}
	directives.").
time_complexity_analysis(Analysis, Resources, Approx, Gr, Clauses, Dicts) :-
	statistics(runtime, _),
% ciao:inverse_rewrite_source_program0(Clauses, NClauses),
	approx_to_bound(Approx, Bound),
%	init_trusted_costs(Approx, Resources, ST),
%	init_external_preds(Clauses, Approx, Resources, ST),
	cls_init_gran_system(Clauses, Bound, Dicts, BT, ST, SCCG, GT, GCG,
	    Directs, DirDicts, Error),
% 	analysis_check(SCCG, ST, Error2),
% 	Error is Error1 * Error2,
	(
	    Error =:= 1 ->
	    statistics(runtime, [_, T1]),
	    simple_message(
		"preprocessed for ~w bounds cost analysis in ~w msec.",
		[Approx, T1]),
	    cs_analysis(SCCG, Resources, Approx, Bound, Gr, BT, ST, GT,
		1, Error3),
	    (
		Error3 =:= 1 ->
		statistics(runtime, [_, T2]),
		( current_pp_flag(prog_lang, ciao) ->
		    simple_message(
			"~w bounds cost analysis performed in ~w msec.",
			[Approx, T2])
		;
		    output_java_statistics(Approx)
		),
		!,
		asserta_fact(inferred(Analysis, '$current_punit',
			comp_info(ST, SCCG, GT, GCG, Directs, DirDicts))),
		output_complexity_analysis(Analysis, Approx, Resources, ST),
		statistics(runtime, [_, T3]),
		simple_message(
		    "~w bounds cost information stored in ~w msec.",
		    [Approx, T3])
	    ;
		error_message(nocost, _, _)
	    )
	;
	    error_message(nocost, _, _)
	),
	map(BT, bottom_entry_to_note, Notes0),
	flatten(Notes0, Notes),
	messages(Notes),
	!.

% TODO: Notes must be turned off by default, perhaps must be shown
% only if verbose mode is activated -- EMM

bottom_entry_to_note(bt(PredName, Approx, Type, BTE), [Message|Messages]) :-
	(
	    current_pp_flag(verbosity, Verb),
	    member(Verb, [off, low]) ->
	    Message = []
	;
	    Message =
	    note([Approx, ' of predicate ', ~~(PredName), ' requires ',
		    TypeName, ' information. Assertion recommended.' ])
	),
	functor(Type, TypeName, _),
	map(BTE, bottom_element_to_message, Messages),
	!.
bottom_entry_to_note(bp(Platform),
	    [note(['Using ', Platform, ' as default platform'])]).

message_type(default, note).
message_type(top,     warning).

bottom_element_to_message(bs(C, L, N),
	    message(warning, ['Undefined size for clause ', ~~(C),
		    ', literal ', ~~(L), ', argument ', N])).
bottom_element_to_message(be(Name, PreType, Value), Message) :-
	message_type(PreType, MessageType),
	(
	    MessageType = note,
	    current_pp_flag(verbosity, Verb),
	    member(Verb, [off, low]) ->
	    Message = []
	;
	    Message = message(MessageType, ['Taking ', PreType,
		    ' value for ', Name, ': ', Value])
	).

cs_analysis(_,  _, _, _, _, _, _, _, 0,     0) :- !.
cs_analysis([], _, _, _, _, _, _, _, Error, Error).
cs_analysis([Comp|SCCG], Resources, Approx, Bound, Gr, BT, ST, GT, 1,
	    Error1) :-
	cs_analysis_each(Comp, Resources, Approx, Bound, Gr, BT, ST, GT,
	    Error),
	cs_analysis(SCCG, Resources, Approx, Bound, Gr, BT, ST, GT, Error,
	    Error1).

:- pred cs_analysis_each/9 :: list(predname) * list(resource) * approx * bound
	* term * list(bottom_entry) * list(symbol_entry) * list * nnegint
	+ (not_fails, is_det).
cs_analysis_each(Comp, Resources, Approx, Bound, Gr, BT, ST, GT, Error) :-
	record_java_time(size, start),
	dependency_analysis(Comp, BT, ST, Adg, Ldg, Gvars, Error),
%	trace_adg(Adg),
%	trace_ldg(Ldg),
	(
	    Error =:= 1 ->
	    size_analysis(Comp, Approx, Bound, BT, ST, Comp, Adg, Gvars, Size),
	    record_java_time(size, end),
% Size relations for granularity control transformation.
	    ( Gr == gr_res ->
		input_arg_size_relation(Comp, Bound, BT, ST, Adg, Gvars, GT)
	    ;
		true
	    ),
	    relation_analysis(Comp, Approx, BT, ST, Comp, Size, Adg, Gvars,
		Ldg, _),
	    determinacy_analysis(Comp, Approx, BT, ST, Adg),
	    solution_analysis(Comp, Approx, Bound, BT, ST, Comp, Size, Adg,
		Gvars, Ldg, _),
	    record_java_time(resources, start),
	    time_analysis(Comp, Resources, Approx, BT, ST, Size, Adg, Gvars,
		Ldg, _),
	    record_java_time(resources, end)
	;
	    true
	).

output_complexity_analysis(_Analysis, _Approx, _Resources, ST) :-
	var(ST),
	!.
output_complexity_analysis(Analysis, Approx, Resources, ST) :-
	nonvar(ST),
	ST = [Entry|Rest],
	Entry = st(Pred, _ClauseList, Mode, Measure, Mutex, Solution_Det,
	    Size, Relation, ResourceList, Domain),
	output_complexity_trust_info(Analysis, Approx, Resources, Pred, Mode,
	    Measure, Mutex, Solution_Det, Size, Relation, ResourceList,
	    Domain),
	output_complexity_analysis(Analysis, Approx, Resources, Rest).

output_complexity_trust_info(Analysis, Approx, Resources, Pred, Mode, Measure,
	    Mutex, Solution_Det, Size, Relation, ResourceList, Domain) :-
	Pred = F/A,
	functor(Goal, F, A),
	make_atom([F, A], Key),
	safe_get_vartype(Key, Goal, Call_VarType, Succ_VarType),
	current_symbol_resources(Resources, Cost, ResourceList),
	(
	    Approx == o ->
	    map(Cost, map(order_of_function), Out_Cost),
	    map(Size, order_of_function, Out_Size)
	;
	    Cost = Out_Cost,
	    Size = Out_Size
	),
	asserta_fact(inferred(Analysis, Goal,
		complexity(Call_VarType, Succ_VarType, Mode, Measure, Mutex,
		    Solution_Det, Out_Size, Relation, Approx, Resources,
		    Out_Cost, Domain))).

% 
%-----------------------------------------------------------------------------

term_size_analysis(Size_Analysis, Resources, Approx, Clauses, Dicts) :-
	statistics(runtime, _),
	approx_to_bound(Approx, Bound),
	cls_init_gran_system(Clauses, Bound, Dicts, BT, ST, SCCG, GT, _, _, _,
	    Error),
% 	analysis_check(SCCG, ST, Error2),
% 	Error is Error1 * Error2,
	(
	    Error =:= 1 ->
	    (
		statistics(runtime, [_, T1]),
		simple_message(
		    "preprocessed for ~w bounds size analysis in ~w msec.",
		    [Approx, T1]),
		tsize_analysis(SCCG, Approx, Bound, BT, ST, GT, Error3),
		(
		    Error3 =:= 1 ->
		    statistics(runtime, [_, T2]),
		    simple_message(
			"~w bounds size analysis performed in ~w msec.",
			[Approx, T2]),
		    !,
		    output_term_size_analysis(Size_Analysis, Approx,
			Resources, ST),
		    statistics(runtime, [_, T3]),
		    simple_message(
			"~w bounds size information stored in ~w msec.",
			[Approx, T3])
		;
		    error_message(nosize, _, _)
		)
	    )
	;
	    error_message(nosize, _, _)
	).

tsize_analysis([],          _,      _,     _,  _,  _,  1).
tsize_analysis([Comp|SCCG], Approx, Bound, BT, ST, GT, Error1) :-
	dependency_analysis(Comp, BT, ST, Adg, _Ldg, Gvars, Error),
	(
	    Error =:= 1 ->
	    size_analysis(Comp, Approx, Bound, BT, ST, Comp, Adg, Gvars,
		_Size),
	    tsize_analysis(SCCG, Approx, Bound, BT, ST, GT, Error1)
	;
	    Error1 = 0
	).

output_term_size_analysis(_Size_Analysis, _Approx, _Resources, ST) :-
	var(ST),
	!.
output_term_size_analysis(Size_Analysis, Approx, Resources, ST) :-
	nonvar(ST),
	ST = [Entry|Rest],
	Entry = st(Pred, _ClauseList, Mode, Measure, Mutex, Solution_Det,
	    Size, Relation, Time, Domain),
	output_term_size_trust_info(Size_Analysis, Approx, Resources, Pred,
	    Mode, Measure, Mutex, Solution_Det, Size, Relation, Time, Domain),
	output_term_size_analysis(Size_Analysis, Approx, Resources, Rest).

output_term_size_trust_info(Size_Analysis, Approx, Resources, Pred, Mode,
	    Measure, Mutex, Solution_Det, Size, Relation, ResourceList,
	    Domain) :-
	Pred = F/A,
	functor(Goal, F, A),
	make_atom([F, A], Key),
	safe_get_vartype(Key, Goal, Call_VarType, Succ_VarType),
	current_symbol_resources(Resources, Cost, ResourceList),
	(
	    Approx == o ->
	    map(Size, order_of_function, Out_Size)
	;
	    Size = Out_Size
	),
	asserta_fact(inferred(Size_Analysis, Goal, complexity(Call_VarType,
		    Succ_VarType, Mode, Measure, Mutex, Solution_Det, Out_Size,
		    Relation, Approx, Resources, Cost, Domain))).


:- data time/2.

record_java_time(RAnal, start) :-
	java_res_analyze(RAnal), !,
	statistics(walltime, _).

record_java_time(RAnal, end) :-
	java_res_analyze(RAnal), !,
	statistics(walltime, [_, T]),
	( retract(time(RAnal, T0)) ->
	    NT is T0 + T
	;
	    NT = T
	),
	assert(time(RAnal, NT)).
record_java_time(RAnal, KeyWord) :-
	error_message("Recording stats for ~q option ~q", [RAnal, KeyWord]).

java_res_analyze(size).
java_res_analyze(resources).

output_java_statistics(Approx) :-
	retract(time(size,      STotal)),
	retract(time(resources, RTotal)),
	simple_message("~q JAVA size analysis performed in ~q ms",
	    [Approx, STotal]),
	simple_message("~q JAVA resource analysis performed in ~q ms",
	    [Approx, RTotal]).
output_java_statistics(_) :-
	error_message("Output of Java statistics").
