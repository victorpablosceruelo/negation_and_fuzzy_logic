:- module(infercost,
	[
	    analysis_steps/4,
	    % complexity_analysis/3,
	    complexity_analysis/4,
	    % complexity_to_approximation/2,
	    % For granularity control
	    cs_analysis/7
	],
	[assertions, infercost(infercost_decl)]).

?- error_in_lns(_, _, warning,

'Using the old computation steps cost analysis which has been 
deprecated.  The current recommended way to perform this analysis is 
to use the generic resource analysis and the predefined resource 
package "predefres(res_steps)", for example:

:- module(_, _, [predefres(res_steps)]).
:- entry append/3 : list(gnd) * list(gnd) * var.
append([],     Z, Z).
append([X|Xs], Z, [X|Ys]) :-
	append(Xs, Z, Ys).
').

% ciao library:
:- use_module(library(messages), [debug_message/2, note_message/2, 
	simple_message/2, error_message/2]).
:- use_module(library(prolog_sys), [statistics/2]).

% ciaopp library:
:- use_module(ciaopp(preprocess_flags), [set_pp_flag/2, current_pp_flag/2, 
	push_pp_flag/2, pop_pp_flag/1]).
:- use_module(infer(infer_db), [inferred/3, cleanup_infer_db/1]).
:- use_module(infer(vartypes), [gather_vartypes/2]).
:- use_module(infer(gather_modes), [gather_modes/4]).
% 
:- use_module(infer(vartypes), [get_vartype/4]).
:- use_module(spec(s_simpspec), [make_atom/2]).
:- use_module(typeslib(type_support), [closed_var_list/2]).
:- use_module(typeslib(regtype_basic_lattice), [set_top_type/1]).
% own library:
:- use_module(infercost(init(dec)), [analysis_check/3]).
:- use_module(infercost(init(gran_init)), [cls_init_gran_system/10]).
:- use_module(infercost(dependency(dependency_)), [dependency_analysis/7]).
:- use_module(infercost(determinacy(determinacy_)), [determinacy_analysis/4]).
:- use_module(infercost(size(size_)), [size_analysis/7]).
:- use_module(infercost(gran(size_rel)), [input_arg_size_relation/6]).
:- use_module(infercost(solution(relation)), [relation_analysis/9]).
:- use_module(infercost(solution(solution_)), [solution_analysis/9]).
:- use_module(infercost(time), [time_analysis/9]).
:- use_module(infercost(algebraic(compare)), [order_of_function/2]).
:- use_module(infercost(top(error)), [error_message/3]).
:- use_module(infercost(infercost_register)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
%% Coment this when upload to SVN
%%issue_debug_messages(time).
%%issue_debug_messages(size).
%%issue_debug_messages(infercost).
%%issue_debug_messages(init).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%--------------------------------------------------------------------------
:- doc(author,"Pedro L@'{o}pez"||
                  " (based on Caslog, by Nai-Wei Lin)").  

:- doc(bug,"1. Generalize builtins and meta-calls.").
:- doc(bug,"2. Add exact cost analyses.").
:- doc(bug,"3. Use trusts.").


:- doc(module,"This component implements the cost analysis.").

:- doc(appendix,"The (basic) organization of the 
code is as follows (the picture does not show all modules for simplicity):

 @image{infercost}

Module @lib{infercost} is in charge of interfacing with the rest of CiaoPP.
Modules @lib{dependency}, @lib{determinacy}, @lib{size_rel}, @lib{size},
@lib{solution}, and @lib{time} implement different analyses which are 
instrumental in inferring cost information. The analysis uses mode and measure
information collected previously. It also uses non-failure information
from @lib{infer}. Cost information inferred is stored in @lib{infer_db}.
").

%--------------------------------------------------------------------------

approximation(X):- current_pp_flag(cost_approximation,X).

%--------------------------------------------------------------------------

complexity_to_approximation(steps_ub,    upper).
complexity_to_approximation(steps_o,     upper).
complexity_to_approximation(steps_lb,    lower).
complexity_to_approximation(size_ub,     upper).
complexity_to_approximation(size_o,      upper).
complexity_to_approximation(size_lb,     lower).

 %% approximation_to_size(upper, size_ub).
 %% approximation_to_size(lower, size_lb).
 %% 
 %% approximation_to_steps(upper, steps_ub).
 %% approximation_to_steps(lower, steps_lb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hook to lazy load of analysis:
:- multifile loaded_analysis/1.
loaded_analysis(Analysis) :- provided_analysis(Analysis).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile analysis/4.

analysis(Analysis, Cls, Ds, Info) :-
	is_complexity_analysis(Analysis),
	!,
	analysis_steps(Analysis, Cls, Ds, Info).

analysis_steps(Analysis, Cls, Ds, _Info) :-
	note_message("Using non-parametric cost analysis", []),
	analysis_complexity_init(Analysis, Cls, Ds, NewCls, NewDs),
	push_pp_flag(ana_cost,Analysis),
	analyze_complexity(Analysis,NewCls,NewDs),
	pop_pp_flag(ana_cost).

analysis_complexity_init(Analysis, Cls, Ds, NewCls, NewDs) :-
	cleanup_infer_db(Analysis),
	cleanup_infer_db(modes),
	cleanup_infer_db(vartypes),
	gather_vartypes(Cls,_Trusts),
	gather_modes(Cls,Ds,NewCls,NewDs).

% steps_lb, steps_ub,size_lb,size_ub
analyze_complexity_approximation(Analysis,AppFlag,Cls,Ds) :-
	set_pp_flag(cost_approximation,AppFlag),
	complexity_analysis(Analysis,Cls,Ds),
	!.

analyze_simple_complexity(Analysis,Cls,Ds):-
	complexity_to_approximation(Analysis, AppFlag),
	!,
	analyze_complexity_approximation(Analysis,AppFlag,Cls,Ds).

analyze_complexity(Analysis,Cls,Ds) :-
	analyze_simple_complexity(Analysis,Cls,Ds),
	!.
analyze_complexity(Analysis,Cls,Ds) :- % resource_ualb, size_ualb
	ualb_complexity_to_analysis(Analysis, _) ->
	(
	    ualb_complexity_to_analysis(Analysis, Analysis0),
	    (
		analyze_simple_complexity(Analysis0, Cls, Ds) ->
		true
	    ;
		error_message("Unable to perform analysis ~w", [Analysis0])
	    ),
	    fail
	;
	    true
	)
    ;
	fail.

%--------------------------------------------------------------------------

:- push_prolog_flag(multi_arity_warnings,off).

complexity_analysis(Analysis, Clauses, Dicts):-
	complexity_analysis(Analysis, none, Clauses, Dicts).

complexity_analysis(Analysis, Gr, Clauses, Dicts):-
	is_time_analysis(Analysis), 
	!,
	time_complexity_analysis(Analysis, Gr, Clauses, Dicts).
complexity_analysis(Analysis, _Gr, Clauses, Dicts):-
	is_size_analysis(Analysis),
	term_size_analysis(Analysis, Clauses, Dicts).

:- pop_prolog_flag(multi_arity_warnings).

%--------------------------------------------------------------------------

safe_get_vartype(Key, Goal, Call_VarType, Succ_VarType):-
	get_vartype(Key, Goal, Call_VarType, Succ_VarType),
	!.
safe_get_vartype(_Key, Goal, Types, Types):-
	create_worst_case_types(Goal, Types).

create_worst_case_types(Goal, Types):-
	closed_var_list(Goal, Varlist),
	assign_term_type_to_variables(Varlist, Types).

assign_term_type_to_variables([Var|R], [Prop|RT]):-
	set_top_type(F),
	Prop =.. [F, Var],
	assign_term_type_to_variables(R, RT).
assign_term_type_to_variables([], []).

%--------------------------------------------------------------------------

:- doc(time_complexity_analysis/2,"Performs the cost analysis of
	the program given as a list, in the fashion determined by the
	@tt{cost_approximation} flag. The clause list is in standard
	format but should include @tt{mode} and @tt{measure}
	directives.").
time_complexity_analysis(Analysis, Gr, Clauses, Dicts):-
	statistics(runtime, _),
	% ciao:inverse_rewrite_source_program0(Clauses, NClauses),
	cls_init_gran_system(Clauses, Dicts, BT, ST, SCCG, GT, GCG, Directs,
	    DirDicts, Error),
	analysis_check(SCCG, ST, _),
%  	Error is Error1*Error2,
 	(
	    Error =:= 1 ->
	    (
		statistics(runtime, [_, T1]),
		approximation(Approx),
		simple_message(
		    "preprocessed for ~w bounds cost analysis in ~w msec.",
		    [Approx, T1]),
		cs_analysis(SCCG, Approx, Gr, BT, ST, GT, Error3),
		(
		    Error3 =:= 1 ->
		    statistics(runtime, [_, T2]),
		    simple_message(
			"~w bounds cost analysis performed in ~w msec.",
			[Approx, T2]),
		    !,
		    asserta_fact(inferred(Analysis, '$current_punit',
		        comp_info(ST, SCCG, GT, GCG, Directs, DirDicts))),
		    output_complexity_analysis(Analysis, ST),
		    statistics(runtime, [_, T3]),
		    simple_message(
			"~w bounds cost information stored in ~w msec.",
			[Approx, T3])
		;
		    error_message(nocost, _, _)
               )
	    )
	;
	    error_message(nocost, _, _)
	).

:- check pred cs_analysis/7 + not_fails.

cs_analysis([],         _Approx, _Gr, _BT, _ST, _GT, 1).
cs_analysis([Comp|SCCG], Approx,  Gr,  BT,  ST,  GT, Error1) :-
	dependency_analysis(Comp, BT, ST, Adg, Ldg, Gvars, Error),
	(
	    Error =:= 1 ->
	    (
		size_analysis(Comp, BT, ST, Comp, Adg, Gvars, Size),
                % Size relations for granularity control transformation.
                (
		    Gr == gr -> 
		    input_arg_size_relation(Comp, BT, ST, Adg, Gvars, GT)
		;
		    true
		),
		relation_analysis(Comp, BT, ST, Comp, Size, Adg, Gvars, Ldg,
		    _),
		determinacy_analysis(Comp, BT, ST, Adg),
		solution_analysis(Comp, BT, ST, Comp, Size, Adg, Gvars, Ldg,
		    _),
		time_analysis(Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, _),
		cs_analysis(SCCG, Approx, Gr, BT, ST, GT, Error1)
	    )
	;
	    Error1 = 0
	).

output_complexity_analysis(_Analysis, ST):- var(ST), !.
output_complexity_analysis( Analysis, ST):-
	nonvar(ST),
	ST = [Entry|Rest],
	Entry = st(Pred, _ClauseList, Mode, Measure, Mutex, Solution_Det,
	    Size, Relation, Time, Domain),
	debug_message("CALL: ~q", 
	    [output_complexity_trust_info(Pred, Mode, Measure, Mutex,
	     Solution_Det, Size, Relation, Time, Domain)]),
	output_complexity_trust_info(Analysis, Pred, Mode, Measure,
	     Mutex, Solution_Det, Size, Relation, Time, Domain),
	debug_message("EXIT: ~q", 
	    [output_complexity_trust_info(Pred, Mode, Measure, Mutex,
	     Solution_Det, Size, Relation, Time, Domain)]),
	output_complexity_analysis(Analysis, Rest).

output_complexity_trust_info(Analysis, Pred, Mode, Measure, Mutex,
	    Solution_Det, Size, Relation, Time, Domain):-
	Pred = F/A,
	functor(Goal, F, A),
	make_atom([F,A],Key),
	safe_get_vartype(Key,Goal,Call_VarType,Succ_VarType),
	(
	    Analysis = steps_o ->
	    order_of_function_list(Time, Out_Time),
	    order_of_function_list(Size, Out_Size)
	;
	    Time = Out_Time,
	    Size = Out_Size
	),
	asserta_fact(inferred(Analysis, Goal,
	    complexity(Call_VarType, Succ_VarType, Mode, Measure, Mutex,
	    Solution_Det, Out_Size, Relation, Out_Time, Domain))).


order_of_function_list([F|R], [OF|OR]):-
	order_of_function(F, OF),
	order_of_function_list(R, OR).
order_of_function_list([], []).

% 
%-----------------------------------------------------------------------------

term_size_analysis(Size_Analysis, Clauses, Dicts):-
	statistics(runtime, _),
        cls_init_gran_system(Clauses, Dicts, BT, ST, SCCG, GT, _GCG,
	    _Directs, _DirDicts, Error),
	analysis_check(SCCG, ST, _),
% 	Error is Error1 * Error2,
 	(
	    Error =:= 1 ->
	    (
		statistics(runtime,[_,T1]),
		approximation(Approx),
		simple_message(
		    "preprocessed for ~w bounds size analysis in ~w msec.",
		    [Approx, T1]),
		tsize_analysis(SCCG, BT, ST, GT, Error3),
		(
		    Error3 =:= 1 ->
         	    statistics(runtime, [_, T2]),
	            simple_message(
			"~w bounds size analysis performed in ~w msec.",
			[Approx, T2]),
                    !,
                    output_term_size_analysis(Size_Analysis, ST),
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

tsize_analysis([],         _BT, _ST, _GT, 1).
tsize_analysis([Comp|SCCG], BT,  ST,  GT, Error1) :-
	dependency_analysis(Comp, BT, ST, Adg, _Ldg, Gvars, Error),
	(
	    Error =:= 1 ->
	    size_analysis(Comp, BT, ST, Comp, Adg, Gvars, _Size),
	    tsize_analysis(SCCG, BT, ST, GT, Error1)
	;
	    Error1 = 0
	).

output_term_size_analysis(_Size_Analysis, ST):-
	var(ST),
	!.
output_term_size_analysis( Size_Analysis, ST):-
	nonvar(ST),
	ST = [Entry|Rest],
	Entry = st(Pred, _ClauseList, Mode, Measure, Mutex, Solution_Det,
	    Size, Relation, Time, Domain),
	output_term_size_trust_info(Size_Analysis, Pred, Mode, Measure, Mutex,
	    Solution_Det, Size, Relation, Time, Domain),
	output_term_size_analysis(Size_Analysis, Rest).

output_term_size_trust_info(Size_Analysis, Pred, Mode, Measure, Mutex,
	    Solution_Det, Size, Relation, Time, Domain):-
	Pred = F/A,
	functor(Goal, F, A),
	make_atom([F, A], Key),
	safe_get_vartype(Key, Goal, Call_VarType, Succ_VarType),
	(
	    Size_Analysis = size_o ->
	    order_of_function_list(Size, Out_Size)
	;
	    Size = Out_Size
	),
	asserta_fact(inferred(Size_Analysis, Goal, complexity(Call_VarType,
	    Succ_VarType, Mode, Measure, Mutex, Solution_Det, Out_Size,
	    Relation, Time, Domain))).
