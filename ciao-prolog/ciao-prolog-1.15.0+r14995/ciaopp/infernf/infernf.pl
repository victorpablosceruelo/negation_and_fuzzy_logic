:- module(infernf, [non_failure_analysis/2, non_failure_analysis/6],
	    [assertions, basicmodes]).

% own library:
:- use_module(infernf(in_out),
	    [init_in_out/0,
		input_nf_analysis_info/3,
% translate_rule_list_for_nonfail/2, %% Commented out May-18-2003 
		validate_data/4,
		remove_preds_from_scc_call_graph/3
	    ]).
:- use_module(infernf(nfgraph), [not_fail_check/2]).
% :- use_module(infernf(nftypes),[init_nftypes/0,type_rule_simplify/0]).
:- use_module(infernf(nonfail), [write_non_fail_info/4]).

% ciaopp library:
:- use_module(plai(tarjan), [tarjan/2]).

% ciao library:
:- use_module(library(messages),   [debug_message/1, simple_message/2]).
:- use_module(library(prolog_sys), [statistics/2]).

%-----------------------------------------------------------------------

:- doc(author, "Pedro L@'{o}pez").

:- doc(bug1, "Generalize builtins and meta-calls.").
:- doc(bug2, "Use trusts.").
:- doc(bug3, "Make it more flexible wrt the absence of required info.").

:- doc(module, "This component implements the non-failure analysis.").

:- doc(appendix, "The (basic) organization of the code is as follows: 

 @image{infernf}

Module @lib{infernf} is in charge of interfacing with the rest of CiaoPP.
The core of the analysis is in @lib{nfgraph}. It makes use of type and mode
information collected previously. It also uses strong connected 
components computed by @lib{tarjan} and the types in @lib{typeslib}.
Information inferred is stored in @lib{infer_db}.
").

%-----------------------------------------------------------------------

:- pred non_failure_analysis(+ClauseList, +ModeTypes)
# "Performs a non-failure analysis of the predicates given in
   @var{ClauseList}.  @var{ModeTypes} is the moded type
   information for these predicates. Once the analysis is done, 
   the non-failure info for each predicate (only at predicate level)
   is asserted as facts of @tt{infer(infer_db):inferred/3}.".

non_failure_analysis(ClauseList, ModeTypes) :-
	non_failure_analysis(ClauseList, ModeTypes, _, _, _, _).

:- pred non_failure_analysis(+ClauseList, +ModeTypes,
	    -TimeNf, -Num_Pred, -Num_NF_Pred, -NCov)
# "Similar to @tt{non_failure_analysis(ClauseList, ModeTypes, Ti, To)}.
   @var{Num_Pred} is the total
   number of predicates analyzed, @var{Num_NF_Pred} the number of
   non-failing predicates detected, and @var{NCov} the number of
   predicates that cover their respective types. @var{TimeNf} is the
   time used by the core non-failure
   analysis exclusively (i.e. excluding instrumental computations).".

non_failure_analysis(ClauseList, ModeTypes, TimeNf, Num_Pred, Num_NF_Pred,
	    NCov) :-
	statistics(runtime, _),
	tarjan(ClauseList, (_, SCCCallGraph)),
%% Commented out May-18-2003 Now uses typeslib database directly.
% get_type_defs(CiaoTypeRules),
% translate_rule_list_for_nonfail(NofailTypeRules, CiaoTypeRules),
%% End Commented out May-18-2003
%% get_type_defs(NofailTypeRules), %% Added May-18-2003 %% Commented 7 jun 2003 
	init_internal_nf_state,
	!,
	debug_message("Reading program ..."),
% input_nf_analysis_info(ClauseList, ModeTypes, NofailTypeRules, TAB), 
% PLG Commented 7 jun 2003 
	input_nf_analysis_info(ClauseList, ModeTypes, TAB),
	!,
	debug_message("done"),
	debug_message("Validating data ..."),
	validate_data(TAB, TAB, ValTAB, No_Analyzable_Preds),
	debug_message("done"),
	!,
	debug_message("Simplifying types rules..."),
% type_rule_simplify, %% Commented out May-18-2003  
	!, % -PL warning !
	debug_message("done"),
	remove_preds_from_scc_call_graph(SCCCallGraph, No_Analyzable_Preds,
	    SCC_CG),
	statistics(runtime, [_, T1]),
	simple_message("preprocessed for nfg in ~w msec.", T1),
	not_fail_check(SCC_CG, ValTAB),
	!,
	statistics(runtime, [_, TimeNf]),
	simple_message("analyzed by nfg in ~w msec.", TimeNf),
% nf_annotate(ValTAB), % I think it is not neccessary. -PLG
	!,
	write_non_fail_info(ValTAB, Num_Pred, Num_NF_Pred, NCov),
	statistics(runtime, [_, T2]),
	simple_message("analysis information stored in ~w msec.", T2).

%-----------------------------------------------------------------------
% Initialize internal data.

init_internal_nf_state:-
	init_in_out.
% init_nftypes. % Commented out May-18-2003
