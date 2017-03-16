:- module(nonfail,
	    [actualize_flags_for_SCC_literal/7,
		actualize_flags_for_literal_before_fixpoint/4,
		compute_predicate_flags/5,
		cover_test_for_non_failing_clauses/5,
		unfold_defined_tests/3,
		not_defined_message/1,
		write_non_fail_info/4
	    ],
	    [assertions, isomodes]).

:- use_module(infernf(nfgraph),
	    [check_if_is_a_defined_test/2,
		literals_meet_selection_condition/1
	    ]).
:- use_module(infernf(nftable),
	    [flag_is_false/1,
		flag_is_unbound/1,
		get_nfentry_info/7,
		set_coverflag_value/2,
		set_flag_value_false/1,
		set_nfailflag_value/2,
		get_lit_pred_name_arity/2,
		get_literal_key/2
	    ]).
:- use_module(infernf(in_out),
	    [get_body_of_clause/2,
		nf_get_trust_dec/4,
		there_are_no_more_clauses/1,
		get_first_clause_id/2
	    ]).
:- use_module(infernf(nftests),
	    [
		compute_test_for_all_clauses/3,
		compute_test_for_selected_clauses/4
	    ]).
:- use_module(infernf(cover),         [covers_check/5]).
:- use_module(infer(infer_db),        [inferred/3]).
:- use_module(infer(infer_dom),       [flag_set/3]).

:- use_module(program(clause_db), [clause_locator/2, literal_locator/2]).

:- use_module(library(messages), [warning_message/3, debug_message/2]).

%-----------------------------------------------------------------------

select_clauses_for_covering(Clauses, _Count, [], _SelectedClauses) :-
	there_are_no_more_clauses(Clauses), !.
select_clauses_for_covering([Clause|CList], Count, SelectedClauseNumbers,
	    SelectedClauses) :-
	get_body_of_clause(Clause, Body),
	( is_body_selected_for_covering(Body)
	-> SelectedClauseNumbers = [Count|ClauseNumbers],
	    SelectedClauses = [Clause|SelClist]
	; SelectedClauseNumbers = ClauseNumbers,
	    SelectedClauses = SelClist ),
	NewCount is Count + 1,
	select_clauses_for_covering(CList, NewCount, ClauseNumbers, SelClist).

cover_test_for_non_failing_clauses(Clauses, ModeType, UseMasc, Masc,
	    Cover_flag) :-
	select_clauses_for_covering(Clauses, 1, SelectedClauseNumbers,
	    SelectedClauses),
	compute_test_for_selected_clauses(SelectedClauseNumbers,
	    SelectedClauses, ModeType, Test),
	debug_message("CALL: ~q", [covers_check(ModeType, UseMasc, Masc, Test,
		    Cover_flag)]),
	covers_check(ModeType, UseMasc, Masc, Test, Cover_flag),
	debug_message("EXIT: ~q", [covers_check(ModeType, UseMasc, Masc, Test,
		    Cover_flag)]).


actualize_flags_for_literal_before_fixpoint(_Clauses, Litinfo, Nfail_flag,
	    Cover_flag) :-
	Nfail_flag = Cover_flag,
	set_nfailflag_value(Litinfo, Nfail_flag),
	set_coverflag_value(Litinfo, Cover_flag).

cover_test_for_all_clauses(Clauses, ModeType, UseMasc, Masc, Cover_flag) :-
	debug_message("~q", [compute_test_for_all_clauses(Clauses, ModeType,
		    Test)]),
	compute_test_for_all_clauses(Clauses, ModeType, Test),
	debug_message("~q", [compute_test_for_all_clauses(Clauses, ModeType,
		    Test)]),
	covers_check(ModeType, UseMasc, Masc, Test, Cover_flag).

is_body_selected_for_covering(Body) :-
	literals_meet_selection_condition(Body).

unfold_defined_tests(Litinfo, Clauses, Nfail_flag) :-
	( flag_is_false(Nfail_flag) ->
	    debug_message("CALL: check_if_is_a_defined_test(~q, ~n ~q)", [
		    Litinfo, Clauses]),
	    check_if_is_a_defined_test(Litinfo, Clauses),
	    debug_message("EXIT: check_if_is_a_defined_test(~q, ~n ~q)", [
		    Litinfo, Clauses])
	;
	    true ).

actualize_flags_for_SCC_literal(Clauses, ModeType, UseMasc, Masc, Cover_flag,
	    Nfail_flag, Change) :-
	debug_message("Mode-Type = ~q Cover Flag = ~q ...", [ModeType,
		Cover_flag]),
	cover_test_for_non_failing_clauses(Clauses, ModeType, UseMasc, Masc,
	    Covflag),
	debug_message(
"done cover_test_for_non_failing_clauses (fixpoint), Cover Flag = ~q", [Covflag]
	),
	( flag_is_false(Covflag)
	-> set_flag_value_false(Cover_flag),
	    set_flag_value_false(Nfail_flag),
	    Change = true
	; true ).


compute_predicate_flags(SccPred, Clauses, ModeType, Cover_flag, Nfail_flag) :-
	debug_message(
"Performing (after fixpoint) cover-check for all clauses of predicate: ~q.", [
		SccPred]),
	debug_message(
	    "Mode-Type = ~q Cover Flag = ~q ...",
	    [ModeType, Cover_flag]),
	cover_test_for_all_clauses(Clauses, ModeType, false, _Masc, Cover_flag
	),
	debug_message(
"done (after fixpoint) cover-check for all clauses of predicate ~q, Cover Flag = ~q", [
		SccPred, Cover_flag]),
	( flag_is_false(Cover_flag)
	-> debug_message("set_flag_value_false(~q)", [Nfail_flag]),
	    set_flag_value_false(Nfail_flag),
	    debug_message("set_flag_value_false(~q)", [Nfail_flag])
	; debug_message("CALL: (after fixpoint) ~q",
		[cover_test_for_non_failing_clauses(Clauses, ModeType, false,
			_Masc, Nfail_flag)]),
	    cover_test_for_non_failing_clauses(Clauses, ModeType, false, _Masc,
		Nfail_flag),
	    debug_message("EXIT: (after fixpoint) ~q",
		[cover_test_for_non_failing_clauses(Clauses, ModeType, false,
			_Masc, Nfail_flag)]) ).

not_defined_message(Litinfo) :-
	get_literal_key(Litinfo, Key),
	literal_locator(Key, Lit_Locator),
	get_lit_pred_name_arity(Litinfo, LitPred),
	warning_message(Lit_Locator, "No trust assertion or definition " ||
	    "available for predicate ~q. Assumed that any call to it fails.",
	    [LitPred]).

% MOVED FROM in_out.pl 

%%                 Annotation of the program.

:- pred write_non_fail_info(+TAB, -Num_Pred, -Num_NF_Pred, -NCov)

# "Traverses the annotated program in @var{TAB} and asserts in the
   data base (module database, see file infernf.pl) the non-failure
   info for each predicate.  This info is only at predicate level. The
   info is asserted as facts of the form trust_nonfail(Head, InTypes,
   OuTypes, FailInfo, CoverInfo), where FailInfo can be either
   not_fail or possible_fail, and CoverInfo can be either covered or
   not_covered.  @var{Num_Pred} is the total number of predicates
   analyzed, @var{Num_NF_Pred} the number of non-failing predicates
   detected, and @var{NCov} the number of predicates that cover their
   respective types.".

write_non_fail_info(TAB, Num_Pred, Num_NF_Pred, NCov) :-
	write_non_fail_info_1(TAB, 0, 0, 0, Num_Pred, Num_NF_Pred, NCov).


write_non_fail_info_1(TAB, Num_Pred, Num_NF_Pred, NCov,
	    Num_Pred, Num_NF_Pred, NCov) :-
	var(TAB),
	!.
write_non_fail_info_1([Entry|TAB], I_Num_Pred, I_Num_NF_Pred, I_NCov,
	    Num_Pred, Num_NF_Pred, NCov) :-
	get_nfentry_info(Entry, Pred, Clauses, _Mode_type, _Test, Nf_flag,
	    Covered_Flag),
%% Assume that there is at least one clause in Clauses.
	T_Num_Pred is I_Num_Pred + 1,
	( flag_is_unbound(Nf_flag) ->
	    get_first_clause_locator(Clauses, Locator1),
	    warning_message(Locator1,
		"Non-failure flag of predicate ~q is unbound.", [Pred])
	;
	    true ),
	( flag_is_false(Nf_flag) ->
	    flag_set(nf, possibly_fails, FailInfo),
	    T_Num_NF_Pred is I_Num_NF_Pred
	;
	    flag_set(nf, not_fails, FailInfo),
	    T_Num_NF_Pred is I_Num_NF_Pred + 1
	),
	( flag_is_unbound(Covered_Flag) ->
	    get_first_clause_locator(Clauses, Locator2),
	    warning_message(Locator2, "Cover flag of predicate ~q is unbound.",
		[Pred])
	;
	    true ),
	( flag_is_false(Covered_Flag) ->
	    flag_set(nf, not_covered, CoverInfo),
	    T_NCov is I_NCov;
	    flag_set(nf, covered, CoverInfo),
	    T_NCov is I_NCov + 1
	),
	nf_get_trust_dec(Pred, Head, InTypes, OuTypes),
% numbervars(trust(Head, InTypes, OuTypes),0,_),
	output_nonfailure_trust_info(Head, InTypes, OuTypes,
	    FailInfo, CoverInfo),
	write_non_fail_info_1(TAB, T_Num_Pred, T_Num_NF_Pred, T_NCov,
	    Num_Pred, Num_NF_Pred, NCov).

output_nonfailure_trust_info(Head, InTypes, OuTypes, FailInfo, CoverInfo) :-
	asserta_fact(inferred(nfg, Head,
		nf(InTypes, OuTypes, FailInfo, CoverInfo))).

get_first_clause_locator(Clauses, Locator) :-
	get_first_clause_id(Clauses, Id),
	clause_locator(Id, Locator).


% END OF MOVED FROM in_out.pl 
