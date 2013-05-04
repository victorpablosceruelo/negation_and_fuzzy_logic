:- module(nfgraph,
	    [check_if_is_a_defined_test/2,
		literals_meet_selection_condition/1,
		not_fail_check/2
	    ],
	    [assertions, basicmodes]).

:- use_module(infernf(nonfail),
	    [actualize_flags_for_SCC_literal/7,
		actualize_flags_for_literal_before_fixpoint/4,
		compute_predicate_flags/5,
		cover_test_for_non_failing_clauses/5,
		unfold_defined_tests/3,
		not_defined_message/1
	    ]).
:- use_module(infernf(nftable)).
:- use_module(infernf(in_out),
	    [get_body_and_vars_of_clause/3,
		get_body_of_clause/2,
		get_head_of_clause/2,
		there_are_no_more_clauses/1
	    ]).
% :- use_module(infernf(nftests),[compute_pred_test/3]). % PLG May-18-2003
:- use_module(infernf(nfbool),  [put_negation/3]).
:- use_module(library(idlists), [member_0/2]).
:- use_module(library(messages)).

%-----------------------------------------------------------------------

:- pred not_fail_check(SCCCallGraph, TAB)

# "Performs non-failure and covering analysis for all the predicates
in the program. @var{SCCCallGraph} is a list with the strongly
connected components of the call graph. @var{TAB} is a data structure
containing a special representation of the program (including
information useful, or instrumental to the analysis).  Once these
analysis are performed, each literal which is not considered to be a
test has its cover_flag with either the value (true or fail) and the
non_fail_flag either with the value fail (non-failure is not ensured)
or as a free variable (the literal will not fail).".

:- doc(module, "Checks if there is a clause with its test
equivalent to true such that all of their literals are non-failing.
Question: Is it useful to store a flag indicating whether a clause has
all of its literals (or those after the first/last cut)
non-failing?").


%% For each clauses we keep a flag.
%% 
%% Non-failure analysis:
%% 
%% All_literals_are_non_failing,
%% All_literals_after_the_first_cut_are_non_failing
%% 
%% 
%% Determinism analysis:
%% 
%% All_literals_are_deterministic,
%% All_literals_after_the_last_cut_are_deterministic

%% Non-failure analysis: we keep an open list of lists. Each list is
%%     the list of clauses (number) such that all of its literals are
%%     non-failing. If for some of such lists it holds that their tests
%%     cover the input type then the predicate is non-failing.

not_fail_check(SCCCallGraph, TAB) :-
	traverse_all_predicate_literals(SCCCallGraph, TAB).

traverse_all_predicate_literals([],                 _TAB) :- !.
traverse_all_predicate_literals([SCC|SCCCallGraph], TAB) :-
	traverse_one_SCC(SCC, TAB),
	traverse_all_predicate_literals(SCCCallGraph, TAB).

traverse_one_SCC(SCC, TAB) :-
	process_SCC_before_fixpoint(SCC, SCC, TAB),
	fixpoint_of_one_SCC(SCC, TAB),
	process_SCC_after_fixpoint(SCC, TAB).

%% Begin of process before fixpoint.

:- pred process_SCC_before_fixpoint(SCC1, SCC2, TAB)

# "Process an strong connected component before computing the fixpoint
   for it.  @var{SCC1} and @var{SCC2} has to be the same at the
   call. We traverse @var{SCC1} to process each predicate in it,
   whereas @var{SCC2} is used to check whether a literal predicate
   belongs to the SCC.  Set the non-fail and cover flags to true for
   literals that are tests. Compute the non-fail and cover flags for
   the predicates that are not in @var{SCC2}, since these
   predicates have been processed before and thus, we know all of their
   non-failing clauses.".

process_SCC_before_fixpoint([],                _SCC, _TAB) :- !.
process_SCC_before_fixpoint([SccPred|RestScc], SCC,  TAB) :-
% find the information corresponding to the predicate in the SCC (i.e. for SccPred).   
	find_entry(TAB, SccPred, PredEntry),
% Get te clauses of predicate SccPred.
	get_nf_info(PredEntry, Clauses, _Mode_type, _Test, _Nfail_flag,
	    _Cover_flag),
	process_all_clauses_before_fixpoint(Clauses, SCC, PredEntry, TAB),
	process_SCC_before_fixpoint(RestScc, SCC, TAB).

process_all_clauses_before_fixpoint(Clauses, _SCC, _PEntry, _TAB) :-
	there_are_no_more_clauses(Clauses),
	!.
process_all_clauses_before_fixpoint([Clause|CList], SCC, PEntry, TAB) :-
	get_body_and_vars_of_clause(Clause, Body, ClVars),
	process_one_body_before_fixpoint(Body, ClVars, SCC, PEntry, TAB),
	process_all_clauses_before_fixpoint(CList, SCC, PEntry, TAB).

process_one_body_before_fixpoint([], _, _, _, _) :- !.
process_one_body_before_fixpoint([Litinfo|LList], ClVars, SCC, PredEntry,
	    TAB) :-
	get_litpred_and_literal(Litinfo, LitPred, Literal),
% If the predicate of the literal Literal is in the SCC then nothing is done,
% otherwise 
	( belongs_to_SCC(LitPred, SCC)
	-> true
	; process_one_literal_before_fixpoint(Litinfo, LitPred, Literal,
		ClVars, PredEntry, TAB) ),
	process_one_body_before_fixpoint(LList, ClVars, SCC, PredEntry, TAB).

belongs_to_SCC(LitPred, SCC) :- member_0(LitPred, SCC).

process_one_literal_before_fixpoint(Litinfo, LitPred, Literal, ClVars,
	    _PredEntry, _TAB) :-
	Litinfo = litinfo(Key_1, Lit_1, TestFlag_1, Test_1, _HeadPred_1,
	    _ModeType_1, Nfail_flag_1, Cover_flag_1),
	pred_assertion(LitPred, ClVars, Key_1, Literal, CallType, TestFlag,
	    Test, Nfail_flag, Cover_flag),
	holds_call_type(CallType, Litinfo, Literal, ClVars),
	!,
	put_negation(Lit_1, Test, Test_1),
	TestFlag_1 = TestFlag,
	Nfail_flag_1 = Nfail_flag,
	Cover_flag_1 = Cover_flag.
process_one_literal_before_fixpoint(Litinfo, LitPred, Literal, _ClVars,
	    PredEntry, TAB) :-
	get_literal_predicate_entry(TAB, LitPred, PredEntry, LitPredEntry),
	( var(LitPredEntry) ->
	    not_defined_message(Litinfo),
	    set_nfailflag_false(Litinfo),
	    set_coverflag_false(Litinfo)
	;
	    compute_cov_and_nf_flags_before_fixpoint(Litinfo, Literal,
		LitPredEntry, true, Literal) ).

holds_call_type(_CallType, _Litinfo, _Literal, _ClVars).

% holds_call_type(CallType, _Litinfo, _Literal, _ClVars) :-
% 	var(CallType),
% 	!.
% holds_call_type(CallType, Litinfo, Literal, ClVars) :-
% % Warning: get_shfr_prog_point_info gets the literal from Litinfo.
% % and if the literal is negated then it is different from Literal.
% 	get_shfr_prog_point_info(Litinfo, ClVars, PPinfo),
% 	construct_call_type(Literal, PPinfo, LitCallType),
% 	debug_message("Processing call to builtin: ~q.", [LitCallType]),
% 	LitCallType = CallType.

% get_shfr_prog_point_info(Litinfo, ClVars, OutputUser) :-
% 	get_literal_key(Litinfo, LiteralKey),
% 	get_literal(Litinfo, Literal),
% 	( (Literal = (!) ; LiteralKey == nokey)
% 	-> OutputUser = []
% 	;
% 	    get_info(vartypes, point, LiteralKey, Goal, (Call, _Succ)),
% 	    OutputUser = Call % need variable renaming.
% 	).

% % Old (commented out 7 jun 2003)
% get_shfr_prog_point_info(Litinfo, ClVars, OutputUser) :-
% 	get_literal_key(Litinfo, LiteralKey),
% 	get_literal(Litinfo, Literal),
% 	( (Literal = (!) ; LiteralKey == nokey)
% 	-> OutputUser = []
% 	;
% 	    debug_message(
% 		"Getting info from database for literal: ~q with key: ~q.",
% 		[Literal, LiteralKey]),
% % 	    recorded_internal(PPId, type_memo_table(Key, Vars, Type), _),
% % 	    debug_message("CALL: ~q", [recorded_internal(LiteralKey,
% % 			type_memo_table(PPKey, PPVars, PPType), _)]),
% % 	    database:recorded_internal(LiteralKey,
% % 		type_memo_table(PPKey, PPVars, PPType), _),
% % 	    debug_message("EXIT: ~q", [recorded_internal(LiteralKey,
% % 			type_memo_table(PPKey, PPVars, PPType), _)]),
% 	    debug_message("CALL: ~q", [recorded_internal(LiteralKey,
% 			memo_table(N, Id, SubVars, Subs), _)]),
% 	    database:recorded_internal(LiteralKey,
% 		memo_table(N, Id, SubVars, Subs), _),
% 	    debug_message("EXIT: ~q", [recorded_internal(LiteralKey,
% 			memo_table(N, Id, SubVars, Subs), _)]),
% 	    output_user_interface(shfr, Subs, SubVars, OutputUser),
% 	    debug_message("recorded_internal(~q, memo_table(~q, ~q, ~q, ~q)).",
% 		[LiteralKey, N, Id, SubVars, Subs]) ),
% 	SubVars = ClVars,
% 	debug_message("done: ~q", [OutputUser]).

compute_cov_and_nf_flags_before_fixpoint(Litinfo, Literal, LitPredEntry,
	    UseMasc, Masc) :-
	functor(Literal, F, A), % Only for debugging.
	debug_message("Performing cover-check for predicate NOT in SCC: ~q.",
	    [F/A]),
	get_info_for_literal_from_predicate(LitPredEntry, Clauses, ModeType,
	    _LitPredCovFlag, _LitPredNF_Flag),
	debug_message("Mode-Type = ~q Cover Flag = ~q ...", [ModeType,
		Cover_flag]),
	cover_test_for_non_failing_clauses(Clauses, ModeType, UseMasc, Masc,
	    Cover_flag),
	debug_message(
"done cover_test_for_non_failing_clauses (before fixpoint), Cover Flag = ~q",
	    [Cover_flag]),
	actualize_flags_for_literal_before_fixpoint(Clauses, Litinfo,
	    Nfail_flag, Cover_flag),
	unfold_defined_tests(Litinfo, Clauses, Nfail_flag).


:- pred check_if_is_a_defined_test(?Litinfo, +Clauses)

# "Succeeds if the clauses @var{Clauses} of the literal of
  @var{Litinfo} define a test. Currently we assume that these clauses
  define a test if there is only one clause and in the body of the
  clause there are only calls to builtin tests. In that case, also
  sets the value and flag for the test of @var{Litinfo}".


check_if_is_a_defined_test(Litinfo, Clauses) :-
	( is_a_defined_test(Litinfo, Clauses, OnlyTests)
	->
	    set_test_flag_value_defined(Litinfo),
	    set_test_value(Litinfo, OnlyTests)
	; true ).

is_a_defined_test(Litinfo, Clauses, OnlyTests) :-
	there_is_only_one_clause(Clauses, OnlyClause),
	there_are_only_tests(Litinfo, OnlyClause, OnlyTests).

there_is_only_one_clause(Clauses, OnlyClause) :-
	nonvar(Clauses),
	Clauses = [OnlyClause|Rest],
	there_are_no_more_clauses(Rest).

there_are_only_tests(Litinfo, InClause, Tests) :-
	copy_term(InClause, Clause),
	get_body_of_clause(Clause, Body),
	there_are_only_body_tests(Body, BodyTests),
	get_head_of_clause(Clause, Head),
	get_literal(Litinfo, Literal),
	functor(Head, _F, A),
	generate_head_unifs(1, A, Literal, Head, HeadUnifs),
	Tests = defined(HeadUnifs, BodyTests).
% append(HeadUnifs, BodyTests, Tests).

there_are_only_body_tests([],             []) :- !.
there_are_only_body_tests([Litinfo|Rest], Tests) :-
% litinfo_is_a_test(Litinfo),
	litinfo_is_a_builtin_test(Litinfo),
% get_literal(Litinfo, Literal),
% known_test(Literal),
	Tests = [Litinfo|ResTests],
	there_are_only_body_tests(Rest, ResTests).

generate_head_unifs(N, A, _Literal, _Head, []) :-
	N > A, !.
generate_head_unifs(N, A, Literal, Head, [Test|Unif_list]) :-
	arg(N, Literal, LitArg),
	arg(N, Head,    HeadArg),
	Test = (LitArg = HeadArg),
	N1 is N + 1,
	generate_head_unifs(N1, A, Literal, Head, Unif_list).

%% What are the nonfail and cover flag of a predicate?
%% 
%% a) Cover flag: 
%%     a1) The non-failing clauses cover the type of the predicate.  
%%     a2) All the clauses cover the type of the predicate.  
%% b) Non-fail flag:
%%     b1) The non-failing clauses DO NOT cover the type of the predicate.  
%%     b2) There is some literal (which is not a test) in the body for which 
%%         non failure is not ensured.
%% Depending of the meaning, we can perform some optimizations or other.

%% End of process before fixpoint.

%% Begin of fixpoint computation.

fixpoint_of_one_SCC(SCC, TAB) :-
	make_one_iteration_on_SCC(SCC, TAB, Change),
	( var(Change)
	-> true
	; fixpoint_of_one_SCC(SCC, TAB) ).

make_one_iteration_on_SCC([],            _TAB, _Change) :- !.
make_one_iteration_on_SCC([SccPred|SCC], TAB,  Change) :-
% find the information corresponding to the predicate in the SCC   
	find_entry(TAB, SccPred, PredEntry),
	get_nf_info(PredEntry, Clauses, _Mode_type, _Test, _Nfail_flag,
	    _Covered_Flag),
	one_iteration_on_SCC_clauses(Clauses, PredEntry, TAB, Change),
	make_one_iteration_on_SCC(SCC, TAB, Change).

one_iteration_on_SCC_clauses(Clauses, _PredEntry, _TAB, _Change) :-
	there_are_no_more_clauses(Clauses),
	!.
one_iteration_on_SCC_clauses([Clause|CList], PredEntry, TAB, Change) :-
	get_body_of_clause(Clause, Body),
	one_iteration_on_body(Body, PredEntry, TAB, Change),
	one_iteration_on_SCC_clauses(CList, PredEntry, TAB, Change).

one_iteration_on_body([],              _PredEntry, _TAB, _Change) :- !.
one_iteration_on_body([Litinfo|LList], PredEntry,  TAB,  Change) :-
	get_lit_info_for_nonfail(Litinfo, LitPred, Literal, Nfail_flag,
	    Cover_flag),
	( flag_is_unbound(Nfail_flag)
	-> one_iteration_on_SCC_literal(TAB, PredEntry, Litinfo, Literal,
		LitPred, Nfail_flag, Cover_flag, Change),
	    check_if_NF_flag_has_changed(Nfail_flag, Change)
	; true ),
	one_iteration_on_body(LList, PredEntry, TAB, Change).


check_if_NF_flag_has_changed(Nfail_flag, Change) :-
	flag_is_unbound(Nfail_flag)
    -> true
    ; Change = true.

one_iteration_on_SCC_literal(TAB, PredEntry, Litinfo, Literal, LitPred,
	    Nfail_flag, Cover_flag, Change) :-
	get_literal_predicate_entry(TAB, LitPred, PredEntry, LitPredEntry),
	( var(LitPredEntry) ->
	    not_defined_message(Litinfo),
	    set_flag_value_false(Nfail_flag),
	    set_flag_value_false(Cover_flag)
	;
	    cov_and_nf_flags_for_SCC_literal(Literal, LitPredEntry, true,
		Literal, Nfail_flag, Cover_flag, Change) ).

cov_and_nf_flags_for_SCC_literal(Literal, LitPredEntry, UseMasc, Masc,
	    Nfail_flag, Cover_flag, Change) :-
% Nfail_flag and Cover_flag are unbound.
	functor(Literal, F, A), % Only for debugging.
	debug_message("Performing cover-check for predicate IN SCC: ~q.", [F/A]
	),
	get_info_for_literal_from_predicate(LitPredEntry, Clauses, ModeType,
	    _PredCovFlag, _PredNF_Flag),
	actualize_flags_for_SCC_literal(Clauses, ModeType, UseMasc, Masc,
	    Cover_flag, Nfail_flag, Change).



% End of fixpoint computation.

% Begin process after fixpoint.

:- pred process_SCC_after_fixpoint(+SCC, TAB)

# "Compute the non-fail and cover flag for each predicate in
    @var{SCC}.  For all literals in the body of these predicates, set
    the non-fail and cover flag to true in the case they are
    unbound. The cover flag of a predicate is set to true if the
    disjunction of the tests of all the clauses of the predicate
    covers the type of the predicate, otherwise, the cover flag is set
    to false. The non-fail flag of a predicate is set to true if the
    disjunction of the tests of the non-failing clauses of the
    predicate covers the type of the predicate, otherwise, the
    non-fail flag is set to false.".

process_SCC_after_fixpoint([],            _TAB) :- !.
process_SCC_after_fixpoint([SccPred|SCC], TAB) :-
% find the information corresponding to the predicate in the SCC   
	find_entry(TAB, SccPred, PredEntry),
	get_nf_info(PredEntry, Clauses, ModeType, _Test, Nfail_flag,
	    Cover_flag),
	compute_predicate_flags(SccPred, Clauses, ModeType, Cover_flag,
	    Nfail_flag),
	process_all_clauses_after_fixpoint(Clauses, PredEntry, TAB),
	process_SCC_after_fixpoint(SCC, TAB).

process_all_clauses_after_fixpoint(Clauses, _PredEntry, _TAB) :-
	there_are_no_more_clauses(Clauses),
	!.
process_all_clauses_after_fixpoint([Clause|CList], PredEntry, TAB) :-
	get_body_of_clause(Clause, Body),
	process_one_body_after_fixpoint(Body, PredEntry, TAB),
	process_all_clauses_after_fixpoint(CList, PredEntry, TAB).

process_one_body_after_fixpoint([],              _PredEntry, _TAB) :- !.
process_one_body_after_fixpoint([Litinfo|LList], PredEntry,  TAB) :-
	get_lit_info_for_nonfail(Litinfo, _LitPred, _Literal, Nfail_flag,
	    Cover_flag),
	process_one_literal_after_fixpoint(Litinfo, Nfail_flag, Cover_flag),
	process_one_body_after_fixpoint(LList, PredEntry, TAB).

process_one_literal_after_fixpoint(Litinfo, Nfail_flag, Cover_flag) :-
	get_test_flag_value(Litinfo, TestFlag),
	( literal_is_a_test(TestFlag)
	-> true
	;
	    ( flag_is_unbound(Nfail_flag) -> set_flag_value_true(Nfail_flag) ;
		true ),
	    ( flag_is_unbound(Cover_flag) -> set_flag_value_true(Cover_flag) ;
		true ) ).

% End process after fixpoint.

%% BEGIN COVER CHECK



literals_meet_selection_condition([]) :- !.
literals_meet_selection_condition([Litinfo|LList]) :-
	get_lit_info_for_nonfail(Litinfo, _LitPred, _Literal, Nfail_flag,
	    _Cover_flag),
	get_test_flag_value(Litinfo, TestFlag),
	( literal_is_a_test(TestFlag) ; flag_is_unbound(Nfail_flag) ;
	    flag_is_true(Nfail_flag) ),
	literals_meet_selection_condition(LList).

%% END COVER CHECK

%% Primero ver si todos los test cubren el tipo. Si lo cubren entonces el
%% cover flag toma el valor cubierto.  Si no ver las clausulas que tienen
%% todos los cuerpos que no fallan (las que tienen algun literal que no
%% tienen valor en el cover flag no se incluyen, es decir aquellos en el
%% SCC).  Si no ver las clausulas que tienen todos los cuerpos que no
%% fallan mas aquellas de los predicados del SCCG que aun no tienen valor
%% en el cover flag.

%% If the disjunction of the tests of all clauses of a predicate COVERS
%% the type of the predicate, then it is posible that the disjunction of
%% a subset of the clauses (excluding the set of all clauses) DO NOT
%% cover the type of a call to that predicate (this type can be a subtype
%% of the type of the predicate, or the type of the predicate).
%% 
%% If the disjunction of the tests of all clauses of a predicate DOES NOT
%% the type of the predicate, then it is posible that the disjunction of
%% the tests of a subset of the clauses (possibly all the clauses) COVERS
%% the type of a call to that predicate (in the case that this type is a
%% subtype of the type of the predicate).
%% 21 Oct 98 PLG
