:- module(time_res, [time_analysis/10],
	    [assertions, resources(inferres_decl),
		library(resdefs(resources_decl))]).

:- doc(author, "Nai-Wei Lin").
:- doc(author, "Pedro L@'{o}pez").
:- doc(module, "This file contains the procedures for performing
   the time analysis for the predicates in the program in
   topologically sorted order.  Perform the time analysis for a
   strongly connected component. Original version from December,
   1991. Modified by PLG and EMM.").

:- use_module(library(messages), [debug_message/2]).

:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(infer(infer),             [get_info/5]).
:- use_module(infer(infer_dom),         [flag_set/3, flag_is/3]).
:- use_module(resources(algebraic_res(simpl_form_res)),
	    [time_simplification/2]).
:- use_module(resources(dependency_res(position_res)),
	    [gen_clause_pos/2, gen_literal_iopos/5]).
:- use_module(resources(resources_basic)).
:- use_module(resources(init_res(symtable_res)),
	    [
		literal_property/10,
		find_symbol_field/4,
		insert_symbol_field/4
	    ]).
:- use_module(resources(init_res(builtin_res)),
	    [
		second_order_predicate/1,
		second_order_predicate_pred_arg/2,
		second_order_predicate_pred_num/3,
		find_entry_trusted_field/6
	    ]).
:- use_module(resources(init_res(initsystem_basic_res)),
	    [clause_type/2]).
:- use_module(resources(size_res(normalize__res)),
	    [
		init_normalize_queue/3,
		literal_output_comp/15,
		normalize_time/15
	    ]).
:- use_module(resources(size_res(clause_res)),
	    [ith_clause_literal/3]).
:- use_module(resources(size_res(size__res)),
	    [remove_recursive_comps/4]).
:- use_module(resources(solution_res(comp_diff_equ_res)),
	    [solve_time_complexity_equ/7]).
:- use_module(resources(solution_res(binding_res)),
	    [normalize_solution_function/14]).
:- use_module(resources(solution_res(relation_res)),
	    [recursive_clause/2]).
:- use_module(resources(top_res(utility_res)),
	    [
		time_addition/3,
		time_multiply/3,
		multiply/3,
		nonsequence/1,
		member/2
	    ]).
:- use_module(resources(res_assrt_defs(resources_trust)),
	    [apply_resource_assertion/7]).
:- use_module(infercost(cost_approx)).

:- entry time_analysis/10: list * list(resource) * approx * term * term *
	list * list * list * list * var.
% rtcheck -- EMM
time_analysis(Comp, Resources, Approx, BT, ST, Size, Adg, Gvars, Ldg, Time) :-
	time_analysis_(Comp, Resources, Approx, BT, ST, Comp, Size, Adg, Gvars,
	    Ldg, [], Time).

:- doc(bug, "Note that if Time4 is partially instantiated, it is not
	necessary to do the resource analysis for all the resources,
	but only for those that does not have trust cost assertions.
	-- EMM").

:- comp time_analysis_/12 + not_fails.
% rtcheck -- EMM
time_analysis_([], _, _, _, _, _, _, _, _, _, _, []).
time_analysis_([Pred|CompList], Resources, Approx, BT, ST, Comp, [Size|SList],
	    [Adg|AList], [Gvars|GList], [Ldg|LList], RTime, [Time|TList]) :-
	find_symbol_field(ST, Pred, resources(Resources), Time1),
	(
	    nonground(Time1) ->
	    find_entry_trusted_field(resources(Resources), BT, ST, Pred,
		Approx, Time4),
	    (
		ground(Time4) ->
		true
	    ;
		approx_to_bound(Approx, Bound),
		find_symbol_field(ST, Pred, clause, ClauseKeys),
		bound_time_predicate(Bound, Resources, Approx, Pred,
		    ClauseKeys, BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime,
		    Time2),
		debug_message("COST (recurrence equations):~q ~n", [Time2]),
		solve_time_complexity_equ(Time2, Pred, Bound, ST, Comp, Size,
		    TTime),
		debug_message("COST (closed form):~q ~n", [TTime]),
		Time3 = [TTime],
		combine_results(time, Time4, Time3)
	    )
	;
	    debug_message("Cost (closed form, equation was computed) ~n ~q ~n",
		[Time1]),
	    Time4 = Time1
	),
	time_analysis_(CompList, Resources, Approx, BT, ST, Comp,
	    SList, AList, GList, LList, [comp(Pred, Time4)|RTime], TList),
	(
	    nonground(Time1) ->
	    (remove_recursive_comps(Time4, ST, resources(Resources), Time5)
                 -> Time = Time5
                  ; set_top_time(Time)
            ),
 	    debug_message("COST (closed form) after assertions for ~q:~q ~n",
		[Pred, Time]),
	    insert_symbol_field(ST, Pred, resources(Resources), Time)
	;
	    true
	).


set_top_time(X):- 
	approximation(Approx),
	set_top_time_(Approx, X).

set_top_time_(upper, inf).
set_top_time_(lower, 0).


:- pred bound_time_predicate/14 + not_fails.

bound_time_predicate(upper, Resources, Approx, _Pred, ClauseKeys, BT, ST, Comp,
	    Size, Adg, Gvars, Ldg, RTime, Time2) :-
	time_clauses(ClauseKeys, void, Resources, Approx, BT, ST, Comp, Size,
	    Adg, Gvars, Ldg, RTime, Time2).
bound_time_predicate(lower, Resources, Approx, Pred, ClauseKeys, BT, ST, Comp,
	    Size, Adg, Gvars, Ldg, RTime, Time2) :-
	lower_bound_time_predicate(Pred, ClauseKeys, Resources, Approx, BT, ST,
	    Comp, Size, Adg, Gvars, Ldg, RTime, Time2).

:- pred non_fail_cover_pred_info(PredName, FailInfo, CoverInfo) ::
	(PredName = F/A) # "Check
   whether a predicate is non-failing (definitely will not fail)".

non_fail_cover_pred_info(F/A, FailInfo, CoverInfo) :-
	functor(Head, F, A),
	get_info(nonfail, pred, F/A, Head, [FailInfo, CoverInfo]),
	!.
non_fail_cover_pred_info(_, FailFlag, CoverFlag) :-
	flag_set(nf, possibly_fails, FailFlag),
	flag_set(nf, not_covered,    CoverFlag).

%% successful_pred(Head, FailInfo) :-
%% 	functor(Head, F, A),
%% 	functor(Head0, F, A),
%% 	trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo),!.
%% 	% is_renaming(Head, Head0), !.
%% successful_pred(_Head, possible_fail).

%% is_a_non_failing_pred(F/A) :-
%%    functor(Head, F, A),
%%    successful_pred(Head, FailInfo),
%%    FailInfo == not_fail.

%% Perform lower bound time analysis -PLG 
%% 
%% CASES:
%% ------
%% 
%% 1) If a PREDICATE is NON-FAILING: then at least one of its clauses
%%  will not fail, and thus, a lower bound on its cost is the minimum of
%%  of the costs of (all of) its clauses (including the ones which have
%%  some literal which has not been proven to be non-failing). And the
%%  cost of a clause is the sum of the costs of all of its literals.
%% 
%% 2) If a PREDICATE is FAILING (non-failure is not ensured), then we have
%% two cases:
%% 
%%   A) THE PREDICATE IS COVERED: then there is at least one clause whose
%%      test will not fail. Then, a lower bound on its cost is the
%%      minimum of the costs of (all of) its clauses. In this case,
%%      the cost of a clause is the sum of the cost of the sequence of
%%      non-failing literals just before the first literal which is not
%%      ensured not to fail.
%% 
%%   B) THE PREDICATE IS NOT COVERED: Then, a lower bound on its cost is
%%      zero (since all head unifications can fail in the worst case).
%% 
%% TREATEMENT OF THE CUT
%% ---------------------
%% 
%% 1) If a PREDICATE is NON-FAILING: ignore. 
%% 
%% 2) If a PREDICATE is FAILING:
%% 
%%   A) THE PREDICATE IS COVERED:  ignore. Note that the implicit test of a
%%    cut is taken into account in the following clauses.
%% 
%%   B) THE PREDICATE IS NOT COVERED: ignore.
%% 
%% INPROVEMENT:
%% ------------
%% 
%%    NOTE however that it can be possible to get a more accurate
%%  lower-bound by discarding some of the clauses. For example, by taking
%%  the CUT into account.
%% 
%% Cases:
%% ------
%% 
%%   i) Detecting a NON-FAILING CUT:
%%   Suppose C1, C2, C3, ..., Cn are clauses such that there is a
%% non-failing cut on Cn, then the clauses Cn+1, Cn+2, ..., Cm can be
%% ignored (in fact, they are dead code).
%% 
%%   A cut is non-failing if all preceding clauses and a restriction of
%% Cn to the literals before the cut are a non-failing set, that is, at
%% least one of the clauses will succeed.
%% 
%%  NOTE that NON-FAILING, covered + FAILING, not covered predicates can
%% have non-failing cuts.
%% 
%% ii) SEARCHING FOR ONLY ONE SOLUTION:
%%   If we are searching for only one solution, and C1, C2, C3,...,Cn 
%%   constitute a non-failing set (that is, at least one of the clauses
%%   will succeed) then the clauses Cn+1, Cn+2, ..., Cm can be ignored
%%   (in fact, they are dead code).
%% 
%% If we are searching for only one solution and find both, a non-failing
%% set of clauses and a failing cut, then use the option that discard the
%% maximun number of clauses.

%% Literals which are builtins that act either as tests or assignments
%% are ignored in the cost analysis since: tests are used to check
%% covering, and assignments always succeed.

lower_bound_time_predicate(Pred, ClauseKeys, Resources, Approx, BT, ST, Comp,
	    Size, Adg, Gvars, Ldg, RTime, Time2) :-
	debug_message("CALL: ~q", [non_fail_cover_pred_info(Pred, FailInfo,
		    CoverInfo)]),
	non_fail_cover_pred_info(Pred, FailInfo, CoverInfo),
	nf_info_to_fail_cost(FailInfo, CoverInfo, FailCost),
	(
	    FailCost == zero ->
	    set_zero_cost(ClauseKeys, Resources, Time2)
	;
	    time_clauses(ClauseKeys, FailCost, Resources, Approx, BT, ST, Comp,
		Size, Adg, Gvars, Ldg, RTime, Time2)
	).

nf_info_to_fail_cost(FailInfo, _CoverInfo, nf) :-
	flag_is(nf, not_fails, FailInfo),
	!.
nf_info_to_fail_cost(FailInfo, CoverInfo, pfc) :-
	flag_is(nf, possibly_fails, FailInfo),
	flag_is(nf, covered,        CoverInfo),
	!.
nf_info_to_fail_cost(_FailInfo, _CoverInfo, zero).

:- pred set_zero_cost(Clauses, Resources, ZeroCost) # " Create a list
   of length equal to the number of clauses of the predicate with
   zeros. I.e. set the cost of each clause to zero.".

set_zero_cost(Clauses, Resources, ZeroCost) :-
	zero_time(Resources, Zero),
	set_zero_cost_(Clauses, Resources, Zero, ZeroCost).

set_zero_cost_(Clauses, _,         _,    []) :- var(Clauses), !.
set_zero_cost_(Clauses, Resources, Zero, [Zero|Time2]) :-
	nonvar(Clauses),
	Clauses = [_|Clauses1],
	set_zero_cost(Clauses1, Resources, Time2).

zero_time(Resources, Zero) :-
	zero_resources(Resources, Zero).

zero_resources([],            []).
zero_resources([_|Resources], [0|Zeros]) :-
	zero_resources(Resources, Zeros).

:- pred time_clauses(ClauseKey, FailCost, Resources, Approx,
	    BT, ST, Comp, SList, AList, GList, LList, RTime, Time)
	: approx(Approx)
# "Perform the time analysis for the set of clauses in a predicate.".

time_clauses(ClauseKeys, _FailCost, _Resources, _Approx, _BT, _ST, _Comp,
	    _SList, _AList, _GList, _LList, _RTime, []) :-
	var(ClauseKeys),
	!.
time_clauses([ClauseKey|CList], FailCost, Resources, Approx, BT, ST, Comp,
	    [Size|SList], [Adg|AList], [Gvars|GList], [Ldg|LList], RTime,
	    [Time|TList]) :-
	clause_key(ClauseKey, ClausePPKey, Key),
	time_clause(FailCost, ClausePPKey, Key, Resources, Approx, BT, ST,
	    Comp, Size, Adg, Gvars, Ldg, RTime, Time1),
	time_simplification(Time1, Time),
	time_clauses(CList, FailCost, Resources, Approx, BT, ST, Comp, SList,
	    AList, GList, LList, RTime, TList).

:- pred time_clause(FailCost, ClausePPKey, Key, Resources, Approx, BT, ST,
	    Comp, Size, Adg, Gvars, Ldg, RTime, Time)
	: approx(Approx) + (not_fails, is_det)
# "Perform the time analysis for a clause.".
% rtcheck -- EMM
time_clause(FailCost, ClausePPKey, Key, Resources, Approx, BT, ST, Comp, Size,
	    Adg, Gvars, Ldg, RTime, Time) :-
	clause_type(ClausePPKey, Type),
	time_clause_(Type, FailCost, ClausePPKey, Key, Resources, Approx, BT,
	    ST, Comp, Size, Adg, Gvars, Ldg, RTime, Time).
time_clause_(rule, FailCost, ClausePPKey, Key, Resources, Approx, BT, ST, Comp,
	    Size, Adg, Gvars, Ldg, RTime, Time) :-
	clause_head_body(ClausePPKey, Head, Body),
	get_litinfo(Head, 0, Key, BT, ST, ClausePPKey, noinfo, Approx,
	    LitInfo),
	apply_resource_assertion(Resources, head, BT, Approx, LitInfo,
	    HeadCosts, _),
	time_body(FailCost, Body, 1, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, 1, RTime, Time1),
	time_addition(Time1, HeadCosts, Time).
time_clause_(fact, _FailCost, ClausePPKey, Key, Resources, Approx, BT, ST, _,
	    _, _, _, _, _, Time) :-
	clause_head(ClausePPKey, Head),
	get_litinfo(Head, 0, Key, BT, ST, ClausePPKey, noinfo, Approx,
	    LitInfo),
	apply_resource_assertion(Resources, head, BT, Approx, LitInfo, Time,
	    _).
:- pred time_body/17 + not_fails.

time_body(void, Body, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	upper_time_body(Body, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time).
time_body(pfc, Body, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	lower_time_body(Body, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time).
time_body(nf, Body, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	nf_lower_time_body(Body, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time).

%
%  Perform the time analysis for the body of a clause.
%

upper_time_body((LitPPKey, Body), LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	!, % Added by PLG cut (1-Sep-97)
	lit_ppkey(LitPPKey, Lit, PPKey),
	time_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime, Time1),
	(
	    Lit == (!) ->
	    Time2 = Time1
	;
	    time_multiply(Time1, Times, Time2)
	),
	(
	    Lit == (!) ->
	    Times1 = 1
	;
	    ( current_pp_flag(prog_lang, ciao) ->
		frequency_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Approx,
		    BT, ST, Comp, Size, Adg, Gvars, _, _, Sol2),
		multiply(Times, Sol2, Times1)
	    ;
		Times1 = 1
	    )
	),
	LitNum1 is LitNum + 1,
	upper_time_body(Body, LitNum1, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times1, RTime, Time3),
	time_addition(Time2, Time3, Time).
upper_time_body(LitPPKey, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	lit_ppkey(LitPPKey, Lit, PPKey),
	nonsequence(Lit),
	time_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Resources,
	    Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime, Time1),
	time_multiply(Time1, Times, Time).

% Added by PLG (23-Mar-97)

lower_time_body((LitPPKey, Body), LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times,
	    RTime, Time) :-
	!,
	lit_ppkey(LitPPKey, Lit, PPKey),
	(
	    not_ending_literal(Lit) ->
	    time_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Resources,
		Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime, Time1),
	    LitNum1 is LitNum + 1,
	    lower_time_body(Body, LitNum1, ClausePPKey, Key, Resources,
		Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, Times,
		RTime, Time3),
	    time_addition(Time1, Time3, Time)
	;
	    zero_time(Resources, Time)
	).
lower_time_body(LitPPKey, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	lit_ppkey(LitPPKey, Lit, PPKey),
	nonsequence(Lit),
	(
	    not_ending_literal(Lit) ->
	    time_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Resources,
		Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime, Time1),
	    time_multiply(Time1, Times, Time)
	;
	    zero_time(Resources, Time)
	).

nf_lower_time_body((LitPPKey, Body), LitNum, ClausePPKey, Key, Resources,
	    Approx, BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	!,
	lit_ppkey(LitPPKey, Lit, PPKey),
	time_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime, Time1),
	LitNum1 is LitNum + 1,
	nf_lower_time_body(Body, LitNum1, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time3),
	time_addition(Time1, Time3, Time).
nf_lower_time_body(LitPPKey, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, Times, RTime, Time) :-
	lit_ppkey(LitPPKey, Lit, PPKey),
	nonsequence(Lit),
	time_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, Ldg, RTime, Time1),
	time_multiply(Time1, Times, Time).

%
%  Perform the lower bound time analysis for the body of a clause.
%

%% Literals which are builtins that act either as tests or assignments
%% are ignored in the cost analysis since: tests are used to check
%% covering, and assignments always succeed.

not_ending_literal(X) :-
	test_or_assignment(X),
	!.
not_ending_literal(X) :-
	functor(X, F, A),
	debug_message("CALL: ~q", [non_fail_cover_pred_info(F / A,
		    FailInfo, _CoverInfo)]),
	non_fail_cover_pred_info(F / A, FailInfo, _CoverInfo),
	debug_message("EXIT: ~q", [non_fail_cover_pred_info(F / A,
		    FailInfo, _CoverInfo)]),
	flag_is(nf, not_fails, FailInfo).

test_or_assignment(X) :-
	var(X),
	!,
	fail.
test_or_assignment(\+(X)) :-
	!,
	test_or_assignment(X).
test_or_assignment(X) :-
	functor(X, F, A),
	utility_res:member(
	    [(!) /1,
		(=) /2,
		(==) /2,
		(\==) /2,
		(is) /2,
		(=:=) /2,
		(=\=) /2,
		(<) /2,
		(>) /2,
		(=<) /2,
		(>=) /2,
		(number) /1,
		(integer) /1,
		(atom) /1,
		(atomic) /1
	    ], F / A).

% End added
%%%%%%%%%%%%%%%%%%%% JNL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% JNL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred time_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, _Ldg, RTime, Time) : approx(Approx)
# " Perform the time analysis for a literal.".
% rtcheck -- EMM
time_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Resources, Approx,
	    BT, ST, Comp, Size, Adg, Gvars, _Ldg, RTime, Time) :-
	functor(Lit, F, A),
	(
	    second_order_predicate(F/A) ->
	    second_order_predicate_pred_arg(Lit, Lit1),
	    functor(Lit1, F1, A1),
	    ClausePPKey = (_ :- Body),
	    second_order_predicate_pred_num(Body, LitNum, Num1),
	    PPKey1 = PPKey % I don't know if this is correct -- EMM
	;
	    F1 = F,
	    A1 = A,
	    Num1 = LitNum,
	    PPKey1 = PPKey,
	    Lit1 = Lit
	),
	literal_output_comp(F1/A1, Lit1, ClausePPKey, Key, PPKey1, Num1, 1,
	    Approx, BT, ST, Comp, Adg, resources(Resources), RTime, LitTime),
% normalize cost function inferred by the analysis    
	normalize_time_function(LitTime, F1/A1, Num1, Approx, BT, ST, Comp,
	    ClausePPKey, Key, Adg, Gvars, Size, RTime, Time0),
	get_litinfo(Lit1, LitNum, Key, BT, ST, ClausePPKey, PPKey1, Approx,
	    LitInfo),
	apply_resource_assertion(Resources, literal, BT, Approx, LitInfo,
	    UCosts0, _),
% normalize cost function provided by the user
	normalize_time_function(UCosts0, F1/A1, Num1, Approx, BT, ST,
	    Comp, ClausePPKey, Key, Adg, Gvars, Size, _, UCosts),
	time_addition(Time0, UCosts, Time).
% 	;
% 	    Time = Time0
% 	).

frequency_literal(Lit, PPKey, LitNum, ClausePPKey, Key, Approx, BT, ST, Comp,
	    Size, Adg, Gvars, _, _, Sol) :-
	functor(Lit, F, A),
	literal_output_comp(F/A, Lit, ClausePPKey, Key, PPKey, LitNum, 1,
	    Approx, BT, ST, [], Adg, det, [], Sol1),
	normalize_solution_function(Sol1, F/A, LitNum, Approx, BT, ST, Comp,
	    ClausePPKey, Key, Adg, Gvars, Size, [], Sol).

:- pred normalize_time_function(LitTime, LitName, LitNum, Approx, BT, ST, Comp,
	    ClausePPKey, Key, Adg, Gvars, Size, RTime, Time) : approx(Approx)
# "Normalize the time function of a literal.".
% rtcheck -- EMM
normalize_time_function(LitTime, LitName, LitNum, Approx, BT, ST,
	    Comp, ClausePPKey, Key, Adg, Gvars, Size, RTime, Time) :-
	gen_clause_pos(Adg, PosSet),
	(
	    recursive_clause(ClausePPKey, Comp) ->
	    (
		ith_clause_literal(0, ClausePPKey, LitPPKey),
		lit_ppkey(LitPPKey, Lit, PPKey),
		literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum,
		    size, Approx, ISz)
	    )
	;
	    ISz = Size
	),
	gen_literal_iopos(Adg, LitName, LitNum, (+), Pos),
	init_normalize_queue(Pos, QHd, QTl),
	normalize_time(LitTime, QHd, QTl, Approx, BT, ST, [], ClausePPKey, Key,
	    Adg, Gvars, PosSet, ISz, RTime, Time).
