:- module(_, [
		exectime_ub/2,
		exectime_lb/2,
		exectime/3,
		read_exectime_file/1,
		fill_lit_wamcode_db/0,
		exectime_of_retry/6,
		gen_profile_file/1,
		failed_tm/2,
		cleanup_exectime_db/0],
	    [runtime_ops, assertions, regtypes, fsyntax,
		resources(inferres_decl)]).

:- use_module(library(messages)).
:- use_module(library(read)).
:- use_module(library(system)).
:- use_module(library(terms)).
:- use_module(library(timingmodel)).
:- use_module(resources(resources_basic)).
:- use_module(sta_exectime(sta_exectime_db)).
:- use_module(sta_exectime(sta_exectime_calibration)).
:- use_module(program(itf_base_db), [curr_file/2]).
:- use_module(library(assertions(assrt_lib)),
	    [normalize_assertion/9, assertion_body/7]).

:- doc(author, "Edison Mera").


init_resource(sta_exectime_res) :-
	lazy_calibration,
	(
	    curr_file(FileName, _Module) ->
	    cleanup_exectime_db,
	    read_exectime_file(~timing_model_file_db),
	    compile_file(FileName),
	    atom_concat(FileBase, '.pl', FileName),
	    read_exectime_file(~atom_concat(FileBase, '.wam')),
	    % This file contains info that should be gathered by ciaopp, but that
	    % currently is not available:
	    % 	    read_exectime_file( ~atom_concat( FileBase, '.klu' ) ),
	    fill_lit_wamcode_db
	;
	    true
	).

fill_lit_wamcode_db :-
	wamcode_db(ClauseKey, Insts),
	lit_wamcode(Insts, ClauseKey, 0, L, L),
	fail
    ;
	true.

exectime_ub(LitInfo, Cost) :-
	exectime(ub, LitInfo, Cost).

exectime_lb(LitInfo, Cost) :-
	exectime(lb, LitInfo, Cost).

exectime(Approx, LitInfo, Cost) :-
	litinfo_get_litnum(LitInfo, LitNum),
	litinfo_get_key(LitInfo, ClauseKey),
	%	litinfo_get_measure( LitInfo, Measure ),
	conversion_unit_db(Unit),
	get_exectime_cost(Approx, ClauseKey, LitNum, Cost0),
	Cost is Cost0 * Unit,
	!.
exectime(_, _, 0).

get_exectime_cost(Approx, ClauseKey, LitNum, Cost) :-
	atom_number(ALitNum, LitNum),
	atom_concat([_Module, ':', Name, '/', AArity, '/', AClauseNum],
	    ClauseKey),
	(
	    LitNum == 0 ->
	    atom_concat([Name, '/', AArity], PredKey),
	    atom_number(AClauseNum, ClauseNum),
	    exectime_of_retry(Approx, PredKey, 1, ClauseNum, 0, Cost0)
	;
	    Cost0 = 0
	),
	atom_concat([Name, '/', AArity, '/', AClauseNum, '/', ALitNum],
	    LitKey),
	lit_wamcode_cost(Approx, LitKey, Cost0, Cost).

assertz_lit_wamcode(ClauseKey, LitNum, LitInst) :-
	atom_number(ALitNum, LitNum),
	atom_concat([ClauseKey, '/', ALitNum], LitKey),
	assertz_fact(lit_wamcode_db(LitKey, LitInst)).

lit_wamcode([], ClauseKey, LitNum, LitInst, []) :-
	assertz_lit_wamcode(ClauseKey, LitNum, LitInst).
lit_wamcode([Inst|Insts], ClauseKey, LitNum, LitInst0, LitInst) :-
	Inst = i(_Offset, WamCode),
	(
	    functor(WamCode, WCName, _),
	    init_new_literal(WCName) ->
	    LitInst = [],
	    assertz_lit_wamcode(ClauseKey, LitNum, LitInst0),
	    LitNum1 is LitNum + 1,
	    lit_wamcode(Insts, ClauseKey, LitNum1, [Inst|LitInst1],
		LitInst1)
	;
	    LitInst = [Inst|LitInst1],
	    lit_wamcode(Insts, ClauseKey, LitNum, LitInst0, LitInst1)
	).

failed_tm(fail, fails(_)).
failed_tm(succ, not_fails(_)).

timing_model(cn,     _,    _,      1) :- !.
timing_model(Approx, Inst, Failed, CostExpr) :-
	Inst = i(_Offset, WamCode),
	(
	    functor(WamCode,  ByteCode, Arity),
	    functor(WamCode0, ByteCode, Arity),
	    timing_model_assrt_db(WamCode0, Comp),
	    failed_tm(Failed, FailedAssrt),
	    member(FailedAssrt, Comp) ->
	    true
	;
	    show_message(warning, "No assertion for instruction ~w (~w)",
		[WamCode, Failed]),
	    CostExpr = 0
	),
	(
	    member(cost(_, Approx, exectime, CostExpr), Comp) ->
	    true
	;
	    show_message(warning, "No timing model for instruction ~w (~w)",
		[Inst, Failed]),
	    CostExpr = 0
	).

lit_wamcode_cost(Approx, LitKey, Cost0, Cost) :-
	lit_wamcode_db(LitKey, Insts),
	wamcode_cost(Insts, Approx, Cost0, Cost).

exectime_of_retry(_Approx, _PredKey, ClauseNum, ClauseNum, Cost, Cost) :-
	!.
exectime_of_retry(Approx, PredKey, ClauseIdx, ClauseNum, Cost0, Cost) :-
	atom_number(AClauseIdx, ClauseIdx),
	atom_concat([PredKey, '/', AClauseIdx], ClauseKey),
	exectime_of_retry_one(Approx, ClauseKey, Cost0, Cost1),
	ClauseIdx1 is ClauseIdx + 1,
	exectime_of_retry(Approx, PredKey, ClauseIdx1, ClauseNum,
	    Cost1, Cost).

exectime_of_retry_one(Approx, ClauseKey, Cost0, Cost) :-
	wamcode_db(ClauseKey, Insts),
	wamcode_cost_until_fail(Insts, Approx, Cost0, Cost).

wamcode_cost_until_fail([],           _,      Cost,  Cost).
wamcode_cost_until_fail([Inst|Insts], Approx, Cost0, Cost) :-
	Inst = i(_Offset, WamCode),
	WamCode =.. [ByteCode|_Args],
	( can_fail(ByteCode) ->
	    timing_model(Approx, Inst, fail, Cost1),
	    Cost is Cost0 + Cost1
	;
	    timing_model(Approx, Inst, succ, Cost1),
	    Cost2 is Cost0 + Cost1,
	    wamcode_cost_until_fail(Insts, Approx, Cost2, Cost)
	).

wamcode_cost([],           _,      Cost,  Cost).
wamcode_cost([Inst|Insts], Approx, Cost0, Cost) :-
	timing_model(Approx, Inst, succ, Cost1),
	Cost2 is Cost0 + Cost1,
	wamcode_cost(Insts, Approx, Cost2, Cost).

read_exectime_file(AFileName) :-
	absolute_file_name(AFileName, FileName),
	(
	    file_exists(FileName) ->
	    show_message(note, "Loading file ~w", [FileName]),
	    read_exectime_file_(FileName)
	;
	    show_message(warning, "Unable to load file ~w", [FileName])
	).

read_exectime_file_(FileName) :-
	open(FileName, read, S),
	read_items(S),
	!,
	close(S).

read_items(S) :-
	read(S, R),
	(
	    R == end_of_file ->
	    true
	;
	    ( R = (:- Assrt) ->
		normalize_assertion(user, Assrt, Pred, true, pred, NABody,
		    _, _, _),
		assertion_body(Pred, _Compat, _Call, _Succ, Comp, _Comm,
		    NABody),
		assertz_fact(timing_model_assrt_db(Pred, Comp))
	    ;
		assertz_fact(R)
	    ),
	    read_items(S)
	).

gen_profile_file(FileBase) :-
	compile_file(FileBase).

cleanup_exectime_db :-
	retractall_fact(wamcode_db(_, _)),
	retractall_fact(lit_wamcode_db(_, _)),
	retractall_fact(timing_model_assrt_db(_, _)),
	retractall_fact(wamcode_params_db(_, _, _)).
