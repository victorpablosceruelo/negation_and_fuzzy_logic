% ---------------------------------------------------------------------------
% Operators

%:- initialization(true). % !! workaround for ciao-1.10-6: ignores next `:- op' otherwise
:- op(700, xfx, '<--').

:- op(700, xfx, '=~~').
:- op(550, xfy, :).
:- op(500, xfx, @).
:- op(500, fx, @).


% ---------------------------------------------------------------------------
% main entries

inoisy :-
	iso_main. 				% go to testbed.pl

iquiet :-
	iso_main([debug(quiet)]). 		% go to testbed.pl

main :-
	main([]).

main(Opts0) :- 
	spec_options(Opts1),
	append(Opts0, Opts1, Opts),
	iso_main([debug(quiet)|Opts]). 		% go to testbed.pl

% ----------------------------------------------------------------------
% Callback entry points from testbed.pl

:- dynamic(initial_predicates/1).

init_tests(Opts, Opts) :-
	retractall(initial_predicates(_)),
	findall(PI, current_test_predicate(PI), Preds),
	asserta(initial_predicates(Preds)),
	init_tests_special,
	prolog_cleanup.

test(Name, Opts0, Opts0, do_test(Goal, Vars, Result, SideEff, Opts), Type, prolog_cleanup) :-
	get_test(Name, Type, Goal0, Program1, Result, SideEff, Opts0), % Dat: succeeds many times
	%%%% pts %%%% !! vvv only if non-verbose
	write(user_output, 'testing '), %%%% pts %%%%
	write(user_output, Name), nl(user_output), %%%%% pts %%%% !! for debugging
	% !! vvv only if debugging
	% write(user_error, 'testing '), %%%% pts %%%%
	% write(user_error, Name), nl(user_error), %%%%% pts %%%% !! for debugging
	term_variables(Goal0, Vars),
	Opts = [goal(Goal0),vars(Vars),sideeff(SideEff),result(Result)|Opts0],
	test_module_qualified(Goal0, Goal1),	
	opt_compile_goal(Goal1, Vars, Program1, Goal, Program, Opts),
	load_program(Program, Opts).

opt_compile_goal(Goal0, Vars, Program0, Goal, [(Goal:-Goal0)|Program0], Opts) :-
	option(comp_goal, Opts), !,
	Goal =.. ['$goal'|Vars].
opt_compile_goal(Goal, _Vars, Program, Goal, Program, _Opts).

extract_variant((Descr1;Descr2), Variant, Opts) :- !,
	Variant = (Variant1;Variant2),
	extract_variant(Descr1, Variant1, Opts),
	extract_variant(Descr2, Variant2, Opts).
extract_variant([Descr1|Descr2], Variant, Opts) :- !,
	Variant = [Variant1|Variant2],
	extract_variant(Descr1, Variant1, Opts),
	extract_variant(Descr2, Variant2, Opts).
extract_variant(variant(Id,List), Variant, Opts) :- !,
	extract_variant(Id, List, Variant, Opts).
extract_variant(Descr, Descr, _).

extract_variant(Id, List, Variant, Opts) :-
	(   select_variant(Id, Case, Opts) -> true
	;   true       % Case = _, unknown - take the first variant
	),
	memberchk(Case-Variant, List).

select_variant(modules, Case, _Opts) :- !,
	(   module_qualified(_, _, _) -> Case = yes
	;   Case = no
	).
select_variant(Id, Case, Opts) :-
	option(Id=Case0, Opts), !, Case = Case0.
select_variant(Id, Case, _) :-
	context_info(Id, Case).



% ----------------------------------------------------------------------
% Clean-up

:- dynamic(user_op/2).


% list_diff(L1, L2, L21, L12).
%    L21 is L2 - L1
%    L12 is L1 - L2
list_diff([], L, L, []).
list_diff([X|L1], L2,  D1, D2) :-
	select(X, L2, L2N), !,
	list_diff(L1, L2N, D1, D2).
list_diff([X|L1], L2, D1, [X|D2]) :-
	list_diff(L1, L2, D1, D2).
	
prolog_cleanup :-
	current_preds(Preds),
	%write(user_error,cp=Preds), nl(user_error), %%%% pts %%%% !!
	member(Pred, Preds),
	test_module_qualified(Pred, MPred),
	abolish_static(MPred),
	%write(user_error,ok), nl(user_error),
	fail.
prolog_cleanup :-
	user_op(Spec, Op),
	op(0, Spec, Op),
	fail.
prolog_cleanup :-
	close_open_streams,
	reset_portray_expect,
	% reset flags to default, that are changed by test-runs...
	set_prolog_flag(unknown, error).
	

close_open_streams :-
	% !! ciao SUXX: current_predicate and predicate_property don't work
	% vvv Dat: ciao doesn't have stream_propery/2
	catch((stream_property(S, file_name(_)), close(S)), _, true),
	fail.
close_open_streams.

% ----------------------------------------------------------------------
% Picking up tests

% !! protect for bad arity of test_case/_

get_test(Name, Type, Goal, Program, Result, SideEff, Opts) :-
	% Dat: type is the test author !! doc:, see test_cases.pl
	test_case(Name, Type, Goal, Result0, Others0),	% see test_cases.pl
	\+ test_ok(Name), % Dat: fail if test has already succeeded (this is good for interactive reloads)
	expand_sideffs(Others0, Others, Opts),
	(   select(prog(Prog), Others, SideEff) ->
	    get_program(Prog, Program0)
	;   Program0 = [], SideEff = Others
	),
	extract_variant(Program0, Program, Opts),
	extract_variant(Result0, Result, Opts).

get_program(ProgName, Program) :-
	program(ProgName, Program), !.
get_program(Program, Program).

% ----------------------------------------------------------------------
% Loading programs

load_program(Name) :-
	program(Name, Program),
	load_program(Program, []).

load_program([], _Opts) :-
	!.
load_program(Prog, Opts) :-
	process_quantifiers(Prog, Clauses, [], _ExV, clauses, [program|Opts]),
	Name = 'tmpfile.pl',
	open(Name, write, S), set_output(S),
	write_clauses(Clauses),
	close(S),
	test_module_qualified(Name, MName),
	load_program_file(MName).

write_clauses(Clauses) :-
	member(Clause, Clauses),
	handle_op(Clause),
	writeq(Clause), write(.), nl,
	fail.
write_clauses(_).

handle_op((:-op(_,Spec,Op))) :-
	!, assertz(user_op(Spec,Op)).
handle_op(_).


% ---------------------------------------------------------------------------
% Running the test

do_test(Goal, Vars, Result, SideEff, Opts) :-
	%set_prolog_flag(gc, true),
	%garbage_collect, % !! pts, SWI-Prolog
	%set_prolog_flag(gc, false),
	AllSolsGoal = findall(Vars, Goal, Sols0),
	%set_prolog_flag(gc, true),
	%garbage_collect, % !! pts, SWI-Prolog
	%set_prolog_flag(gc, false),
	check_timeout(Result, Timed),
	ensure_preconditions(SideEff, Opts),
	expect_exception(Timed, AllSolsGoal, ExcRes, Opts),
	% option(test_name(Name), Opts),
	treat_failure(Sols0, Sols),
	check_result(Result, ExcRes, Vars, Sols, Opts),
	% Dat: too early to check for test_ok or test_failed
	check_side_effects1(SideEff, Opts),
	check_postconditions(SideEff, Opts),
	% Dat: too early to check for test_ok or test_failed
	close_open_streams,
	check_side_effects2(SideEff, Opts),
	clean_files_ops(SideEff, Opts).

check_timeout(sto(_), timed) :-
	sto_behaviour(plain), !.
check_timeout(time_out, timed) :- !.
check_timeout(_, untimed).

treat_failure([], failure) :- !.
treat_failure(Sols, Sols).

% ---------------------------------------------------------------------------
% Expanding the side-effect specifications

expand_sideffs([SE|SEs], Es, Opts) :-
	expand_sideff(SE, SEs1, SEs, Opts), !,
	expand_sideffs(SEs1, Es, Opts).
expand_sideffs([SE|SEs], [SE|Es], Opts) :-
	valid_sideff(SE), !,
	expand_sideffs(SEs, Es, Opts).
expand_sideffs([SE|SEs], Es, Opts) :- !,
	my_print_message(warning,
		      my_format('Unknown side effect spec: ~w, ignoring.', [SE])),
	expand_sideffs(SEs, Es, Opts).
expand_sideffs([], Es, _Opts) :- !, Es = [].
expand_sideffs(SE, Es, Opts) :-
	expand_sideff(SE, Es, [], Opts).

valid_sideff(makein(_Type, _File, _Before)).
valid_sideff(openin(_Type, _File, _Alias)).
valid_sideff(checkin(_Type, _After, _Alias)).
valid_sideff(openout(_Type, _File, _Alias, _Before)).
valid_sideff(checkout(_Type, _File, _After)).
valid_sideff(clean(_File)).
valid_sideff(cleanop(_Spec, _Op)).
valid_sideff(dynp(_Prog)).
valid_sideff(prog(_Prog)).
valid_sideff(pre(_Goal)).
valid_sideff(post(_Goal)).

expand_sideff(variant(Id,List), Es, Es0, Opts) :- !,
	extract_variant(Id, List, Variant, Opts),
	expand_sideffs(Variant, Es1, Opts),
	append(Es1, Es0, Es).
expand_sideff(SE, Es, Es0, _Opts) :-
	expand_sideff(SE, Es, Es0).

expand_sideff(txtin(A)) --> [in(text, [], A, '')].
expand_sideff(txtin(B, A)) --> [in(text, [], B, A)].
expand_sideff(txtin(Al, B, A)) --> [in(text, [alias(Al)], B, A)].
expand_sideff(binin(A)) --> [in(binary, [], A, [])].
expand_sideff(binin(B, A)) --> [in(binary, [], B, A)].
expand_sideff(binin(Al, B, A)) --> [in(binary, [alias(Al)], B, A)].
expand_sideff(txtout(A)) --> [out(text, [], '', A)].
expand_sideff(txtout(B, A)) --> [out(text, [], B, A)].
expand_sideff(txtout(Al, B, A)) --> [out(text, [alias(Al)], B, A)].
expand_sideff(binout(A)) --> [out(binary, [], [], A)].
expand_sideff(binout(B, A)) --> [out(binary, [], B, A)].
expand_sideff(binout(Al, B, A)) --> [out(binary, [alias(Al)], B, A)].
expand_sideff(maketxt(File, Contents)) -->
	[makein(text, File, Contents)],
	[clean(File)].
expand_sideff(checktxt(File, Contents)) -->
	[checkout(text, File, Contents)],
	[clean(File)].
expand_sideff(makebin(File, Contents)) -->
	[makein(binary, File, Contents)],
	[clean(File)].
expand_sideff(checkbin(File, Contents)) -->
	[checkout(binary, File, Contents)],
	[clean(File)].
expand_sideff(in(Type, Alias, Before, After)) -->
	{File = 'tmp.in'},
	[makein(Type, File, Before)],
	[openin(Type, File, Alias)],
	[checkin(Type, After, Alias)],
	[clean(File)].
expand_sideff(out(Type, Alias, Before, After)) -->
	{File = 'tmp.out'},
	[openout(Type, File, Alias, Before)],
	[checkout(Type, File, After)],
	[clean(File)].
expand_sideff(Spec, SE0, SE) :-
	macro(Spec, Body),
	(   append(Body, SE, SE0) -> true
	;   append([Body], SE, SE0)
	).

% ---------------------------------------------------------------------------
% Preconditions

ensure_preconditions([SE|SEs], Opts) :-
	ensure_precondition(SE, Opts),
	ensure_preconditions(SEs, Opts).
ensure_preconditions([], _).

ensure_precondition(makein(Type, File, Contents), _) :- !,
	open(File, write, S, [type(Type)]),
	write_contents(Type, Contents, S),
	close(S).
ensure_precondition(openin(Type, File, Alias), _) :- !,
	open(File, read, S, [type(Type),eof_action(error)|Alias]),
	(   Alias = [] -> set_input(S)
	;   true
	).
ensure_precondition(openout(Type, File, Alias, Before), _) :- !,
	open(File, write, S, [type(Type)|Alias]),
	(   Alias = [] -> set_output(S)
	;   true
	),
	write_contents(Type, Before, S).
ensure_precondition(pre(Goal), _) :-
	!, test_module_qualified(Goal, MGoal),
	MGoal.
ensure_precondition(_, _).
	
write_contents(text, Atoms, S) :-
	(   atom(Atoms) -> my_format(S, '~a', [Atoms])
	;   write_list(Atoms, S)
	).
write_contents(binary, Bytes, S) :-
	put_byte_list(Bytes, S).

write_list([], _) :- !.
write_list([Head|Tail], S) :- !,
	write_list(Head, S), write_list(Tail, S).
write_list(Atom, S) :- 
	atom(Atom), !,
	my_format(S, '~a', [Atom]).
write_list(Code, S) :-
	put_code(S, Code).

put_byte_list([], _).
put_byte_list([B|Bs], S) :-
	put_byte(S, B),
	put_byte_list(Bs, S).

% ---------------------------------------------------------------------------
% Checking side effects round 1 (before closing all open streams)

check_side_effects1([SE|SEs], Opts) :-
	check_side_effect1(SE, Opts),
	check_side_effects1(SEs, Opts).
check_side_effects1([], _).

check_side_effect1(checkin(Type, ExpAfter, Alias), Opts) :-
	!, get_input_stream(Alias, S),
	get_rest_input(Type, S, GotAfter, Opts),
	comparison_pred(Type, ExpAfter, GotAfter, Comparison, ExpAfter1),
	expect(rest_input, ExpAfter1, GotAfter, Opts, Comparison).
check_side_effect1(dynp(Prog), Opts) :-
	!, check_dyn_preds(Prog, Opts).
check_side_effect1(_, _Opts).

get_input_stream([], S) :-
	current_input(S).
get_input_stream([alias(A)], S) :-
	stream_property(S, alias(A)).

% !! temp fix for gprolog: change {past} to ''
get_rest_input(_Type, S, {past}, _) :-
	stream_property(S, end_of_stream(past)), !.
%get_rest_input(_Type, S, '', _) :-
%	stream_property(S, end_of_stream(past)), !.

get_rest_input(binary, S, Bytes, _) :-
	get_bytes(S, Bytes).
get_rest_input(text, S, Atom, Opts) :-
	option(max_output_chars(Max), Opts, Max=1000),
				% to filter out infinite output, e.g. in repeat:1
	get_codes(S, Codes, Max),
	atom_codes(Atom, Codes).

get_bytes(S, []) :-
	at_end_of_stream(S), !, close(S).
get_bytes(S, [C|Cs]) :-
	get_byte(S, C), get_bytes(S, Cs).

get_codes(S, [], _) :-
	at_end_of_stream(S), !, close(S).
get_codes(_S, [], Max) :-
	Max =< 0, !.
get_codes(S, [C|Cs], Max) :-
	Max1 is Max-1,
	get_code(S, C),
	get_codes(S, Cs, Max1).

check_dyn_preds(ProgName, Opts) :-
	get_program(ProgName, Prog),
	process_quantifiers(Prog, Clauses, [], ExV, clauses, Opts),
	setof(Func, member((:- dynamic(Func)), Clauses), ExpdPreds),
	current_dyn_preds(GotPreds),
	expect(dynamic_predicates, ExpdPreds, GotPreds, Opts),
	(   member(Func, ExpdPreds), member(Func, GotPreds), Func = Name/Arity,
	    functor(Head, Name, Arity),
	    test_module_qualified(Head, MHead),
	    findall(Cl, (clause(MHead, Body),
			    mk_clause(Head, Body, Cl)), GotCls),
	    clauses_of_predicate(Clauses, Name, Arity, ExV, [], NExV, ExpCls),
	    expect(predicate(Func)-clauses, ExpCls, GotCls, Opts,
		      sub_variant(ExpCls, GotCls, NExV)),
	    fail
	;   true
	).

current_dyn_preds(DynPreds) :-
	initial_predicates(InitPreds),
	setof(Func, new_dyn_pred(InitPreds, Func), DynPreds), !.
current_dyn_preds([]).

current_preds(Preds) :-
	initial_predicates(InitPreds),
	findall(PI, current_test_predicate(PI), CurrPreds),
	list_diff(InitPreds, CurrPreds, Preds, _).

new_pred(InitPreds, Func) :-
	current_test_predicate(Func),
	\+ member(Func, InitPreds).
	
new_dyn_pred(InitPreds, Func) :-
	new_pred(InitPreds, Func),
	test_module_qualified(Func, MFunc),
	is_dynamic(MFunc).
	
mk_clause(Head, Body, Head):- Body == true, !.
mk_clause(Head, Body, (Head :- Body)).

clause_head((Head :- _Body), Head) :- !.
clause_head(Head, Head).

clauses_of_predicate([], _Name, _Arity, _ExV, NExV, NExV, []).
clauses_of_predicate([Cl|Cls], Name, Arity, ExV, NExV0, NExV, [NCl|RCls]) :-
	clause_head(Cl, Head),
	functor(Head, Name, Arity), !,
	copy_term(Cl+ExV, NCl+NExV1),  % should take the intersection of term_variables(Cl) and ExV... 
	append(NExV1, NExV0, NExV2),	
	clauses_of_predicate(Cls, Name, Arity, ExV, NExV2, NExV, RCls).
clauses_of_predicate([_Cl|Cls], Name, Arity, ExV, NExV0, NExV, RCls) :-
	clauses_of_predicate(Cls, Name, Arity, ExV, NExV0, NExV, RCls).

% ----------------------------------------------------------------------
% Checking side effects round 2 (after closing all open streams)

check_side_effects2([SE|SEs], Opts) :-
	check_side_effect2(SE, Opts),
	check_side_effects2(SEs, Opts).
check_side_effects2([], _).

check_side_effect2(checkout(Type, File, ExpContents), Opts) :- !,
	open(File, read, S, [type(Type)]),
	get_rest_input(Type, S, GotContents, Opts),
	comparison_pred(Type, ExpContents, GotContents, Comparison, ExpContents1),
	expect(file(File)-containing, ExpContents1, GotContents, Opts, Comparison).
check_side_effect2(_, _Opts).

comparison_pred(text, rx(ExpContents0), GotContents,
		regexp_match(ExpContents, GotContents), ExpContents) :- !,
	opt_concat_list(ExpContents0, ExpContents).
comparison_pred(text, ExpContents0, GotContents,
		ExpContents = GotContents, ExpContents) :- 
	opt_concat_list(ExpContents0, ExpContents), !.
comparison_pred(_, ExpContents, GotContents,
		ExpContents = GotContents, ExpContents).

opt_concat_list(Atom, Atom) :-
	atom(Atom), !.
opt_concat_list(List, Atom) :-
	List = [_|_],
	list_chars(List, Codes, []),
	atom_codes(Atom, Codes).

	
% ---------------------------------------------------------------------------
% Postconditions

check_postconditions([SE|SEs], Opts) :-
	(   SE = post(Goal) ->
	    check_postcondition(Goal, Opts)
	;   true
	),
	check_postconditions(SEs, Opts).
check_postconditions([], _).

check_postcondition(Goal, _Opts) :-
	call(Goal), !.
check_postcondition(Goal, Opts) :-
	expected(postcondition, Goal, failure, Opts).

list_to_goal([], true).
list_to_goal([Goal], Goal) :- !.
list_to_goal([Goal|GoalList], (Goal,Goals)) :-
	list_to_goal(GoalList, Goals).

% ----------------------------------------------------------------------
% Cleaning-up the files left over

clean_files_ops(SideEff, _Opts) :-
	member(clean(File), SideEff),
	clean_file(File),
	fail.
clean_files_ops(SideEff, _Opts) :-
	member(cleanop(Spec,Op), SideEff),
	op(0, Spec, Op), fail.
clean_files_ops(_SideEff, _Opts).


% ----------------------------------------------------------------------
% check_result/5: Checking the result of the test run.

check_result(_Result, time_out, _Vars, _Sols, _Opts) :- !.
check_result(eval(int_overflow), ExcRes, Vars, Sols, Opts) :-
	integer_domain(unlimited), !, check_result(failure, ExcRes, Vars, Sols, Opts).
check_result((Result1;Result2), ExcRes, Vars, Sols, Opts) :-
	(   check_result(Result1, ExcRes, Vars, Sols, [fail|Opts]) -> true
	;   check_result(Result2, ExcRes, Vars, Sols, [fail|Opts]) -> true
	;   expected(oneof, (Result1;Result2), ExcRes+Sols, Opts)
	).
check_result(sto(Result), ExcRes, Vars, Sols, Opts) :-
	!, sto_behaviour(STO), 
	check_sto_result(STO, Result, ExcRes, Vars, Sols, Opts).
check_result(Result, ExcRes, _Vars, Sols, Opts) :-
	exception(Result, Exc, ExV, Opts), !,
	(   ExcRes = exception(E) ->
	    (   option(ignore_excs, Opts) -> true
	    ;   (   select_variant(non_callable_culprit, first, Opts),
		    first_non_callable_culprit(Exc, Exc1) -> true
		;   Exc1 = Exc
		),
		expect(exception, Exc1, E, Opts, sub_variant(E, Exc1, ExV))
	    )
	;   expected(exception, Exc, Sols, Opts)
	).
	
check_result(Result, ExcRes, Vars, Sols, Opts) :-
	(   ExcRes = exception(E) ->
	    expected(run, to_complete_without_error, E, Opts)
	;   check_result(Result, Vars, Sols, Opts)
	).

% ---------------------------------------------------------------------------
% Converting exception abbreviations to exception terms

bip_exception(inst, instantiation_error, [], []).
bip_exception(type(ValidType,Culprit0), type_error(ValidType,Culprit),
	      Culprit0, Culprit).
bip_exception(dom(ValidDomain,Culprit0),
	      domain_error(ValidDomain,Culprit),
	      Culprit0, Culprit).
bip_exception(exist(ObjectType,Culprit0),
	      existence_error(ObjectType,Culprit),
	      Culprit0, Culprit).
bip_exception(perm(Operation,PermissionType,Culprit0),
	      permission_error(Operation,PermissionType,Culprit),
	      Culprit0, Culprit).
bip_exception(repr(Flag), representation_error(Flag), [], []).
bip_exception(eval(Error), evaluation_error(Error), [], []).
bip_exception(resource(Error), resource_error(Error), [], []).
bip_exception(syntax, syntax_error(Error),
	      Error, Error).
bip_exception(system, system_error, [], []).

exception(SysExcAbbrev, error(SysExc, SysDep), [SysDep|EVs], Opts) :-
	bip_exception(SysExcAbbrev, SysExc, Culprit0, Culprit), !,
	get_quantifiers(Culprit0, Culprit, Qs),
	(   Qs = [] -> term_variables(Culprit, EVs)
	;   process_quantifiers(Qs, [], EVs, exception, Opts)
	).
exception(user_error(Exc), ExcTerm, ExV, Opts) :-
	process_quantifiers(Exc, ExcTerm, [], ExV, exception, Opts).

% ---------------------------------------------------------------------------
% check_result/4: Checking the result of the test run not causing any exceptions

check_result(Result, Vars, Sols, Opts) :-
	undef(Result), !, 
	debug_undef(Result, Vars, Sols, Opts).
check_result(succfail, Vars, Sols, Opts) :-
	!,
	(   variant([Vars], Sols) -> true
	;   Sols = failure -> true
	;   expected(run, succeed_or_fail, sols:Sols, Opts)
	).
check_result(success, Vars, Sols, Opts) :-
	!, expect(single_success, sols:[Vars],
		  sols:Sols, Opts).
check_result(failure, _Vars, Sols, Opts) :-
	!, expect(run, to_fail, sols:Sols, Opts, Sols = failure).
check_result(Result, _Vars, failure, Opts) :-
	!, expected(solutions, Result, failure, Opts).
check_result(Result, Vars, Sols, Opts) :-
	Result = [_|_],
	check_subs(Result, 1, Sols, Vars, Opts).
check_result({Result}, Vars, Sols, Opts) :-
	commas_to_list(Result, Set, []),
	check_subs_set(Set, 1, Sols, Vars, Opts).

undef(undefined).
undef(impl_defined).
undef(impl_dependent).

debug_undef(_Result, _Vars, _Sols, _Opts).

% ---------------------------------------------------------------------------
% Checking sets of substitutions

commas_to_list((A, B), L0, L) :-
	!, commas_to_list(A, L0, L1),
	commas_to_list(B, L1, L).
commas_to_list(A, [A|L], L).

check_subs_set([], _, Sols, _, Opts) :-
	expect(no_more_solutions, [], Sols, Opts).
check_subs_set([...], _, _, _, _) :- !.
check_subs_set([Res|Results], N, Sols0, Vars, Opts) :-
	(   Sols0 = [_|_] ->
	    (	select(Sol, Sols0, Sols), 
		\+ \+ check_sub(Res, N, Sol, Vars, [fail|Opts]) ->
		true
	    ;	expected(substitution(N), Res, not_found_in_solution_set(Sols0), Opts),
		Sols = Sols0
	    ),
	    N1 is N+1,
	    check_subs_set(Results, N1, Sols, Vars, Opts)
	;   expected(more_solutions, [Res|Results], Sols0, Opts)
	).

% ---------------------------------------------------------------------------
% Checking lists of substitutions

check_subs([], _, Sols, _, Opts) :-
	expect(no_more_solutions, [], Sols, Opts).
check_subs([Res|Results], N, Sols0, Vars, Opts) :-
	(   Sols0 = [Sol|Sols] ->
	    \+ \+ check_sub(Res, N, Sol, Vars, Opts), N1 is N+1,
	    check_subs(Results, N1, Sols, Vars, Opts)
	;   expected(more_solutions, [Res|Results], Sols0, Opts)
	).

check_sub(Res, SN, Sol, Vars0, Opts0) :-
	process_quantifiers(Res, Sub, [], EVs0, SN, Opts0),
	reorder_sub(Sub, FullRes, Sol, CorrSol, Vars0, Vars, PostConds),
	Opts = [vars(Vars)|Opts0],
	term_variables(PostConds, EVs1),
	append(EVs0, EVs1, EVs),
	list_to_goal(PostConds, PostGoal),
	(   sub_variant(FullRes, CorrSol, EVs) ->
	    Vars = CorrSol,
	    option(sideeff(SideEff), Opts),
	    check_postcondition(PostGoal, Opts),
	    check_postconditions(SideEff, Opts)
	;   expected(sol(SN)-variant_except(EVs), FullRes, CorrSol, Opts)
	).

reorder_sub([], Vars, Sol, Sol, Vars, Vars, []).
reorder_sub([V<--Tpl|As], [Tpl|Tpls], Sol, [Sub|Subs], Vars, [V|Vs], PostConds) :-
	!, select_var(V, Vars, Vars1, Sub, Sol, Sol1),
	reorder_sub(As, Tpls, Sol1, Subs, Vars1, Vs, PostConds).
reorder_sub([PCond|As], Tpls, Sol, Subs, Vars, Vs, [PCond|PostConds]) :-
	reorder_sub(As, Tpls, Sol, Subs, Vars, Vs, PostConds).


select_var(X, [V|Vs], Vs, Sub, [Sub|Sol], Sol) :-
	X == V, !.
select_var(X, [V|Vs0], [V|Vs], Sub, [Sub0|Sol0], [Sub0|Sol]) :-
	select_var(X, Vs0, Vs, Sub, Sol0, Sol).

% ---------------------------------------------------------------------------
% Quantifier processing

process_quantifiers(Term0, Term, EVs0, EVs, SN, Opts) :-
	get_quantifiers(Term0, Term, Qs),
	process_quantifiers(Qs, EVs0, EVs, SN, Opts).

get_quantifiers(Term0, Term, Qs) :-
	get_implicit_quantifiers(Term0, Term1, [], Qs0),
	get_explicit_quantifiers(Term1, Term, Qs0, Qs).

process_quantifiers([Q|Qs], EVs0, EVs, SN, Opts) :-
	(   process_quantifier(Q, EVs0, EVs1, Opts) -> true
	;   expected(sol(SN),quantifier,Q, Opts),
	    EVs1 = EVs0
	),
	process_quantifiers(Qs, EVs1, EVs, SN, Opts).
process_quantifiers([], EVs, EVs, _SN, _Opts).

get_explicit_quantifiers(Term, Term, Qs, Qs) :- var(Term), !.
get_explicit_quantifiers(Q^Term0, Term, Qs0, [Q|Qs]) :-
	!, get_explicit_quantifiers(Term0, Term, Qs0, Qs).
get_explicit_quantifiers(Term, Term, Qs, Qs).

get_implicit_quantifiers(Term, Term, Qs, Qs) :- var(Term), !.
get_implicit_quantifiers(@@, V, Qs, [V|Qs]):- !.
get_implicit_quantifiers(@B, A, Qs, [A=mod(B)|Qs]):- !.
get_implicit_quantifiers(X@T, A, Qs, [A=X^T|Qs]) :- !.
get_implicit_quantifiers(Term0, Term, Qs0, Qs) :-
	compound(Term0), !, Term0 =.. [F|Args0],
	get_implicit_quantifiers_list(Args0, Args, Qs0, Qs),
	Term =.. [F|Args].
get_implicit_quantifiers(Term, Term, Qs, Qs).

get_implicit_quantifiers_list([Arg0|Args0], [Arg|Args], Qs0, Qs) :-
	get_implicit_quantifiers(Arg0, Arg, Qs0, Qs1),
	get_implicit_quantifiers_list(Args0, Args, Qs1, Qs).
get_implicit_quantifiers_list([], [], Qs, Qs).

process_quantifier(V, EVs0, [V|EVs0], _) :-
	var(V), !.
process_quantifier(A=ModB, EVs0, EVs, Opts) :-
	nonvar(ModB), ModB = mod(B), !,
	process_module_quantifier(B, A, EVs0, EVs, Opts).
process_quantifier(A=XT, EVs0, EVs, _) :-
	nonvar(XT), XT = X^T, !,
	sto_behaviour(STO),
	process_cyclic_term(STO, X, T, A, EVs0, EVs).
process_quantifier(A=B, EVs, EVs, _) :-
	A = B.

process_module_quantifier(B, B, EVs, EVs, Opts) :-
	option(program, Opts), !.
process_module_quantifier(B, A, EVs, [M|EVs], _) :-
	module_qualified(B, M, A0), !, A = A0. % Dat: module_qualified/3 is defined in main_*.pl
process_module_quantifier(B, B, EVs, EVs, _).

process_cyclic_term(cyclic, X, X, X, EVs, EVs).
process_cyclic_term(plain, X, B, B, EVs, [X|EVs]).

% ---------------------------------------------------------------------------
% Variants

sub_variant(As, B, EVs) :-
	sub_variant(As, B, EVs, _).

sub_variant(As, B, EVs, A) :-
	choice_of(As, A),
	vars_except(A, EVs, AVs),
	vars_except(B, EVs, BVs),
	(   numbervars(AVs, 0, _), \+ A = B -> fail
	;   numbervars(BVs, 0, _), \+ A = B -> fail
	;   true
	).

choice_of([A|As], [C|Cs]) :- !,
	choice_of_elem(A, C),
	choice_of(As, Cs).
choice_of(A, C) :-
	choice_of_elem(A, C).

choice_of_elem(A, A) :-
	var(A), !.
choice_of_elem((A;B), C) :- !,
	(   choice_of_elem(A, C)
	;   choice_of_elem(B, C)
	).
choice_of_elem(A, A).


vars_except(T, EVs, Vs) :-	    
	term_variables(T, TVs),
	set_remove(EVs, TVs, Vs).

set_remove([], S, S).
set_remove([E|L], S0, S) :-
	my_delete(S0, E, S1),
	set_remove(L, S1, S).

my_delete([], _, []).
my_delete([Head|Tail], Element, Rest) :-
	Head==Element, !,
	my_delete(Tail, Element, Rest).
my_delete([Head|Tail], Element, [Head|Rest]) :-
	my_delete(Tail, Element, Rest).
	    
% ---------------------------------------------------------------------------
% STO

check_sto_result(plain, Result, ExcRes, Vars, Sols, Opts) :-
	(   ExcRes = exception(timeout) -> true
	;   check_result(Result, ExcRes, Vars, Sols, Opts)
	).
check_sto_result(cyclic, Result, ExcRes, Vars, Sols, Opts) :-
	check_result(Result, ExcRes, Vars, Sols, Opts).
check_sto_result(occurs_check, Result, ExcRes, Vars, Sols, Opts) :-
	opposite(Result, NewResult),
	check_result(NewResult, ExcRes, Vars, Sols, Opts).

opposite(failure, success) :- !. 		%  a bit weak :-(
opposite(_, failure).

% ---------------------------------------------------------------------------
% Utiltities

list_chars([], Chars, Chars) :- !.
list_chars([Head|Rest], Chars0, Chars) :-
	list_chars(Head, Chars0, Chars1),
	list_chars(Rest, Chars1, Chars).
list_chars(Atom, Chars0, Chars) :- 
	atom(Atom),
	atom_codes(Atom, Chs),
	append(Chs, Chars, Chars0).
list_chars(Code, [Code|Chars], Chars) :-
	integer(Code).

% vvv Dat: imported from library(lists) in SICStus 4, but not neede.
%prefix_length(N, [X|L], [X|T]) :-
%	N > 0, !, N1 is N-1, prefix_length(N1, L, T).
%prefix_length(_, _, []).


% ---------------------------------------------------------------------------
% Customising the display of test status (`ok' or `not ok')
portray_ok(Opts) :-
	%%%% pts %%%% !! only if verbose
	option(test_name(GroupNum), Opts),
	write(user_output, 'ok      '), write(user_output, GroupNum), nl(user_output).

portray_not_ok(Opts) :-
	option(test_name(GroupNum), Opts),
	write(user_output, 'not ok  '), write(user_output, GroupNum), nl(user_output).

% ---------------------------------------------------------------------------
% Customising the display of un-"expect"-ed results (maybe >1 for a single
% test case), called from testbed

portray_expect(What, Expected, Got, Opts) :-
	muzzle_expect(What, Expected, Got, Opts), !.
portray_expect(What, Expected, Got, Opts) :-
	current_output(CO), set_output(user_output),
	option(test_name(Group:Num), Opts),
	option(goal(Goal), Opts),
	option(vars(Vars), Opts),
	option(result(Result), Opts),
	get_test(Group:Num, _, _Goal, Program, _Result, _SideEff, Opts),
	( portray_nonconformance(Group, Num, Vars, Goal, What, Result, Expected, Got), %%%% pts %%%%
	  fail % Dat: we must fail here, because portray_nonconformance uses numbervars/3 to bind some vars
	; true
	),
	(   portray_program(Program, Opts) -> true %%%% pts %%%%
	;   my_format('---portray_program failed~n', [])
	),
	my_format('----------------------------------------~n', []),
	set_output(CO).

%%%% pts %%%% Dat: moved to separate predicate
portray_nonconformance(Group, Num, Vars, Goal, What, Result, Expected, Got) :-
	% my_format('pts: *****1~n', []), %%%%% pts %%%% Dat: for debugging
	numbervars(Goal+What+Result+Expected+Got, 0, _),
	% my_format('pts: *****2~n', []),
	my_format('~N******* ISO non-conformance in group `~w\', test ~w ********~n',
		   [Group, Num]),
	my_format('        Goal: ~q, Vars: ~q~n        Prescribed result: ~q~n', [Goal, Vars, Result]),
	print_options(PrOpt0),
	PrOpt = [numbervars(true),quoted(true)|PrOpt0],
	my_format('        Expected ~q: `~@'', got `~@''.~n',
		                 [What, write_term(Expected, PrOpt),
				 write_term(Got, PrOpt)]),
	!.
portray_nonconformance(Group, Num, Vars, Goal, What, Result, Expected, Got) :-
	my_format('---format failed~n', []), % Dat: sometimes in aprolog
	%%%% pts %%%%
	% Imp: also print Program
	%( my_format('t1(~q)', [foo(bar,baz)])
	( writeq(case=Group:Num) -> true ; true ), nl,
	( writeq(vars=Vars) -> true ; true ), nl,
	( writeq(goal=Goal) -> true ; true ), nl,
	( writeq(what=What) -> true ; true ), nl,
	( writeq(result=Result) -> true ; true ), nl,
	( writeq(expected=Expected) -> true ; true ), nl,
	( writeq(got=Got) -> true ; true ), nl.

% !! why do we have this?!
muzzle_expect(_, _, _, _) :- fail.

% ---------------------------------------------------------------------------
% Portraying programs

:- dynamic(program_shown/0).

reset_portray_expect :-
	retractall(program_shown).

portray_program(_, _) :-
	program_shown, !, my_format('Program: see above~n', []).
portray_program(Program, Opts) :-
	(   Program = [] -> true
	;   my_format('Program: ~n', []), show_program(Program, Opts),
	    assertz(program_shown)
	).
	    
show_program(Prog, Opts) :-
	process_quantifiers(Prog, Clauses, [], _ExV, clauses, Opts),
	member(Clause, Clauses),
	show_clause(Clause), fail.
show_program(_, _).


% ---------------------------------------------------------------------------
% Debugging utils

:- op(1199, fx, (ex)).
:- op(1199, xfx, (ex)).

ex(Goal) :-
	bb_get(sno, N),
	N1 is N+1,
	ex(N1, Goal).

ex(N, Goal) :-
	expect_exception(Goal, Result),
	bb_put(sno, N),
	(   Result = exception(error(Iso, SP)) ->
	    test_line(N, Goal, Iso),
	    my_format('~30| SP: ~q~n', [SP])
	;   writeq(Result), nl
	), fail.
ex(_, _).

test_line(N, Goal, Iso) :-
	numbervars(Goal, 22, _),
	last_subgoal(Goal, SubGoal),
	functor(SubGoal, Name, _),
	my_format('atc(~a:', [Name]),
	my_format('~|~t~d~2+, (~q),\t\t~q,\t\t[]).~n', [N,Goal,Iso]).

last_subgoal((_, Goal), SubGoal) :-
	!, last_subgoal(Goal, SubGoal).
last_subgoal((Goal; _), SubGoal) :-
	!, last_subgoal(Goal, SubGoal).
last_subgoal(Goal, Goal).


% ---------------------------------------------------------------------------
% self-check

check :-
	isosec(Sec, Preds),
	findall(N, (test_case(P:N,_,_,_,_), member(P, Preds)), L),
	(   \+nums(L) -> true
	;   \+L = [1|_]
	),
	write(Sec-Preds-L), nl,
	fail.
check.

nums([_]).
nums([A,B|L]):-
	B =:= A+1, nums([B|L]).

% ---------------------------------------------------------------------------
% Floating point precision: testing for approximate equality:

=~~(F1, F2) :-
	abs(F1-F2) < 1.0e-4.

% ---------------------------------------------------------------------------
test_module_qualified(Term, MTerm) :-
	module_qualified(Term, iso_test, MTerm), !.
test_module_qualified(Term, Term).

current_test_predicate(Func) :-
	test_module_qualified(Func, MFunc),
	% write(user_error,mf(Func,MFunc)), nl(user_error),
	% ( fail, nonvar(MFunc), MFunc=A:B -> A:current_predicate(B) % !! yap (remove fail to make it work)
	( nonvar(MFunc), MFunc=A:B -> A:current_predicate(B) % !! yap (remove fail to make it work)
	; current_predicate(MFunc)
	).
	%write(user_error,ok).

compl2 :-
	negative_integer_representation(compl2).

first_non_callable_culprit(error(type_error(callable,Goal0),Err),
			   error(type_error(callable,Goal1),Err)) :-
	subgoal_of(Goal0, Goal1), \+ callable(Goal1), !.

subgoal_of(G, G).
subgoal_of(once(G), SG) :-  % ????
	subgoal_of(G, SG).
subgoal_of((G1,G2), SG) :-
	(   subgoal_of(G1, SG)
	;   subgoal_of(G2, SG)
	).
subgoal_of((G1;G2), SG) :-
	(   subgoal_of(G1, SG)
	;   subgoal_of(G2, SG)
	).
subgoal_of((G1 -> G2), SG) :-
	(   subgoal_of(G1, SG)
	;   subgoal_of(G2, SG)
	).