:- module(nftable,
	    [create_a_litinfo/3,
		find_entry/3,
		flag_is_false/1,
		flag_is_true/1,
		flag_is_unbound/1,
		get_lit_pred_name_arity/2, % Added 17-may-2003
		get_info_for_literal_from_predicate/5,
		get_lit_info_for_nonfail/5,
		get_literal/2,
		get_literal_flag/3,
		get_literal_key/2,
		get_literal_predicate_entry/4,
		get_litpred_and_literal/3,
		get_nf_info/6,
		get_nfentry_info/7,
		get_pred_clauses/3,
		get_test_flag_value/2,
		get_test_value/2,
		insert_field/4,
		insert_mode_type/3,
		is_a_builtin/3,
		literal_is_a_test/1,
		literals_before_the_first_cut/2,
		litinfo_is_a_builtin_test/1,
		pred_assertion/9,
		set_coverflag_false/1,
		set_coverflag_value/2,
		set_flag_value_false/1,
		set_flag_value_true/1,
		set_in_top_modetype/3,
		set_nfailflag_false/1,
		set_nfailflag_value/2,
		set_test_flag_value_defined/1,
		set_test_value/2
	    ],
	    [assertions,
		basicmodes
	    ]).

:- use_module(library(messages), [warning_message/2, warning_message/3]).

:- use_module(infernf(in_out),
	    [get_key_and_concrete_literal_from_external_literal/3]).
:- use_module(infernf(nfbool),    [remove_negation/2]).
:- use_module(typeslib(typeslib), [set_top_type/1]).
:- use_module(program(p_unit),    [type_of_goal/2]).

%-----------------------------------------------------------------------

:- doc(module, "This file contains the procedures for handling the
NF table.  The NF table is an open list of items of the form
st(Pred/Arity, Clauses, Mode_type, Ann_clauses, Test, Nfail_flag,
Cover_Flag).").

:- doc(module, "A clause has the following information:
           clause(Tests_before_the_first_cut,
                  Tests_after_the_first_cut,
                  Literals_before_the_first_cut, 
                  Literals_after_the_first_cut_and_before_the_last, 
                  Literals_after_the_last_cut,
                  % Flags
                  Has_a_cut).
            Cuts are removed.").


%% :- typedef nftable ::= var ; [nfentry|nftable].
%% 
%% :- typedef nfentry ::= ^st(Pred/Arity, Clauses, Mode_type, Ann_clauses, Test, Nfail_flag,
%% Cover_Flag)

:- pred insert_entry(ST, +Pred, Entry)

# "Insert an entry for predicate @var{Pred} in the table.  If it is
    already in, return the entry as @var{Entry} otherwise, an entry
    @var{Entry} for @var{Pred} is inserted and returned.".

insert_entry(ST, Pred, Entry) :-
	var(ST), !,
	Entry = st(Pred, _, _, _, _, _, _),
	ST = [Entry|_].
insert_entry(ST, Pred, Entry) :-
	nonvar(ST),
	ST = [Entry|_],
	Entry = st(Pred, _, _, _, _, _, _), !.
insert_entry(ST, Pred, Entry) :-
	nonvar(ST),
	ST = [E|S],
	E \== st(Pred, _, _, _, _, _, _),
	insert_entry(S, Pred, Entry).

% If the predicate have already a type declaration, ignore the current
% type declaration.

insert_mode_type(ST, Pred, Mode_Type) :-
	!,
	insert_entry(ST, Pred, st(Pred, _, Prev_Mode_Type, _, _, _, _)),
	( var(Prev_Mode_Type) ->
	    Prev_Mode_Type = Mode_Type
	;
	    warning_message(
		"Predicate ~q has the type ~q. The type ~q is ignored.", [Pred,
		    Prev_Mode_Type, Mode_Type]) ).

insert_field(ST, Pred, clause, Clause) :-
	!, % Insert a clause for predicate Pred into the  table.
	insert_entry(ST, Pred, st(Pred, ClauseList, _, _, _, _, _)),
	insert_clause(ClauseList, Clause).
insert_field(ST, Pred, mode_type, Mode_Type) :- !,
	insert_entry(ST, Pred, st(Pred, _, Mode_Type, _, _, _, _)).
insert_field(ST, Pred, ann_clauses, ClauseList) :- !,
	insert_entry(ST, Pred, st(Pred, _, _, ClauseList, _, _, _)).
insert_field(ST, Pred, test, Test) :- !,
	insert_entry(ST, Pred, st(Pred, _, _, _, Test, _, _)).
insert_field(ST, Pred, nfail, NFail) :- !,
	insert_entry(ST, Pred, st(Pred, _, _, _, _, NFail, _)).
insert_field(ST, Pred, covered, Covered) :-
	insert_entry(ST, Pred, st(Pred, _, _, _, _, _, Covered)).


% 
% Inserts the clause at the end of an open List. Duplicated clauses are allowed.
% But this is programmer's  responsability.
%

insert_clause(ClauseList, Clause) :-
	var(ClauseList), !,
	ClauseList = [Clause|_].
insert_clause(ClauseList, Clause) :-
	nonvar(ClauseList),
	ClauseList = [_|CList],
	insert_clause(CList, Clause).

%
%  Find the entry for predicate Pred in the  table.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%

find_entry(ST, _, _) :-
	var(ST), !.
find_entry(ST, Pred, Entry) :-
	nonvar(ST),
	ST = [Entry|_],
	Entry = st(Pred, _, _, _, _, _, _), !.
find_entry(ST, Pred, Entry) :-
	nonvar(ST),
	ST = [E|S],
	E = st(Pred1, _, _, _, _, _, _),
	Pred \== Pred1,
	find_entry(S, Pred, Entry).

/*
%
%  Find a field for predicate Pred in table.
%

find_field(ST, Pred, clause, ClauseList) :- !,
	find_entry(ST, Pred, st(Pred, ClauseList, _, _, _, _, _)).
find_field(ST, Pred, mode_type, Mode_Type) :- !,
	find_entry(ST, Pred, st(Pred, _, Mode_Type, _, _, _, _)).
find_field(ST, Pred, ann_clauses, ClauseList) :- !,
	find_entry(ST, Pred, st(Pred, _, _, ClauseList, _, _, _)).
find_field(ST, Pred, test, Test) :- !,
	find_entry(ST, Pred, st(Pred, _, _, _, Test, _, _)).
find_field(ST, Pred, nfail, NFail) :- !,
	find_entry(ST, Pred, st(Pred, _, _, _, _, NFail, _)).
find_field(ST, Pred, covered, Covered) :-
	find_entry(ST, Pred, st(Pred, _, _, _, _, _, Covered)).
*/
:- pred get_literal_predicate_entry(TAB, LitPred, PredEntry, LiteralPredEntry)

# "Get the entry in @var{TAB} of the literal predicate @var{LitPred}
   and return it in @var{LiteralPredEntry}.  @var{PredEntry} is the
   entry of the predicate in which body the literal is.  If
   @var{PredEntry} is the entry of @var{LitPred}, then returns it,
   otherwise find the literal predicate entry in @var{TAB}.".

get_literal_predicate_entry(TAB, F/A, PredEntry, LiteralPredEntry) :-
	( PredEntry = st(F/A, _, _, _, _, _, _)
	-> LiteralPredEntry = PredEntry
	; find_entry(TAB, F/A, LiteralPredEntry) ).

% Creates a mode-type ModType for predicate F/A where all arguments are input and of type top.
set_in_top_modetype(F, A, ModType) :-
	functor(ModType, F, A),
	set_in_top_modetype_0(A, ModType).

set_in_top_modetype_0(0, _ModType) :- !.
set_in_top_modetype_0(A, ModType) :-
	A > 0, !,
	set_top_type(Type),
	arg(A, ModType, (in:Type)),
	A1 is A - 1,
	set_in_top_modetype_0(A1, ModType).

%% OPERATIONS OVER LITERALS

%% literal is of the form:
%%   litinfo(Literal, TestFlag, Test, HeadPred, ModeType, Nfail_flag, Cover_flag).
%% where
%% Literal: as it appears in Lit_List.
%% HeadPred: predicate in which definition the the literal appears.
%% ModeType: calling type (not used at the moment). Its use is intended
%% when it is allowed to have different call types for different literals. 
%% Nfail_flag: non-fail flag (gets the value fail if the literal is
%% not guaranteed to not fail, and remains as a free variable otherwise).
%% Cover_flag: covered flag. Gets the value fail if the calling type ModeType 
%% of the literal is not covered by the test of its predicate,    
%% and gets the value true if it is covered.

:- pred get_litpred_and_literal(+Litinfo, -LitPred, -Literal)

# "Gets the predicate literal and the literal of @var{Litinfo}.".

get_litpred_and_literal(Litinfo, LitPred, Literal) :-
	Litinfo = litinfo(_Key, Lit, _TestFlag, _Test, _HeadPred, _ModeType,
	    _Nfail_flag, _Cover_flag),
	remove_negation(Lit, Literal),
	functor(Literal, F, A),
	LitPred = F/A.

:- pred get_lit_info_for_nonfail(+Litinfo, -LitPred, -Literal, -Nfail_flag,
	    -Cover_flag)

# "Gets information about one literal, which is needed for non-failure
and covering analysis.".

get_lit_info_for_nonfail(Litinfo, LitPred, Literal, Nfail_flag, Cover_flag) :-
	Litinfo = litinfo(_Key, Literal, _TestFlag, _Test, _HeadPred,
	    _ModeType, Nfail_flag, Cover_flag),
	functor(Literal, F, A),
	LitPred = F/A.

/*
get_lit_info(Litinfo, HeadPred, Nfail_flag) :-
	Litinfo = litinfo(_Key, _Literal, _TestFlag, _Test, HeadPred,
	    _ModeType, Nfail_flag, _Cover_flag).
*/
%

get_literal_key(Litinfo, Key) :-
	Litinfo = litinfo(Key, _Literal, _TestFlag, _Test,
	    _HeadPred, _ModeType, _Nfail_flag, _Cover_flag).

get_lit_pred_name_arity(Litinfo, LitPred) :-
	Litinfo = litinfo(_Key, Literal, _TestFlag, _Test,
	    _HeadPred, _ModeType, _Nfail_flag, _Cover_flag),
	functor(Literal, F, A),
	LitPred = F/A.

get_literal(Litinfo, Literal) :-
	Litinfo = litinfo(_Key, Literal, _TestFlag, _Test, _HeadPred,
	    _ModeType, _Nfail_flag, _Cover_flag).

get_test_flag_value(Litinfo, TestFlag) :-
	Litinfo = litinfo(_Key, _Literal, TestFlag, _Test, _HeadPred,
	    _ModeType, _Nfail_flag, _Cover_flag).

get_test_value(Litinfo, Test) :-
	Litinfo = litinfo(_Key, _Literal, _TestFlag, Test, _HeadPred,
	    _ModeType, _Nfail_flag, _Cover_flag).

get_literal_flag(Litinfo, Literal, Nfail_flag) :-
	Litinfo = litinfo(_Key, Literal, _TestFlag, _Test, _HeadPred,
	    _ModeType, Nfail_flag, _Cover_flag).

create_a_litinfo(ExterLiteral, HeadPred, Litinfo) :-
	get_key_and_concrete_literal_from_external_literal(ExterLiteral, Key,
	    ConcLiteral),
	translate_to_internal(ConcLiteral, CLiteral),
	Litinfo = litinfo(Key, CLiteral, _TestFlag, _Test, HeadPred,
	    _ModeType, _Nfail_flag, _Cover_flag).

translate_to_internal(Literal, InternalLiteral) :-
	type_of_goal(builtin(InternalLiteral), Literal),
	!.
translate_to_internal(Literal, InternalLiteral) :-
	type_of_goal(metapred(not(G), _Meta), Literal),
	G = $(Term, _Go, _Ty),
	!,
	translate_to_internal(Term, InternalGoal),
	functor(Literal,         F, 1),
	functor(InternalLiteral, F, 1),
	arg(1, InternalLiteral, InternalGoal).
translate_to_internal(Literal, Literal).

there_is_some_assertion(Literal, Vars, PPKey) :-
	functor(Literal, F, A),
	pred_assertion(F/A, Vars, PPKey, Literal, _CallType,
	    _TestFlag, _Test, _Nfail_flag, _Cover_flag).

is_a_builtin(Literal, Vars, PPKey) :-
	there_is_some_assertion(Literal, Vars, PPKey).

%% Operations on test flags.
/*
litinfo_is_a_test(Litinfo) :-
	get_test_flag_value(Litinfo, TestFlag),
	literal_is_a_test(TestFlag).
*/

litinfo_is_a_builtin_test(Litinfo) :-
	get_test_flag_value(Litinfo, TestFlag),
	( TestFlag == unification;
	    TestFlag == arithmetic;
	    TestFlag == meta ).

literal_is_a_test(TestFlag) :-
	TestFlag == unification;
	TestFlag == arithmetic;
	TestFlag == meta;
	TestFlag == definedtest.

set_test_flag_value_defined(Litinfo) :-
	Litinfo = litinfo(_Key, _Literal, TestFlag, _Test, _HeadPred,
	    _ModeType, _Nfail_flag, _Cover_flag),
	TestFlag = definedtest.

set_test_value(Litinfo, Test) :-
	Litinfo = litinfo(_Key, _Literal, _TestFlag, Test, _HeadPred,
	    _ModeType, _Nfail_flag, _Cover_flag).

set_nfailflag_false(Litinfo) :-
	Litinfo = litinfo(_Key, _Literal, _TestFlag, _Test, _HeadPred,
	    _ModeType, fail, _Cover_flag).

set_coverflag_false(Litinfo) :-
	Litinfo = litinfo(_Key, _Literal, _TestFlag, _Test, _HeadPred,
	    _ModeType, _Nfail_flag, fail).

set_nfailflag_value(Litinfo, Nfail_flag) :-
	Litinfo = litinfo(_Key, _Literal, _TestFlag, _Test, _HeadPred,
	    _ModeType, Nfail_flag, _Cover_flag).

set_coverflag_value(Litinfo, Cover_flag) :-
	Litinfo = litinfo(_Key, _Literal, _TestFlag, _Test, _HeadPred,
	    _ModeType, _Nfail_flag, Cover_flag).

%%

:- pred get_info_for_literal_from_predicate(+LitPredEntry, -Clauses, -ModeType,
	    -PredCovFlag, -PredNF_Flag)

# "Get the mode and type calling information from the entry
  corresponding to the predicate of a literal.".

get_info_for_literal_from_predicate(LitPredEntry, Clauses, ModeType,
	    PredCovFlag, PredNF_Flag) :-
	LitPredEntry = st(_Pred, Clauses, ModeType, _Ann_clauses, _Test,
	    PredCovFlag, PredNF_Flag).

/*

%% get the non-fail flag Pnfail_flag of a predicate and the list of literals 
%% PLiterals which have the same predicate (i.e. those literals such that 
%% there is an edge from the predicate whose entry is Entry to each literal). 

get_pred_graph_info(Entry, Pnfail_flag, PLiterals) :-
	Entry = nf(_Pred, Pnfail_flag, PLiterals).
*/

:- pred get_nf_info(+PredEntry, -Clauses, -Mode_type, -Test, -Nfail_flag,
	    -Covered_Flag)

# "Gets info of predicate Predicate from the entry @var{Entry}
corresponding to the predicate, namely: @var{Nfail_flag} is the
non-fail flag (gets the value fail if the predicate is guaranteed to
not fail, and remains as a free variable if the predicate does not
fail).  @var{Clauses} are the clause bodies of the predicate (a list
of lists).  @var{Test} is the test of the predicate.".

get_nf_info(PredEntry, Clauses, Mode_type, Test, Nfail_flag, Covered_flag) :-
	PredEntry = st(_Pred, Clauses, Mode_type, _Ann_clauses, Test,
	    Nfail_flag, Covered_flag).

get_nfentry_info(Entry, Pred, Clauses, Mode_type, Test, Nfail_flag,
	    Covered_Flag) :-
	Entry = st(Pred, Clauses, Mode_type, _Ann_clauses, Test, Nfail_flag,
	    Covered_Flag).

get_pred_clauses(Entry, Clauses, Ann_Clauses) :-
	Entry = st(_Pred, Clauses, _Mode_type, Ann_Clauses, _Test, _Nfail_flag,
	    _Covered_Flag).
/*
get_pred_info(Entry, Pred, Ann_Clauses, Nfail_flag) :-
	Entry = st(Pred, _Clauses, _Mode_type, Ann_Clauses, _Test, Nfail_flag,
	    _Covered_Flag).
*/

% Operations over non-fail and cover flags.

flag_is_unbound(Flag) :-
	var(Flag).
flag_is_true(Flag) :-
	nonvar(Flag),
	Flag = true.
flag_is_false(Flag) :-
	nonvar(Flag),
	Flag = fail.

set_flag_value_true(Flag) :-
	Flag = true.
set_flag_value_false(Flag) :-
	Flag = fail.

/*
clause_has_a_cut(Clause) :-
	get_body_of_clause(Clause, Body),
	body_has_a_cut(Body).

body_has_a_cut(Body) :-
	get_clause_cut_info(Body, _Literals_before_1st_cut,
	    _Literals_after_last_cut, Has_a_cut),
	flag_is_true(Has_a_cut).
*/

:- pred literals_before_the_first_cut(+BodyList, -Literals_Before_1st_Cut)

# "Collects the litinfos in a clause body before the first cut.".

literals_before_the_first_cut([],             []) :- !.
literals_before_the_first_cut([Litinfo|Rest], Literals_Before_1st_Cut) :-
	( is_a_cut(Litinfo)
	-> Literals_Before_1st_Cut = []
	;
	    Literals_Before_1st_Cut = [Litinfo|RestLits],
	    literals_before_the_first_cut(Rest, RestLits) ).

is_a_cut(Litinfo) :-
	get_literal(Litinfo, Literal),
	nonvar(Literal),
	functor(Literal, !, 0).

% If a cut is found then the flag Has_a_cut is set to true, otherwise it is set to false.
/*
get_clause_cut_info(Body, Literals_before_1st_cut, Literals_after_last_cut,
	    Has_a_cut) :-
	get_clause_cut_info_4(Body, Literals_before_1st_cut,
	    Literals_after_last_cut, Has_a_cut),
	( flag_is_unbound(Has_a_cut) ->
	    set_flag_value_false(Has_a_cut)
	;
	    set_flag_value_true(Has_a_cut) ).

% If a cut is found then the flag Has_a_cut is set to true, otherwise it is unbound.

get_clause_cut_info_4([], [], [], _Has_a_cut) :- !.
get_clause_cut_info_4([Litinfo|Rest], Literals_before_1st_cut,
	    Literals_after_last_cut, Has_a_cut) :-
	( is_a_cut(Litinfo)
	-> set_flag_value_true(Has_a_cut),
	    Literals_before_1st_cut = [],
	    get_literals_after_the_last_cut(Rest, [], Literals_after_last_cut)
	;
	    Literals_before_1st_cut = [Litinfo|RestLitsBefore],
	    get_clause_cut_info_4(Rest, RestLitsBefore,
		Literals_after_last_cut, Has_a_cut) ).

get_literals_after_the_last_cut([], In_Literals_after_last_cut,
	    Literals_after_last_cut) :-
	!,
	reverse(In_Literals_after_last_cut, Literals_after_last_cut).
get_literals_after_the_last_cut([Litinfo|Rest], In_Literals_after_last_cut,
	    Literals_after_last_cut) :-
	( is_a_cut(Litinfo)
	-> Tem_Literals_after_last_cut = []
	;
	    Tem_Literals_after_last_cut = [Litinfo|In_Literals_after_last_cut]
	),
	get_literals_after_the_last_cut(Rest, Tem_Literals_after_last_cut,
	    Literals_after_last_cut).
*/

:- doc(bug, "The existence  of this predicate, convert_lit/2, is a
	kludge  because  in some  predicates  we  need the  predicates
	[is/2, true/0] qualified by module, and in others not. We have
	to review why we are removing the module qualification of such
	specific predicates. -- EMM").

convert_lit(true/0, 'basiccontrol:true'/0) :- !.
convert_lit(is/2,   'arithmetic:is'/2) :- !.
convert_lit('='/2,  'term_basic:='/2) :- !.
convert_lit(Lit,    Lit).

literal_test('arithmetic:is'/2, is(X, Y), =:=(X, Y)) :- !.
literal_test(_,                 Literal,  Literal).

fail_by_default(fail) :- !.
fail_by_default(_).

:- pred pred_assertion(+LitPred, +Vars, +PPKey, ?Literal, ?CallType,
	    -TestFlag, -Test, -Nfail_flag, -Cover_flag)

# "Supplies information about builtins, library predicates, or
  predicates whose source code is not available. If there is some fact
  for a predicate, then it is assumed to be a builtin, otherwise, it
  is assumed to be user defined".

:- use_module(resources(init_res(builtin_res)), [find_point_trusted_field/10]).

pred_assertion(Lit, Vars, PPKey, Literal, _CallType, TestFlag, Test, NF,
	    Cover) :-
	convert_lit(Lit, Lit1),
	find_point_trusted_field(nf,        _BT, _ST, Literal, Vars, PPKey,
	    _, Lit1, Approx, NF),
	find_point_trusted_field(cover,     _BT, _ST, Literal, Vars, PPKey,
	    _, Lit1, Approx, Cover),
	find_point_trusted_field(test_type, _BT, _ST, Literal, Vars, PPKey,
	    _, Lit1, Approx, TestFlag),
	( ( ground([NF, Cover])
% This commented-out code is to help debugging -- EMM:
% 	    -> note(['For ', ~~(Lit), ' taking cover (', Cover,
% 			') and nfail (', NF, ') value from trust assertions'])
% 	    ; fail
	    )
	; ( ground(TestFlag) ->
		fail_by_default(NF),
		fail_by_default(Cover)
	    ; fail ) ),
	!,
	literal_test(Lit1, Literal, Test).

% This commented-out code is to help debugging -- EMM:
% 	(
% 	    pred_assertion_(Lit, Literal, _CallType, TestFlag, Test,
% 		_NF, _Cover) ->
% 	    ( NF \= _NF
% 	    -> warning(['For the literal ', ~~(Lit),
% 			' nfail value from table is ', _NF,
% 			' but from trusts is ', NF, '.']) ; true ),
% 	    ( _Cover \= Cover
% 	    -> warning(['For the literal ', ~~(Lit),
% 			' cover value from table is ', _Cover,
% 			' but from trusts is ', Cover, '.']) ; true )
% 	;
% 	    true
% 	).

% :- include(infernf(nf_deal_builtin)).
