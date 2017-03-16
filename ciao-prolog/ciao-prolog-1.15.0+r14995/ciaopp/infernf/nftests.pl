:- module(nftests,
	    [compute_a_clause_test_in_minset_format/3,
		compute_pred_test/3,
		compute_test_for_all_clauses/3,
		compute_test_for_selected_clauses/4,
% for plai
		create_unif_tests/4,
% for detplai
		test_is_true/1,
		test_is_false/1,
		set_test_false/1
	    ],
	    [assertions, isomodes]).
%	[assertions,regtypes]).

:- use_module(infernf(in_out),
	    [get_head_and_body_of_clause/3,
		there_are_no_more_clauses/1
	    ]).
:- use_module(infernf(nfsets), [create_minset_and_project/4]).
:- use_module(infernf(nftable),
	    [get_test_flag_value/2,
		get_test_value/2,
		get_lit_pred_name_arity/2
	    ]).
:- use_module(infernf(nfbool), [push_neg_in_test/2]).

:- use_module(library(messages), [debug_message/2]).
:- use_module(library(lists),    [append/3]).

%-----------------------------------------------------------------------

test_is_true(Test) :- Test == true.
test_is_false(Test) :- Test == false ; Test == fail.

% set_test_true(Test) :- Test = true.

set_test_false(Test) :- Test = false.

% Operations over tests:

compute_test_for_all_clauses(Clauses, ModeType, Test) :-
	compute_pred_test_0(Clauses, ModeType, [], Test).

%% This is not used in the determinacy analysis.
compute_test_for_selected_clauses(SelectedClauseNumbers,
	    SelectedClauses, ModeType, Test) :-
	( SelectedClauseNumbers == []
	-> Test = false
	; compute_pred_test_0(SelectedClauses, ModeType, [], Test) ).

%% Not used 22 oct 98
% get_clauses_from_numbers([], _Count, _Clauses, _SelectedClauses) :-
% 	!.
% get_clauses_from_numbers([ClauseNumber|SelectedClauseNumbers], Count,
% 	    [Clause|ResClauses], SelectedClauses) :-
% 	( ClauseNumber =:= Count
% 	-> SelectedClauses = [Clause|ResSelectedClauses]
% 	; SelectedClauses = ResSelectedClauses ),
% 	NewCount is Count + 1,
% 	get_clauses_from_numbers(SelectedClauseNumbers, NewCount, ResClauses,
% 	    SelectedClauses).

%% :- regtype minset_test(A) # "@var{A} is a clause test in minset format
%%    (used for non-failure analysis).".
%% 
%% minset_test(true).
%% minset_test(false).
%% minset_test(test(minset(T, C), O)):-
%%    tuple_of_terms(T),
%%    cobasic_set_list(C),
%%    othertests(O).
%% 
%%  cobasic_set_list(C) can be a possibly empty list of cobasic sets.
%% % If there are only unification/disunification tests then "othertests"
%% % is true.
%% 
%% othertests(true).
%% othertests([T|L]):- 
%%    list_of_tests(L).

:- pred compute_a_clause_test_in_minset_format(+Clause, +Type, -Clause_Test)
# "Compute the test of a clause and put it in minset format.".

compute_a_clause_test_in_minset_format(Clause, Type, Clause_Test) :-
	debug_message("Call: ~q.",
	    [compute_clause_test(Clause, Type, Var_list, Unification_Tests,
		    Arithm_Tests, _Meta_Tests)]),
	compute_clause_test(Clause, Type, Var_list, Unification_Tests,
	    Arithm_Tests, _Meta_Tests),
	debug_message("Exit: ~q.",
	    [compute_clause_test(Clause, Type, Var_list, Unification_Tests,
		    Arithm_Tests, _Meta_Tests)]),
	( (Arithm_Tests == [], Unification_Tests == []) ->
	    Clause_Test = true
	;
	    (Arithm_Tests == [] -> Others = true; Others = Arithm_Tests),
	    create_minset_and_project(Var_list, Unification_Tests, Others,
		Clause_Test) ).


%% compute_pred_test(+ClauseList, +Type, -Test): 
%%  Compute the input test Test of a predicate.
%%  ClauseList: list of clauses defining the predicate. 
%%  Type: mode and type information of the predicate.
%%  Test is a disjunction of tests (one per clause) represented as a
%%  list. Each test is in the format test(Minset, Others), where Minset
%%  is a minset and Others is a list of primitive tests which are not
%%  unification nor disunification tests (this list represents the
%%  conjunction of the primitive tests).
%% NOTE: If there are no tests in Others (i.e. Others is the empty
%% list) then Others is unified with the atom true. 
%% Example:
%% 
%%       [test(minset([[X|Y], Z], []), [X > Z]),
%%        test(minset([[X1|Y1], Z1], []), [X1 =< Z1]),
%%        test(minset([[], Z2], []), true)]

compute_pred_test(Clauses, Type, Test) :-
% 	write(Clauses), nl,
% 	write(Type), nl,
% 	functor(Type, F, A), %
% 	( debugging(nf) ->
% 	    write('Computing test for predicate: '), write(F/A), write('... '),
% 	    nl ; true ),
	compute_pred_test_0(Clauses, Type, [], Test1),
	transform_test_for_analysis(Test1, Test).
% 	( debugging(nf) ->
% 	    write('Test computed for predicate: '), write(F/A), nl, nl,
% 		write(Test), nl
% 	; true ).


:- pred compute_clause_test(+Clause, +Type, -Var_list, -Unification_Tests,
	    -Arithm_Tests, -Meta_Tests)
# "Compute the test of a clause.".

compute_clause_test(Clause, Type, Var_list, Unification_Tests, Arithm_Tests,
	    Meta_Tests) :-
	get_head_and_body_of_clause(Clause, Head, BodyList),
	create_unif_tests(Head, Type, Var_list, Unif_test_list),
	debug_message("Call: ~q.", [body_tests(BodyList, Unif_Body_Tests,
		    Arithm_Tests, Meta_Tests)]),
	body_tests(BodyList, Unif_Body_Tests, Arithm_Tests, Meta_Tests),
	debug_message("Exit: ~q.", [body_tests(BodyList, Unif_Body_Tests,
		    Arithm_Tests, Meta_Tests)]),
	append(Unif_test_list, Unif_Body_Tests, Unification_Tests).

create_unif_tests(Head, Type, Var_list, Unif_test_list) :-
	functor(Head, F, A),
	functor(X,    F, A),
	create_unif_tests_(1, A, X, Head, Type, Var_list, Unif_test_list).

create_unif_tests_(N, A, _X, _Head, _Type, [], []) :-
	N > A, !.
create_unif_tests_(N, A, X, Head, Type, Var_list, Unif_test_list) :-
	arg(N, X,    Var),
	arg(N, Head, Term),
	arg(N, Type, Mode:_Typ),
	( var(Term) ->
	    ( LVar = Term,
		Unif_test_list = NUnif_test_list )
	;
	    ( LVar = Var,
		Unif_test_list = [(LVar = Term)|NUnif_test_list] )
	),
	( Mode = in ->
	    Var_list = [LVar|NVar_list] ;
	    Var_list = NVar_list
	),
	N1 is N + 1,
	create_unif_tests_(N1, A, X, Head, Type, NVar_list, NUnif_test_list).

:- pred body_tests(+Literals, -Unif_Tests, -Arithm_Tests, -Meta_Tests)
# "Collects the tests in a clause body.".

body_tests([],             [],         [],           []) :- !.
body_tests([Litinfo|Rest], Unif_Tests, Arithm_Tests, Meta_Tests) :-
	get_test_flag_value(Litinfo, TestFlag),
	get_test_value(Litinfo, Test),
	get_lit_pred_name_arity(Litinfo, Lit),
	intercept(
	    clasify_test(TestFlag, Test, Rest, Unif_Tests, Arithm_Tests,
		Meta_Tests, Rest_1, Unif_Tests_1, Arithm_Tests_1, Meta_Tests_1
	    ),
	    wrong_neg_test(T),
	    warning(['For literal ', ~~(Lit), ' test is a free variable: ', T])
	),
	body_tests(Rest_1, Unif_Tests_1, Arithm_Tests_1, Meta_Tests_1).

clasify_test(TestFlag, _Test, Rest, Unif_Tests, Arithm_Tests, Meta_Tests, Rest,
	    Unif_Tests, Arithm_Tests, Meta_Tests) :-
	var(TestFlag),
	!.
clasify_test(unification, Test, Rest, [Test1|Unif_Tests], Arithm_Tests,
	    Meta_Tests, Rest, Unif_Tests, Arithm_Tests, Meta_Tests) :-
	!,
	push_neg_in_test(Test, Test1).
clasify_test(arithmetic, Test, Rest, Unif_Tests, [Test1|Arithm_Tests],
	    Meta_Tests, Rest, Unif_Tests, Arithm_Tests, Meta_Tests) :-
	!,
	push_neg_in_test(Test, Test1).
clasify_test(meta, Test, Rest, Unif_Tests, Arithm_Tests, [Test1|Meta_Tests],
	    Rest, Unif_Tests, Arithm_Tests, Meta_Tests) :-
	!,
	push_neg_in_test(Test, Test1),
	body_tests(Rest, Unif_Tests, Arithm_Tests, Meta_Tests).
clasify_test(definedtest, Test, Rest, Unif_Tests, Arithm_Tests, Meta_Tests,
	    Rest_1, Unif_Tests_1, Arithm_Tests, Meta_Tests) :-
	!,
%%  
%%      Test = [Uni, Ari, Meta],
%%      append(Uni, Arithm_Tests,  
%%      push_neg_in_test(Test, Test1),
%%      body_tests(Rest, Unif_Tests, Arithm_Tests, Meta_Tests).
	Test = defined(Unifs, Litinfos),
	append(Unifs,    Unif_Tests_1, Unif_Tests),
	append(Litinfos, Rest,         Rest_1).

/*
:- pred compute_clause_test_before_the_first_cut(+Clause, +Type, -Var_list,
	    -Unification_Tests, -Arithm_Tests, -Meta_Tests)
# "Compute the test of a clause before the first cut.".

compute_clause_test_before_the_first_cut(Clause, Type, Var_list,
	    Unification_Tests, Arithm_Tests, Meta_Tests) :-
	get_head_and_body_of_clause(Clause, Head, BodyList),
	create_unif_tests(Head, Type, Var_list, Unif_test_list),
	literals_before_the_first_cut(BodyList, Literals_Before_1st_Cut),
	body_tests(Literals_Before_1st_Cut, Unif_Body_Tests, Arithm_Tests,
	    Meta_Tests),
	append(Unif_test_list, Unif_Body_Tests, Unification_Tests).
*/

% MOVED FROM low_level.pl 

transform_test_for_analysis(Test, Test).

compute_pred_test_0(Clauses, _Type, InTest, InTest) :-
	there_are_no_more_clauses(Clauses),
	!.
compute_pred_test_0([Clause|CList], Type, InTest, OuTest) :-
	compute_a_clause_test_in_minset_format(Clause, Type, Clause_Test),
	( Clause_Test == true -> OuTest = true
	;
	    ( Clause_Test == false ->
		TemTest = InTest;
		TemTest = [Clause_Test|InTest] ),
	    compute_pred_test_0(CList, Type, TemTest, OuTest) ).


% END OF MOVED FROM low_level.pl
