/********************************************************************
*  File: conversion.pl                                              * 
*  Purpose: Conversion of data to different formats                 *
*  Author:  Pedro Lopez                                             *
*  Date:    3 Sep  1995                                             *
*                                                                   *
********************************************************************/

%% list_2_minset_test_format(+Test_List, +Vars_Tested, +Other_Tests, -Test)
%% Purpose: rewrites Test_List as a minset Minset. 
%% Test_List: a list of unification tests.
%% Vars_Tested: a list of variables.
%% Other_Tests:
%% Test:

list_2_minset_test_format(Test_List, Vars_Tested, Other_Tests,
	    OutMinset) :-
	separate_eqs_and_diseqs(Test_List, Eqs, DisEqs),
	copy_term(t(Vars_Tested, Other_Tests, Eqs), t(NVars_Tested,
		NOther_Tests, NEqs)),
	( solvable_eqs(NEqs) ->
	    create_cobasic_sets(DisEqs, Vars_Tested, NVars_Tested, CoBasicList
	    ),
	    OutMinset = test(minset(NVars_Tested, CoBasicList), NOther_Tests)
	; OutMinset = false
	).


create_cobasic_sets([], _Vars_Tested, _NVars_Tested, []).
create_cobasic_sets(['$noteq$'(X, Y)|DisEqList], Vars_Tested,
	    NVars_Tested, OutCoBasicList) :-
	copy_term((Vars_Tested, =(X, Y)), (NVars, NEq)),
	solvable_eqs([NEq]),
	copy_term(NVars_Tested, Cobasic_Set),
	( unify_with_occurs_check(Cobasic_Set, NVars)
	-> OutCoBasicList = [Cobasic_Set|CoBasicList]
	; OutCoBasicList = CoBasicList ),
	create_cobasic_sets(DisEqList, Vars_Tested, NVars_Tested, CoBasicList).

/*
convert_typass([],         []) :- !.
convert_typass([X:Type|L], [X:CType|CL]) :-
	convert_type(Type, CType),
	convert_typass(L, CL).

%% Conversion from Debray's type definition format to my format.  
%% Assign to all (anonymous) variables in Type, the type '$top$'.
%% It is assumed that a list represents a union, and that there is 
%% no union type in none of the items of the list.

%%  convert_type_union([], []).
%%  convert_type_union([Type|Typunion], [CType|CTypunion]):-
%%          convert_type(Type, CType),
%%          convert_type_union(Typunion, CTypunion).

convert_type(Type, '$top$') :-
	var(Type), !.
convert_type(top,  '$top$') :- !.
convert_type(Type, CType) :-
	functor(Type,  F, A),
	functor(CType, F, A),
	convert_type_3(A, Type, CType).

convert_type_3(0, _,    _) :- !.
convert_type_3(A, Type, CType) :-
	arg(A, Type, Arg),
	convert_type(Arg, CArg),
	arg(A, CType, CArg),
	NA is A - 1,
	convert_type_3(NA, Type, CType).
*/

%% translate_to_bool_format(+MinsetList, -Disj):
%% write the list of minsets MinsetList (which represents a disjunction) 
%% into a representation using the functors ';' (disjunction) and 
%% ','(conjunction) .   

translate_to_bool_format([test(Minset, _Others)], Disj) :- !,
	b_trans_minset(Minset, Disj).
translate_to_bool_format([test(Minset, _Others)|Mlist], ';'(Disj, RDisj)) :- !,
	b_trans_minset(Minset, Disj),
	translate_to_bool_format(Mlist, RDisj).

b_trans_minset(minset(BasicSet, CoBasicSets), Conj) :-
	( CoBasicSets = []
	-> Conj = BasicSet;
	    b_trans_cobasicsets(CoBasicSets, PCoBasicSets),
	    Conj = ','(BasicSet, PCoBasicSets)
	).

b_trans_cobasicsets([CoBasicSet],          not(CoBasicSet)) :- !.
b_trans_cobasicsets([CoBasic|CoBasicSets], ','(not(CoBasic), PCoBasicSets)) :-
	b_trans_cobasicsets(CoBasicSets, PCoBasicSets).
