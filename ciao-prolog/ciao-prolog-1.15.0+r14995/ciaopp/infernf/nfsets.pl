:- module(nfsets,
	    [add_item_to_typassign/4,
		change_type_in_typassgn/4,
		create_minset_and_project/4,
		generate_type_annotated_term/4,
		included/2,
		included_ta_term_in_tuple/2,
		remove_item_from_typAss/3,
		simplify_conjunctions/2,
		ta_term_basicset_intersec/3,
		ta_term_empty/1,
		translate_to_bool_format/2
	    ],
	    [assertions, isomodes]).

:- use_module(library(lists), [length/2, append/3]).

:- use_module(library(iso_misc), [unify_with_occurs_check/2]).

:- use_module(typeslib(typeslib),
	    [create_new_type_rule/2,
		intersec_types/4,
		compute_types/4
	    ]).

:- use_module(typeslib(type_support),
	    [closed_var_list/2,
		var_list/2,
		vset_diff/3
	    ]).

%%      list_2_minset_test_format/4
%%           conversion
:- include(infernf(conversion)).
:- include(infernf(s_eqsolve)).



/********************************************************************
*                                                                   *
*  File:    nfsets.pl                                               *
*  Purpose: Manipulation of minsets, and basic and cobasic sets     *
*  Author:  Pedro Lopez                                             *
*  Date:    3 Aug 1995                                              *
*                                                                   *
********************************************************************/

:- pred generate_type_annotated_term(+ModeType, +UseMasc, ?Masc, -Ta_Term)

# "Creates a type-annotated term @var{Ta_Term} of the form (BasicSet,
TypeAss), where BasicSet is a list of distinct variables (each
variable corresponds to an input argument of the predicate, in the
left to right order). TypeAss is a type assigment which assigns the
type expressed in @var{ModeType} to each variable of BasicSet.
e.g. generate_type_annotated_term(p(in:term, in:term, out:term), ([B,
A],[A:term, B:term])).".

generate_type_annotated_term(ModeType, UseMasc, Masc, Ta_Term) :-
	functor(ModeType, _, A),
	( UseMasc == true
	-> generate_a_type_annotated_term_WITH_masc(1, A, ModeType, Masc,
		Ta_Term)
	; generate_a_type_annotated_term_WITHOUT_masc(1, A, ModeType, Ta_Term)
	).


generate_a_type_annotated_term_WITHOUT_masc(1, A, ModeType, Ta_Term) :-
	generate_basicset_and_typeass(1, A, ModeType, BasicSet, TypeAss),
	construct_ta_term(BasicSet, TypeAss, Ta_Term).

construct_ta_term(BasicSet, TypeAss, (BasicSet, TypeAss)).

generate_a_type_annotated_term_WITH_masc(1, A, ModeType, Masc, OuTa_Term) :-
	copy_term(Masc, NewMasc),
	gen_basic_typeass_mascbasic(1, A, ModeType, NewMasc, BasicSet, TypeAss,
	    MascBasicSet),
	construct_ta_term(BasicSet, TypeAss, Ta_Term),
	( ta_term_basicset_intersec_0(Ta_Term, MascBasicSet, TmpTa_Term)
	-> OuTa_Term = TmpTa_Term
	; OuTa_Term = ([], []) ).

generate_basicset_and_typeass(N, A, _ModeType, [],       []) :- N > A, !.
generate_basicset_and_typeass(N, A, ModeType,  BasicSet, TypeAss) :-
	N =< A,
	!,
	arg(N, ModeType, Mode:Type),
	( Mode = in
	-> BasicSet = [Var|RBasics],
	    TypeAss = [Var:Type|RTypeAss]
	; BasicSet = RBasics,
	    TypeAss = RTypeAss
	),
	N1 is N + 1,
	generate_basicset_and_typeass(N1, A, ModeType, RBasics, RTypeAss).


gen_basic_typeass_mascbasic(N, A, _ModeType, _Masc, [],       [],      []) :- N
	> A, !.
gen_basic_typeass_mascbasic(N, A, ModeType,  Masc,  BasicSet, TypeAss,
	    MascBasicSet) :-
	N =< A,
	!,
	arg(N, ModeType, Mode:Type),
	( Mode = in
	-> BasicSet = [Var|RBasics],
	    TypeAss = [Var:Type|RTypeAss],
	    arg(N, Masc, ArgNMasc),
	    MascBasicSet = [ArgNMasc|ReMascBasicSet]
	; BasicSet = RBasics,
	    TypeAss = RTypeAss,
	    MascBasicSet = ReMascBasicSet
	),
	N1 is N + 1,
	gen_basic_typeass_mascbasic(N1, A, ModeType, Masc, RBasics, RTypeAss,
	    ReMascBasicSet).


:- pred ta_term_basicset_intersec(+Ta_Term, +Basic_Set, -Out_Ta_term)

# "Creates a type annotated term @var{Out_Ta_term} which is the intersection
of the type annotated term @var{Ta_Term} and the basic set @var{Basic_Set}.
@var{Ta_Term} and @var{Basic_Set} are not modified.  The variables in @var{Out_Ta_term}
are fresh.".

ta_term_basicset_intersec(Ta_Term, Basic_Set, OTa_Term) :-
	( ta_term_basicset_intersec_0(Ta_Term, Basic_Set, TeTa_Term)
	-> OTa_Term = TeTa_Term
	; set_ta_term_empty(OTa_Term) ).

ta_term_basicset_intersec_0(Ta_Term, Basic_Set, OTa_Term) :-
	Ta_Term = (Term, TypeAss),
	var_list((Term, Basic_Set), Var_List),
	copy_term(Term,      New_Term),
	copy_term(Basic_Set, New_Basic_Set),
	var_list((New_Term, New_Basic_Set), New_Var_List),
	unify_with_occurs_check(New_Term, New_Basic_Set),
	compute_types(Var_List, New_Var_List, TypeAss, Types),
	var_list(New_Term, New_Term_Vars),
%% init_typ_symbol_counter, % warning!!
	intersec_types(New_Term_Vars, Types, [], F_TypeAss),
	OTa_Term = (New_Term, F_TypeAss).
%%      Commented 29 Oct 96
%%      normalize(F_TypeAss, [], New_Term, O_TypeAss),
%%      OTa_Term = (New_Term,  O_TypeAss).

%% Old version
%% ta_term_basicset_intersec(Ta_Term, Basic_Set, OTa_Term):-
%%      Ta_Term = (Term, TypeAss),
%%      var_list((Term, Basic_Set), Var_List),
%%      copy_term(Term, New_Term),
%%      copy_term(Basic_Set, New_Basic_Set),
%%      var_list((New_Term, New_Basic_Set), New_Var_List),
%%      unify_with_occurs_check(New_Term, New_Basic_Set),
%%      compute_types(Var_List, New_Var_List, TypeAss, Types),
%%      var_list(New_Term, New_Term_Vars),
%%      %% init_typ_symbol_counter, % warning!!
%%      intersec_types(New_Term_Vars, Types, [], F_TypeAss),
%%      OTa_Term = (New_Term,  F_TypeAss). 
%%  %%      Commented 29 Oct 96
%%  %%      normalize(F_TypeAss, [], New_Term, O_TypeAss),
%%  %%      OTa_Term = (New_Term,  O_TypeAss).

%% Comme 1_dec-96
%% ta_term_basicset_intersec(Ta_Term, Basic_Set, OTa_Term):-
%%      Ta_Term = (Term, TypeAss),
%%      var_list((Term, Basic_Set), Var_List),
%%      copy_term(Term, New_Term),
%%      copy_term(Basic_Set, New_Basic_Set),
%%      var_list((New_Term, New_Basic_Set), New_Var_List),
%%      unify_with_occurs_check(New_Term, New_Basic_Set),
%%      compute_types(Var_List, New_Var_List, TypeAss, Types),
%%      var_list(New_Term, New_Term_Vars),
%%      %% init_typ_symbol_counter, % warning!!
%%      intersec_types(New_Term_Vars, Types, [], F_TypeAss),
%%      OTa_Term = (New_Term,  F_TypeAss). 
%%  %%      Commented 29 Oct 96
%%  %%      normalize(F_TypeAss, [], New_Term, O_TypeAss),
%%  %%      OTa_Term = (New_Term,  O_TypeAss).


%% Old version 14 Nov 96.
%% generate_type_annotated_term(ModeType, (BasicSet, OutTypeAss)):-
%%    functor(ModeType, _, A),
%%    generate_basicset_and_typeass(1, A, ModeType, BasicSet, TypeAss),
%%    convert_typass(TypeAss, CTypAsg),
%%    normalize(CTypAsg, [], BasicSet, OutTypeAss).


%%-----------------------------------------------------------------------------
%% type annotated terms

% Checks that the type annotated term X is empty.

ta_term_empty((_Term, TypAss)) :-
	TypAss == empty_type_assignment.

set_ta_term_empty((_Term, TypAss)) :-
	TypAss = empty_type_assignment.

/*
%% normalize(TypeAss, TypeAss1, Subs, NewTypeAss, NewSubs)
%% Input: two type assignments TypeAss and TypeAss1, and an idempotent
%%      substitution Subs. 
%% 
%% Output: A pair (NewTypeAss, NewSubs), where: 
%%   a) NewTypeAss is a type assignment
%%   of the form $(x_1:T_1, \ldots, x_k:T_k)$, where each $T_i$, 
%%   $1 \leq i \leq k$ is a type expression which is either, a union type, 
%%   or $top$, and b) NewSubs is an idempotent substitution.

normalize_4([],                 TypeAss,   _Term, TypeAss).
normalize_4([Var:Type|TypeAss], InTypeAss, Term,  OutTypeAss) :-
	define_a_ground_type(Type),
	!,
	Var = Type,
	normalize_4(TypeAss, InTypeAss, Term, OutTypeAss).
normalize_4([Var:Type|TypeAss], InTypeAss, Term, OutTypeAss) :-
% functor(Type, F, A),
	compound_pure_type_term(Type, _Term, F, A),
% F/A \== '$typedef$'/1, F/A \== '$top$'/0,
	!,
	functor(NTerm, F, A), Var = NTerm,
	normalize_args(A, NTerm, Type, TypeAss, InTypeAss, Term, OutTypeAss).
normalize_4([Var:Type|TypeAss], InTypeAss, Term, OutTypeAss) :-
	normalize_4(TypeAss, [Var:Type|InTypeAss], Term, OutTypeAss).

normalize_args(0, _NTerm, _Type, TypeAss, InTypeAss, Term, OutTypeAss) :-
	normalize_4(TypeAss, InTypeAss, Term, OutTypeAss).
normalize_args(A, NTerm, Type, TypeAss, InTypeAss, Term, OutTypeAss) :-
	arg(A, NTerm, Var),
	arg(A, Type,  TypeArg),
	NA is A - 1,
	normalize_args(NA, NTerm, Type, [Var:TypeArg|TypeAss], InTypeAss,
	    Term, OutTypeAss).
*/

:- pred create_minset_and_project(+Var_list, +Test_List, -Other_Tests, -PMinset)

# "IMPORTANT NOTE: This procedure needs to be modified to compute the projection over
  the variables in Var_list correctly.

@var{Var_list}: a list containing the input variables of a predicate.
Each variable corresponds to a input argument of the predicate. The
variables are ordered according to the argument number (in left to
right order) they correspond to.  @var{Test_List}: a list of
unification tests.  @var{PMinset}: a minset. @var{PMinset} is
@var{Test_List} in minset format.  The minset is represented as
minset(Term, CoBasicList).".

create_minset_and_project(Var_list, Test_List, Other_Tests, PTest) :-
%% compute the list of variables Eliminable_Vars which are in Test_List
%% and are not in Var_list, i.e. variables that should be elimined,
%% because they are existentally cuantified.
	closed_var_list(Test_List, Test_Vars),
	vset_diff(Test_Vars, Var_list, Eliminable_Vars),
%%
	append(Var_list, Eliminable_Vars, Vars_Tested),
	list_2_minset_test_format(Test_List, Vars_Tested, Other_Tests, Test),
	( Test == false ->
	    PTest = false
	;
	    project_minset(Var_list, Test, PTest) ).

project_minset(Var_list, test(Minset, Others), test(PMinset, Others)) :-
	project_minset_0(Var_list, Minset, PMinset).

project_minset_0(Var_list, minset(BasicSet, CoBasicSets),
	    minset(PBasicSet, PCoBasicSets)) :-
	project_basicset(Var_list, BasicSet, PBasicSet),
	project_cobasicsets(CoBasicSets, Var_list, PCoBasicSets).


project_basicset([],              _BasicSet,       []) :- !.
project_basicset([_Var|Var_list], [BVar|BasicSet], [BVar|PBasicSet]) :-
	project_basicset(Var_list, BasicSet, PBasicSet).

project_cobasicsets([],                    _Var_list, []).
project_cobasicsets([CoBasic|CoBasicSets], Var_list,  [PCoBasic|PCoBasicSets])
:- project_basicset(Var_list, CoBasic, PCoBasic),
	project_cobasicsets(CoBasicSets, Var_list, PCoBasicSets).


simplify_conjunctions([],            []).
simplify_conjunctions([Conj|NBTest], OutConj) :-
	( simplify_conjunction(Conj, SConj)
	-> OutConj = [SConj|SNBTest]
	; OutConj = SNBTest
	),
	simplify_conjunctions(NBTest, SNBTest).

%% NBTest is supposed to be non-empty.

simplify_conjunction(NBTest, minset(SBasic, CoBasicList)) :-
	separate_basics_and_cobasics(NBTest, Basic, CoBasic),
	( Basic = []
	-> NBTest = [not(CoBset)|_],
	    gen_basic_set(CoBset, SBasic)
	; Basic = [BasicSet|_],
	    basic_set_intersection(Basic, BasicSet, SBasic)
	),
	simp_cobasic_sets(CoBasic, SBasic, CoBasicList).

gen_basic_set(CoBset, SBasic) :-
	length(CoBset, N),
	length(SBasic, N).

separate_basics_and_cobasics([],            [],  []).
separate_basics_and_cobasics([not(S)|Rest], Bas, [not(S)|CoBas]) :-
	!,
	separate_basics_and_cobasics(Rest, Bas, CoBas).
separate_basics_and_cobasics([S|Rest], [S|Bas], CoBas) :-
	separate_basics_and_cobasics(Rest, Bas, CoBas).

basic_set_intersection([],            BasicInt,  BasicInt) :- !.
basic_set_intersection([Basic|BList], IBasicInt, OBasicInt) :-
	basic_intersec(IBasicInt, Basic, Int),
	basic_set_intersection(BList, Int, OBasicInt).

basic_intersec(BSet1, BSet2, New_BSet1) :-
	copy_term(BSet1, New_BSet1),
	copy_term(BSet2, New_BSet2),
	unify_with_occurs_check(New_BSet1, New_BSet2).

simp_cobasic_sets([],                 _Bas, []).
simp_cobasic_sets([not(Cob)|CobList], Bas,  OutCoBasicList) :-
	copy_term(Cob, NCob),
	copy_term(Bas, NBas),
	( unify_with_occurs_check(NCob, NBas)
	-> OutCoBasicList = [NBas|CoBasicList]
	; OutCoBasicList = CoBasicList ),
	simp_cobasic_sets(CobList, Bas, CoBasicList).

/*
%% non_empty_intersec(+BSet1, +BSet2):
%%   Purpose: Succeeds if the intersection of the basic sets BSet1 and BSet2
%%      is not empty (i.e. BSet1 and BSet2 unify).

non_empty_intersec(BSet1, BSet2) :-
	copy_term(BSet1, New_BSet1),
	copy_term(BSet2, New_BSet2),
	unify_with_occurs_check(New_BSet1, New_BSet2).

%% non_empty_intersec_3(+BSet1, +BSet2, -Substitution):
%%   Purpose: Succeeds if the intersection of the basic sets BSet1 and BSet2
%%      is not empty (i.e. BSet1 and BSet2 unify).
%%      "Substitution" is the the mgu of BSet1 and BSet2 projected to the 
%%      variables of BSet1.

non_empty_intersec_3(BSet1, BSet2,
	    subs(BSet1_Var_List, New_BSet1_Var_List)) :-
	var_list(BSet1, BSet1_Var_List),
	copy_term(BSet1, New_BSet1),
	var_list(New_BSet1, New_BSet1_Var_List),
	copy_term(BSet2, New_BSet2),
	unify_with_occurs_check(New_BSet1, New_BSet2).
*/
%% included(Basic_Set1, Basic_Set):
%% Purpose: Succeeds if Basic_Set subsumes to Basic_Set1.

included(Basic_Set1, Basic_Set) :-
	copy_term(Basic_Set1, New_Basic_Set1),
	var_list(New_Basic_Set1, New_Basic_Set1_Var_List),
	assign_distinct_constants(New_Basic_Set1_Var_List, 1),
	copy_term(Basic_Set, New_Basic_Set),
	unify_with_occurs_check(New_Basic_Set1, New_Basic_Set).

%% included_ta_term_in_tuple(Ta_term, Tuple):
%% Purpose: Succeeds if Tuple subsumes to Ta_term.

included_ta_term_in_tuple(Ta_term, Tuple) :-
	Ta_term = (Tuple1, _TypAss),
	included(Tuple1, Tuple).


%% assign_distinct_constants(Var_List, Num):
%%    Assigns distinct constants (which do not appear in the type assigment nor
%%    in the tests) to the variables in Var_List.

assign_distinct_constants(Var_List, _Num) :-
	var(Var_List), !.
assign_distinct_constants(Var_List, Num) :-
	nonvar(Var_List),
	Var_List = [Var|Rest_Var_List],
	name(Num, Num_List),
	append(Num_List, "!@#$%^&", Constant_List),
	name(Var, Constant_List),
	NNum is Num + 1,
	assign_distinct_constants(Rest_Var_List, NNum).


%%-----------------------------------------------------------------------------
%% Operations on type assignments.

%% add_item_to_typassign(InTypAss, Var, Type, OuTypAss)
%% InTypAss: a type assignment.
%% Var: a variable.
%% Type: a type.
%% OuTypAss: a type assignment.
%% Description: adds the assignment Var:Type at the beginning of InTypAss,
%% obtaining OuTypAss.

add_item_to_typassign(InTypAss, Var, Type, OuTypAss) :-
	OuTypAss = [Var:Type|InTypAss].

%% change_type_in_typassgn(+Types, +Var, +InTypeAss, -OuTypeAss)
%% Types: a list of types.
%% Var: a variable.
%% InTypeAss: a type assignment.
%% OuTypeAss: a type assignment.
%% Description:  OuTypeAss is the result of replacing the type of
%%         variable Var in InTypeAss by Types. If Types is empty,
%%         then OuTypeAss is InTypeAss. If types have more than one
%%         item, then a type rule defining Types is created, and its
%%         type symbol is set as the type of Var in OuTypeAss.


change_type_in_typassgn([],     _Var, ITypeAss, ITypeAss) :- !.
change_type_in_typassgn([Type], Var,  ITypeAss, OTypeAss) :-
	!,
	replace_type_in_typeass(ITypeAss, Var, Type, OTypeAss).
change_type_in_typassgn([Type|Restypes], Var, ITypeAss, OTypeAss) :-
	!,
	create_new_type_rule([Type|Restypes], TypeSymbol),
	replace_type_in_typeass(ITypeAss, Var, TypeSymbol, OTypeAss).

%% replace_type_in_typeass(+InTypeAss, +Var, +Type, -OuTypAss)
%% InTypeAss: a type assignment.
%% Var: a variable.
%% Type: a type.
%% OuTypAss: a type assignment.
%% Description:  OuTypeAss is the result of replacing the type of
%%         variable Var in InTypeAss by Type.

replace_type_in_typeass([Var:_OldType|Rest], IVar, Type,
	    [Var:Type|Rest]) :- Var == IVar, !.
replace_type_in_typeass([Var:OldType|Rest],  IVar, Type,
	    [Var:OldType|RTypass]) :-
	Var \== IVar,
	replace_type_in_typeass(Rest, IVar, Type, RTypass).

%% remove_item_from_typAss(+InTypeAss, +Var, -OuTypAss)
%% InTypeAss: a type assignment. 
%% Var: a variable. 
%% OuTypAss: a type assignment.
%% Description: remove the assignment Var:Type in InTypeAss,
%% obtaining OuTypAss.

remove_item_from_typAss([Var:_Type|Rest], IVar, Rest) :- Var == IVar, !.
remove_item_from_typAss([Var:Type|Rest],  IVar, [Var:Type|RTypass]) :-
	Var \== IVar,
	remove_item_from_typAss(Rest, IVar, RTypass).
