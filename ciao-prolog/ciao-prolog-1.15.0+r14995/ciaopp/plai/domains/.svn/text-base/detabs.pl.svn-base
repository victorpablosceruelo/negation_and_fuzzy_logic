:- module(detabs,
	[ det_call_to_entry/8,
	  det_exit_to_prime/7,
	  det_project/3,
	  det_extend/4,
	  det_compute_lub/2,
	  det_compute_covering/3,
	  det_glb/3,
	  det_less_or_equal/2,
	  det_identical_abstract/2,
	  det_sort/2,
	  det_call_to_success_fact/8,
	  det_special_builtin/1,
	  det_success_builtin/5,
	%  det_call_to_success_builtin/6,
	  det_input_interface/4,
	  det_input_user_interface/3,
	  det_asub_to_native/3,
	  det_unknown_call/3,
	  det_unknown_entry/2,
	  det_empty_entry/2,
          det_statistics/1,
          det_obtain/4
	],
	[ assertions,regtypes,basicmodes,hiord
	]).

:- use_module(infernf(nfsets), [create_minset_and_project/4]).
:- use_module(domain(mutexcheck), [mutual_exclusion_check/5]).
:- use_module(infernf(nfbool), [push_neg_in_test/2, remove_negation/2, translate_test/2]).
:- use_module(domain(share), [shfr_obtain/4]).
:- use_module(domain(s_eqs), [peel/4]).
:- use_module(domain(nfdet_statistics)).
:- use_module(spec(s_simpspec), [make_atom/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(sets), [merge/3]).

%------------------------------------------------------------------------%

:- doc(bug,"Think on an adequate treatment for negation.").

%------------------------------------------------------------------------%

asub(nf(Tests,Covered,Fails),Tests,Covered,Fails).

tests(t(InVars,Unif,Arith,Meta),InVars,Unif,Arith,Meta).

%------------------------------------------------------------------------%
% det_call_to_entry(+,+,+,+,+,+,-,-)                                      %
% det_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo)                %
%------------------------------------------------------------------------%
% Entering a clause: initialize an asub to start gathering the tests of
% this clause 

det_call_to_entry(_Sv,_Sg,_Hv,_Head,_Fv,_Proj,Entry,_Extra):-
	det_empty_entry(_Vars,Entry).

%------------------------------------------------------------------------%
% det_exit_to_prime(+,+,+,+,+,-,-)                                        %
% det_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime)                   %
%------------------------------------------------------------------------%
% Exiting a clause: project the tests gathered while traversing the clause 
% onto the variables of the goal

det_exit_to_prime(Sg,_Hv,Head,_Sv,Exit,GVars,Prime):-
	asub(Exit,Tests,Covered,Fails),
	tests(Tests,_InVars,Unif,Arith,Meta),
/*	create_unif_tests(Sg,Type,SgInVars,SgEqs),
	create_unif_tests(Head,Type,HeadInVars,HeadEqs),
	copy_term((HeadInVars,HeadEqs,Tests),(HeadInVarsC,HeadEqsC,TestsC)),
	unify(TestsC),
	unify(HeadEqsC),
	copy_term((SgInVars,SgEqs),(SgInVarsC,SgEqsC)),
	unify(SgEqsC),
	SgInVarsC = HeadInVarsC, !,
	new_tests(SgInVars,SgInVarsC,PrimeTests),
*/
	peel(Sg,Head,Bindings,Unif), !,
	tests(PrimeTests,GVars,Bindings,Arith,Meta),
	asub(Prime,PrimeTests,Covered,Fails).
det_exit_to_prime(_Sg,_Hv,_Head,_Sv,_Exit,_Extra,'$bottom').

/*
new_tests([],[],[]).
new_tests([X|Xs],[Y|Ys],[X=Y|Eqs]):-
	new_tests(Xs,Ys,Eqs).
*/

%------------------------------------------------------------------------%
% det_project(+,+,-)                                                      %
% det_project(ASub,Vars,Proj)                                             %
%------------------------------------------------------------------------%
% To project on Vars, leave only tests for Vars

det_project(ASub,Vars,Proj):-
	asub(ASub,Tests0,Covered,Fails),
	select_tests(Tests0,Vars,TestsProj),
	asub(Proj,TestsProj,Covered,Fails).

select_tests(Tests,_Vars,Tests).

%------------------------------------------------------------------------%
% det_extend(+,+,+,-)                                                     %
% det_extend(Prime,Sv,Call,Succ)                                          %
%------------------------------------------------------------------------%
% Return back to the calling clause: merge the tests in Call with the
% tests in Prime

det_extend(Prime,_Sv,Call,Succ):-
	asub(Prime,Tests0,Covered0,Fails0),
	asub(Call,Tests1,Covered1,Fails1),
	merge_tests(Tests0,Tests1,Tests),
	glb_covering(Covered0,Covered1,Covered),
	glb_nonfailure_1(Fails0,Fails1,Fails),
	asub(Succ,Tests,Covered,Fails).

% b) simple tests, do not collect:
merge_tests(_Tests0,Tests,Tests).
% c) collect tests from the body goals
%% merge_tests(Tests0,Tests1,Tests):-
%% 	append(Tests0,Tests1,Tests).

glb_nonfailure_1(not_fails,not_fails,not_fails):- !.
glb_nonfailure_1(_,_,possibly_fails).

%------------------------------------------------------------------------%
% det_compute_lub(+,-)                                                    %
% det_compute_lub(ListASub,Lub)                                           %
%------------------------------------------------------------------------%
% Simply put all tests together (this is due to the way this operation
% is called from the fixpoint)

det_compute_lub(ListASub,Lub):-
	asub(ASub,[],covered,not_fails),
	foldr( ListASub,  ASub, accumulate, Lub ).

:- entry accumulate/3.
% Differs from nf
accumulate('$bottom',ASub0,ASub0):- !.
accumulate(ASub,ASub0,NewASub):-
	asub(ASub0,Tests0,Covered0,Fails0),
	asub(ASub,Tests,Covered1,Fails1),
	append_(Tests,Tests0,Tests1),
	lub_covering(Covered0,Covered1,Covered),
	lub_nonfailure(Fails0,Fails1,Fails),
	asub(NewASub,Tests1,Covered,Fails).
%

append_(Tests,Tests0,Tests1):-
	Tests=[_|_], !,
	append(Tests,Tests0,Tests1).
append_(Tests,Tests0,[Tests|Tests0]).

lub_covering(covered,covered,covered):- !.
lub_covering(_,_,not_covered).

lub_nonfailure(not_fails,not_fails,not_fails):- !.
lub_nonfailure(_,_,possibly_fails).

%------------------------------------------------------------------------%
% det_compute_covering(+,+,-)                                             %
% det_compute_covering(ModeTypes,Lub,ASub)                                %
%------------------------------------------------------------------------%
% New operation, has to be called from fixpoint when all clauses of a 
% predicate have been traversed: compute covering information

det_compute_covering(ModeTypes,Lub,ASub):-
	% this one is a little tricky: Lub is not a well-formed abstract
        % substitution, it is a collection of tests from compute_lub
	asub(Lub,TestsList,_Covered,Fails0),
	( TestsList = [] -> CoverTest = true %% ???? PLG Differs from nf.
	; TestsList = [[]] -> CoverTest = true %% ???? PLG
	; TestsList = [_|_] -> 
	  test_list_to_cover_test(TestsList, [], CoverTest)
	% if only one asub, lub is not computed, thus, it is not a list:
	; test_list_to_cover_test([TestsList], [], CoverTest)
	),
        covers_check(ModeTypes,false,_Masc,CoverTest,Res),
	result_to_covering(Res,Covered),
	covering_to_nonfailure(Covered,Fails1),
	% Fails0 should always be not_fails!
	lub_nonfailure(Fails0,Fails1,Fails),
	foldr_testlist(TestsList,Tests),
	asub(ASub,Tests,Covered,Fails).

covers_check(ModeTypes,UseMasc,Masc,CoverTest,Res):-
    mutual_exclusion_check(ModeTypes,UseMasc,Masc,CoverTest,Res).

/*
covers_check_(ModeTypes,false,_Masc,MinSetTestsList,Res):-
	covers_check(ModeTypes,false,_Masc,MinSetTestsList,Res), !.
covers_check_(_ModeTypes,false,_Masc,_MinSetTestsList,true).
*/

result_to_covering(true,covered).
result_to_covering(fail,not_covered).
result_to_covering(false,not_covered).

covering_to_nonfailure(covered,not_fails).
covering_to_nonfailure(not_covered,possibly_fails).

foldr_testlist(_TestsList,Tests):-
	det_empty_entry([],Entry),
	asub(Entry,Tests,_Covered,_Fails).

% Differs from nf

test_list_to_cover_test([], InTest, InTest):-
	!.
test_list_to_cover_test([T|TList], InTest, OuTest):-
  (has_a_cut(T) -> 
        TemTest = InTest
  ; 
	clause_test_to_minset_test(T, Clause_Test),
	( Clause_Test == true
	-> OuTest = true
	 ; ( Clause_Test == false
	   -> TemTest = InTest
	    ; TemTest = [Clause_Test|InTest]
	   )
	)
  ),
  test_list_to_cover_test(TList, TemTest, OuTest).

% has_a_cut(t(_InVars,_Unif,_Arith,Meta)):- member(cut, Meta). 
has_a_cut(t(_InVars,_Unif,_Arith,Meta)):- member((!), Meta). 

 %% test_list_to_cover_test([], InTest, InTest):-
 %% 	!.
 %% test_list_to_cover_test([T|TList], InTest, OuTest):-
 %% 	( has_a_cut(T) -> 
 %%           TemTest = InTest
 %% 	; 
 %%           clause_test_to_minset_test(T, Clause_Test),
 %%           TemTest = [Clause_Test|InTest]
 %% 	),
 %% 	test_list_to_cover_test(TList, TemTest, OuTest).
 %% 
 %% 
 %%                     !. 
 %% has_a_cut([_H|T]):- has_a_cut(T). 
 %% 
 %% is_a_cut(_H):- fail.
 %% % is_a_cut(H):- functor(H, (!), 0).

%

clause_test_to_minset_test(Clause_Test, Clause_Minset_Test):-
	tests(Clause_Test, Var_list, Unification_Tests, Arithm_Tests, Meta_Tests),
	( Arithm_Tests == [], Unification_Tests == [], Meta_Tests == [] -> 
	    Clause_Minset_Test = true
	  ;  
            clause_test_to_minset_test_(Var_list,Unification_Tests,Arithm_Tests,Meta_Tests,Clause_Minset_Test)
        ).

clause_test_to_minset_test_(Var_list,Unification_Tests,Arithm_Tests,Meta_Tests,Clause_Minset_Test):-
   append(Arithm_Tests, Meta_Tests, Other_Tests),
   ( Other_Tests == [] -> 
       Others = true
   ; 
       Others = others(Arithm_Tests, Meta_Tests) 
   ),
   create_minset_and_project(Var_list, Unification_Tests, Others, Clause_Minset_Test).
           
 %% clause_test_to_minset_test(Clause_Test, Clause_Minset_Test):-
 %% 	tests(Clause_Test,Var_list,Unification_Tests,Arithm_Tests,_Meta_Tests),
 %% 	( Arithm_Tests == [], Unification_Tests == [] -> 
 %% 	  Clause_Minset_Test = true
 %% 	; ( Arithm_Tests == [] -> Others = true; Others = Arithm_Tests ),
 %% 	  create_minset_and_project(Var_list, Unification_Tests, Others, 
 %% 	                            Clause_Minset_Test)
 %% 	).

% End added by PLG

%------------------------------------------------------------------------%
% det_glb(+,+,-)                                                         %
% det_glb(ASub0,ASub1,Glb)                                               %
%------------------------------------------------------------------------%

det_glb(ASub0,ASub1,Glb):-
	asub(ASub0,Tests0,Covered0,Fails0),
	asub(ASub1,Tests1,Covered1,Fails1),
	merge_tests(Tests0,Tests1,Tests),
	glb_covering(Covered0,Covered1,Covered),
	glb_nonfailure(Fails0,Fails1,Fails),
	asub(Glb,Tests,Covered,Fails).

glb_covering(not_covered,not_covered,not_covered):- !.
glb_covering(_,_,covered).

glb_nonfailure(possibly_fails,possibly_fails,possibly_fails):- !.
glb_nonfailure(_,_,not_fails).

%------------------------------------------------------------------------%
% det_less_or_equal(+,+)                                                  %
% det_less_or_equal(ASub0,ASub1)                                          %
%------------------------------------------------------------------------%

det_less_or_equal(ASub0,ASub1):-
	asub(ASub0,_Tests0,Covered0,Fails0),
	asub(ASub1,_Tests1,Covered1,Fails1),
	le_covering(Covered0,Covered1),
	le_nonfailure(Fails0,Fails1).

le_covering(covered,not_covered).
le_covering(covered,covered).
le_covering(not_covered,not_covered).

le_nonfailure(not_fails,possibly_fails).
le_nonfailure(possibly_fails,possibly_fails).
le_nonfailure(not_fails,not_fails).

%------------------------------------------------------------------------%
% det_identical_abstract(+,+)                                             %
% det_identical_abstract(ASub1,ASub2)                                     %
%------------------------------------------------------------------------%

det_identical_abstract(ASub0,ASub1):-
	asub(ASub0,_Tests0,Covered,Fails),
	asub(ASub1,_Tests1,Covered,Fails).

%------------------------------------------------------------------------%
% det_sort(+,-)                                                           %
% det_sort(ASub0,ASub1)                                                   %
%------------------------------------------------------------------------%

det_sort(ASub,ASub).

%------------------------------------------------------------------------%
% det_call_to_success_fact(+,+,+,+,+,+,-,-)                               %
% det_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ)            %
%-------------------------------------------------------------------------

det_call_to_success_fact(_Sg,_Hv,_Head,_Sv,Call,Proj,Prime,Succ):-
	Succ = Call,
	Prime = Proj.

%-------------------------------------------------------------------------
% det_special_builtin(+)                                                  |
% det_special_builtin(SgKey)                                              |
%-------------------------------------------------------------------------

det_special_builtin(SgKey):-
   det_builtin(SgKey, _Sg, _CallType, _BType, _BSg, _CovNF).                          

% det_builtin('!/0',   Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('!/0',  Sg, _CallType,  meta, Sg, _CovNf):-!.
% det_builtin('cut/0',  Sg, _CallType,  meta, Sg, _CovNf):-!.
% det_builtin('=/2'   , Sg, _CallType, unif, Sg, _CovNf):-!.
det_builtin('=/2'   , Sg, _CallType, aritunif, Sg, _CovNf):-!.
det_builtin('==/2'  , Sg, _CallType, unif, Sg, _CovNf):-!.
det_builtin('\==/2' , Sg, _CallType, unif, Sg, _CovNf):-!. 
% 
det_builtin('is/2', X is E, CallType, notest, X is E, CovNf):-
     is_free_var(X, CallType),
     !, 
     det_builtin_trust(CovNf, covered, not_fails).
det_builtin('is/2', X is E, _CallType, arit, =:=(X, E), _CovNf):-!.
% Arithmetic tests
det_builtin('=:=/2', Sg, _CallType, arit, Sg, _CovNf):-!. 
det_builtin('=\=/2', Sg, _CallType, arit, Sg, _CovNf):-!. 
det_builtin('</2',   Sg, _CallType, arit, Sg, _CovNf):-!. 
det_builtin('>/2',   Sg, _CallType, arit, Sg, _CovNf):-!. 
det_builtin('=</2',  Sg, _CallType, arit, Sg, _CovNf):-!. 
det_builtin('>=/2',  Sg, _CallType, arit, Sg, _CovNf):-!. 
% For Sicstus
det_builtin('number/1',  Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('integer/1', Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('atom/1',    Sg, _CallType, meta, Sg, _CovNf):-!. 
% For CIAO
det_builtin('num/1',     Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('int/1',     Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('atm/1',     Sg, _CallType, meta, Sg, _CovNf):-!. 
% Meta-tests
det_builtin('var/1',     Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('nonvar/1',  Sg, _CallType, meta, Sg, _CovNf):-!. 
% For Sicstus
det_builtin('ground/1',  Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('float/1',   Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('ground/1',  Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('atomic/1',  Sg, _CallType, meta, Sg, _CovNf):-!. 
% For CIAO
det_builtin('gnd/1',     Sg, _CallType, meta, Sg, _CovNf):-!. 
det_builtin('flt/1',     Sg, _CallType, meta, Sg, _CovNf):-!. 
% Sometimes may act as tests and sometimes succeed. 
det_builtin('get_code/1', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, covered, not_fails).
det_builtin('get_code/1', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, covered, not_fails).
det_builtin('current_op/3', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, covered, not_fails).
det_builtin('functor/3', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, covered, not_fails).
det_builtin('findall/3', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, covered, not_fails).
det_builtin('arg/3', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, covered, not_fails).
% No tests that always succeeds.
det_builtin('true/0',    Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('nl/0',      Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('ttynl/0',   Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('ttyput/1',  Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('write/1',   Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('tab/1',     Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('writeq/1',  Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('display/1', Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('print/1',   Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('check/1',   Sg, _CallType, id, Sg, _CovNf):-!.
det_builtin('\+/1', Sg, _CallType, negation, Sg, _CovNf):-!.
det_builtin('fail/0', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, not_covered, possibly_fails).
det_builtin('false/0', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, not_covered, possibly_fails).
det_builtin('indep/1', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, not_covered, possibly_fails).
det_builtin('indep/2', Sg, _CallType, notest, Sg, CovNf):- 
     !, 
     det_builtin_trust(CovNf, not_covered, possibly_fails).

is_free_var(X, CallType):-
     nonvar(CallType),
     shfr_obtain(free,[X],CallType,[Y]),
     X == Y.

%-------------------------------------------------------------------------
% det_success_builtin(+,+,+,+,-)                                          |
% det_success_builtin(Type,Sv_u,Condv,Call,Succ)                          |
%-------------------------------------------------------------------------
% Accumulates the tests:

det_success_builtin(SgKey, CallType,Sg,Call,Succ):-
   det_builtin(SgKey,Sg,CallType,BType,BSg,CovNF),
   det_success_builtin_(BType,CallType,CovNF,BSg,Call,Succ).
     
det_success_builtin_(id, _CallType, _CovNF, _BSg, Call, Succ):-  
        % For efficiency (builtins that do not fail).
        !, 
        Succ=Call.
det_success_builtin_(notest, _CallType, CovNF, _BSg, Call, Succ):-
        !,
        builtin_trust_to_succ(CovNF,Call,Succ).
det_success_builtin_(negation, CallType, _CovNF, BSg, Call, Succ):-
        remove_negation(BSg, NSg),
        functor(NSg, F, A),
        make_atom([F,A], NSgkey),
        det_builtin(NSgkey, NSg, CallType, BType, _S, _C),
        (is_a_test(BType) ->
             push_neg_in_test(BSg, NBSg), 
             det_success_test(BType, NBSg, Call, Succ)
             ; 
             det_success_negation(Call, Succ)).
det_success_builtin_(meta, _CallType, _CovNF, BSg, Call, Succ):- % cut
        det_success_test(meta, BSg, Call, Succ).
det_success_builtin_(BType, _CallType, _CovNF, BSg, Call, Succ):-
        is_a_test(BType),
        translate_test(BSg, NewBSg),
        det_success_test(BType, NewBSg, Call, Succ).

det_success_test(BType, BSg, Call, Succ):-
	asub(Call,Tests0,Covered,Fails),
	tests(Tests0,InVars,Unif0,Arith0,Meta0),
	add_test(BType,BSg,Unif0,Arith0,Meta0,Unif,Arith,Meta),
	tests(Tests,InVars,Unif,Arith,Meta),
	asub(Succ,Tests,Covered,Fails).

det_success_negation(Call, Succ):-
	asub(Call,Tests,Covered,_Fails),
	asub(Succ,Tests,Covered,possibly_fails).


add_test(unif,Sg,Unif,Arith,Meta,[Sg|Unif],Arith,Meta).
add_test(arit,Sg,Unif,Arith,Meta,Unif,[Sg|Arith],Meta).
add_test(aritunif,Sg,Unif,Arith,Meta,[Sg|Unif],[Sg|Arith],Meta).
add_test(meta,Sg,Unif,Arith,Meta,Unif,Arith,[Sg|Meta]).

is_a_test(Btype):- Btype == unif.
is_a_test(Btype):- Btype == arit.
is_a_test(Btype):- Btype == aritunif.
is_a_test(Btype):- Btype == meta.

builtin_trust_to_succ(CovNF,Call,Succ):-
  	asub(Call,Tests,Covered1,Fails1),
        det_builtin_trust(CovNF, Covered0, Fails0),   
	glb_covering(Covered0,Covered1,Covered),
	glb_nonfailure_1(Fails0,Fails1,Fails),
	asub(Succ,Tests,Covered,Fails).
 
det_builtin_trust((Covered, Fails), Covered, Fails).   

%-------------------------------------------------------------------------
% det_call_to_success_builtin(+,+,+,+,+,-)                                %
% det_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ)                 %
%-------------------------------------------------------------------------
% Not used

%------------------------------------------------------------------------%
% det_input_interface(+,+,+,-)                                            %
% det_input_interface(InputUser,Kind,StructI,StructO)                     %
%------------------------------------------------------------------------%
% Something more intelligent should be done with the argument of the props
% than simply ignore them!!!

det_input_interface(Prop,Kind,SI,SO):-
	functor(Prop,P,1),
        internal_value_to_native(Internal,P),
	det_input_interface_(Internal,Kind,SI,SO).

det_input_interface_(not_fails,perfect,(Cov,Fail0),(Cov,Fail1)):-
	myappend(Fail0,not_fails,Fail1).
det_input_interface_(possibly_fails,perfect,(Cov,Fail0),(Cov,Fail1)):-
	myappend(Fail0,possibly_fails,Fail1).
det_input_interface_(covered,perfect,(Cov0,Fail),(Cov1,Fail)):-
	myappend(Cov0,covered,Cov1).
det_input_interface_(not_covered,perfect,(Cov0,Fail),(Cov1,Fail)):-
	myappend(Cov0,not_covered,Cov1).

myappend(Vs,V0,V):-
	var(Vs), !,
	V=V0.
myappend(Vs,V0,V):-
	merge(Vs,V0,V).

may_be_var(X,X):- ( X=[] ; true ), !.

%------------------------------------------------------------------------%
% det_input_user_interface(+,+,-)                                         %
% det_input_user_interface(InputUser,Qv,ASub)                             %
%------------------------------------------------------------------------%

det_input_user_interface((Cov0,Fail0),Qv,ASub):-
	may_be_var(Cov0,Cov1),
	may_be_var(Fail0,Fail1),
	foldr( Cov1, covered, glb_covering, Covered ),
	foldr( Fail1, not_fails, glb_nonfailure, Fails ),
	det_empty_entry(Qv,Entry),
	asub(Entry,Tests,_Covered,_Fails),
	asub(ASub,Tests,Covered,Fails).

%------------------------------------------------------------------------%
% det_asub_to_native(+,+,-)                                               %
% det_asub_to_native(ASub,Qv,ASub_user)                                   %
%------------------------------------------------------------------------%
% Qv should be the goal for comp-props!!!!!
% something has to be done to put the props in the comp part, not the success
% part of the assertion!!!

det_asub_to_native(ASub,Qv,[PropF,PropC]):-
	asub(ASub,_Tests,Covered,Fails),
        internal_value_to_native(Covered, NatDetCovered),
        internal_value_to_native(Fails, NatDetFails),
	functor(PropF,NatDetFails,1),
	functor(PropC,NatDetCovered,1),
	arg(1,PropF,Qv),
	arg(1,PropC,Qv).

internal_value_to_native(not_fails,is_det).
internal_value_to_native(possibly_fails,non_det).
internal_value_to_native(covered,mut_exclusive).
internal_value_to_native(not_covered,not_mut_exclusive).

%------------------------------------------------------------------------%
% det_unknown_call(+,+,-)                                                 %
% det_unknown_call(Vars,Call,Succ)                                        %
%------------------------------------------------------------------------%

det_unknown_call(_Vars,Call,Succ):-
	asub(Call,Tests,_,_),
	asub(Succ,Tests,not_covered,possibly_fails).

%------------------------------------------------------------------------%
% det_unknown_entry(+,-)                                                  %
% det_unknown_entry(Vars,Entry)                                           %
%------------------------------------------------------------------------%

det_unknown_entry(Vars,Entry):-
	det_empty_entry(Vars,Entry).

%------------------------------------------------------------------------%
% det_empty_entry(+,-)                                                    %
% det_empty_entry(Vars,Entry)                                             %
%------------------------------------------------------------------------%

det_empty_entry(Vars,Entry):-
	tests(Tests,Vars,[],[],[]),
	asub(Entry,Tests,covered,not_fails).

%-----------------------------------------------------------------------

foldr([], B, _F, B).
foldr([A|As], B, F, R):-
	foldr(As, B, F, R1),
	call(F, A, R1, R).

%-----------------------------------------------------------------------

det_statistics([detstatistics([total_preds(Total),
                preds_det(NF_Preds),
                preds_mutex(Cov_Preds),
                preds_some_variant_det(NF_Variants),
                preds_some_variant_mutex(Cov_Variants)])]):-
nf_det_statistics(det, _S, Total, NF_Preds, Cov_Preds, NF_Variants, Cov_Variants).

%-----------------------------------------------------------------------

det_obtain(_Prop,_Vars,ASub,Info):- 
           ASub = nf(_Types, _Modes, nf(_Tests,Covered,Fails)),
           internal_value_to_native(Covered,Mutex),
           internal_value_to_native(Fails,Det),
           Info = [Mutex,Det].
