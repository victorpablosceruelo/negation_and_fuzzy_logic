:- module(aiakl_uudg_main,
        [
            aiakl_test/3
        ], []).


:- use_package(andprolog_nd).
:- use_module(library(lists), [append/3]).
:- use_module(library(sort)).
:- use_module('./common_bench').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aiakl_test(N, Reps, Name/Arity) :-
	prepare(N, L1, L2),
        functor(Goal, Name, Arity),
        arg(1, Goal, L1),
	arg(2, Goal, L2),
        bench_rep(Reps, Goal, Name/Arity).

init_vars_seq(E1,E2,E1init,E2init) :-
	find_all_vars2(E1,Vars11),
	sort(Vars11,Vars1),
	find_all_vars2(E2,Vars22),
	sort(Vars22,Vars2),
	intersect(Vars1,Vars2,_X,Notin1,Notin2),
	init_vars3(Notin1,E1,Einit11),
	sort(Einit11,E1init),
	init_vars3(Notin2,E2,Einit22),
	sort(Einit22,E2init).

init_vars_mel(E1,E2,E1init,E2init) :-
        find_all_vars2_par(E1,Vars11),
        sort(Vars11,Vars1) '&!'
        find_all_vars2_par(E2,Vars22),
        sort(Vars22,Vars2),
        intersect(Vars1,Vars2,_X,Notin1,Notin2),
        init_vars3(Notin1,E1,Einit11),
        sort(Einit11,E1init),
        init_vars3(Notin2,E2,Einit22),
        sort(Einit22,E2init) .

init_vars_udg(E1,E2,E1init,E2init) :-
        (
         find_all_vars2_par(E2,Vars22),
         sort(Vars22,Vars2)
        ) '&!'
        (
         find_all_vars2_par(E1,Vars11),
         sort(Vars11,Vars1)
        ),
        intersect(Vars1,Vars2,_X,Notin1,Notin2),
        init_vars3(Notin1,E1,Einit11),
        sort(Einit11,E1init),
        init_vars3(Notin2,E2,Einit22),
        sort(Einit22,E2init) .

init_vars_uoudg(E1,E2,E1init,E2init) :-
        find_all_vars2_par(E1,Vars11),
        sort(Vars11,Vars1)'&!>' H2,
        find_all_vars2_par(E2,Vars22),
        sort(Vars22,Vars2),
        H2 '<&!',
        intersect(Vars1,Vars2,_X,Notin1,Notin2),
        init_vars3(Notin1,E1,Einit11),
        sort(Einit11,E1init),
        init_vars3(Notin2,E2,Einit22),
        sort(Einit22,E2init) .

init_vars_uudg(E1,E2,E1init,E2init) :-
        find_all_vars2_par(E2,Vars22)'&!>' H3,
        find_all_vars2_par(E1,Vars11),
        sort(Vars11,Vars1)'&!>' H2,
        H3 '<&!',
        sort(Vars22,Vars2),
        H2 '<&!',
        intersect(Vars1,Vars2,_X,Notin1,Notin2),
        init_vars3(Notin1,E1,Einit11),
        sort(Einit11,E1init),
        init_vars3(Notin2,E2,Einit22),
        sort(Einit22,E2init) .

find_all_vars2([],[]).
find_all_vars2([Vars=_Values|Es],AllVars) :-
	find_all_vars2(Es,AllVars1),
	append(Vars,AllVars1,AllVars).

find_all_vars2_par([],[]).
find_all_vars2_par([Vars=_Values|Es],AllVars) :-
	append(Vars,AllVars1,AllVars) '&!'
	find_all_vars2_par(Es,AllVars1).

init_vars3([],E,E).
init_vars3([Var|Vars],E,[[Var]=[unbound]|Es]) :-
        init_vars3(Vars,E,Es) .

intersect(As,[],[],[],As) :- !.
intersect([],Bs,[],Bs,[]) :- !.
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        A=B,
        !,
        Cs=[A|Cs2],
        intersect(As,Bs,Cs2,Ds,Es) .
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        A@<B,
        !,
        Es=[A|Es2],
        intersect(As,[B|Bs],Cs,Ds,Es2) .
intersect([A|As],[B|Bs],Cs,Ds,Es) :-
        Ds=[B|Ds2],
        intersect([A|As],Bs,Cs,Ds2,Es) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare(N0,LList1,LList2) :- 
        N is N0*5,
        subst(SList1,SList2),
        mlist(N,SList1,LList1),
        mlist(N,SList2,LList2).

subst(E1,E2):- 
        E1 = [X = [a], X = [a],X = [a], X = [a]],
        E2 = [Y = [a], Y = [a],Y = [a], Y = [a]],
        X = [5,7,8,3,2,4,1,6,9,15,17,18,13,12,14,11,16,19,25,27,28,23,22,24,
             21,26,29],
        Y = [15,17,18,13,12,14,11,16,19,35,37,38,33,32,34,5,7,8,3,2,4,1,6,9,
             31,36,39].

mlist(0,_,[]).
mlist(X,SList,LList) :- 
        X>0,
        Y is X-1,
        mlist(Y,SList,MList),
        append(SList,MList,LList).

