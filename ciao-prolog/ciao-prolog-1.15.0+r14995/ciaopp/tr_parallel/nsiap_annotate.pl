:- module(nsiap_annotate,
	[
 	    urlp_annotate/5,
 	    urlp_annotate_clp/6,
 	    crlp_annotate/5,
%	    crlp_annotate_clp/5,
	    compute_indep0/4
	],
	[]).

:- use_module(library(lists), 
	[
	    append/3,
	    length/2,
	    dlist/3,
	    insert_last/3
	]).

:- use_module(annotate, 
	[
	    condition/7,
	    vertex/4,
	    vertex_info/4
	]).

:- use_module(library(aggregates), [findall/3]).

:- use_module(library(vndict), [find_name/4]).

:- use_module(library(sets), 
	[
	    ord_intersection/3,
	    ord_member/2,
	    ord_subtract/3,
	    ord_union/3,
	    ord_union_change/3,
	    ord_disjoint/2, 
	    ord_intersect/2,
	    ord_subset/2
	]).

:- use_module(global_info, [translate_nodes/3]).

:- push_prolog_flag(multi_arity_warnings,off).

:- op( 950, xfy,[(&),(\&)]).


% progvar <= _.
% vars    <= ord_set(progvar). /* Ordered list without duplicates */
% frset   <= vars.
% shset   <= vars.
% sh_abs  <= ord_set(shset).
% fr_abs  <= frset.
% shfr_abs <= shfr(sh_abs,fr_abs).
% goal_index <= integer.
% goal_node  <= node(goal_index, vars, shfr_abs, shfr_abs).
% nsi_l <= list(nsi(goal_index, goal_index)).
% goal_dict <= {Functor/Arity}(goal).

urlp_annotate(EntryGoals, Dict, GoalDict, Exp, NewDict) :-
        translate_nodes(EntryGoals, Goals, _),
        Dict = dic(Vs,_),
        copy_term([Vs|Goals],[NVs|Goals_copy]),
        count_vars(NVs, 1), % substitute vars in Goals_copy for numbers
        compute_indep(Goals_copy, Nsi_l),
        make_par_exp(Goals_copy, Nsi_l, ListExp),
        seq_exp(ListExp, Exp1),
        ren_subst_goals(Exp1, Goals, GoalDict, Exp, Dict, NewDict).

crlp_annotate(EntryGoals, Dict, GoalDict, Exp, NewDict) :-
        translate_nodes(EntryGoals, Goals, _),
        Dict = dic(Vs,_),
        copy_term([Vs|Goals],[NVs|Goals_copy]),
        count_vars(NVs, 1), % substitute vars in Goals_copy for numbers
        compute_indep(Goals_copy, Nsi_l),
        make_par_exp(Goals_copy, Nsi_l, ListExp),
        put_tests(ListExp, Exp1, rename(NVs,Vs)),
        ren_subst_goals(Exp1, Goals, GoalDict, Exp, Dict, NewDict).

count_vars([], _).
count_vars([N|Vs], N) :-
        N1 is N+1,
        count_vars(Vs,N1).

% Begin check this part
% ---------------------
urlp_annotate_clp(EntryGoals,Dict,Mode,GoalDict,Exp,NewDict):-
        translate_nodes_and_compute_dep(EntryGoals,Goals,Mode,Nsi_l,[]),
        make_par_exp(Goals, Nsi_l, ListExp),
        ren_subst_goals(ListExp, Goals, GoalDict, RListExp, Dict, NewDict),
        seq_exp(RListExp, Exp).

translate_nodes_and_compute_dep([],[],_Mode,Nsi,Nsi).
translate_nodes_and_compute_dep([N],[G],_Mode,TNsi,TNsi):-
	translate_node_void(N,G).
translate_nodes_and_compute_dep([X,Y|Rest],[GX|Gs],Mode,Nsi,TNsi):-
	translate_node_void(X,GX),
	translate_nodes_and_compute_dep0([Y|Rest],X,Mode,Nsi,Nsi1),
	translate_nodes_and_compute_dep([Y|Rest],Gs,Mode,Nsi1,TNsi).

translate_nodes_and_compute_dep0([Y,Z|Rest],X,Mode,Nsi,TNsi):- !,
	translate_nodes_and_compute_dep0([Y],X,Mode,Nsi,Nsi1),
	translate_nodes_and_compute_dep0([Z|Rest],X,Mode,Nsi1,TNsi).
translate_nodes_and_compute_dep0([Y],X,Mode,Nsi,Tail):-
	vertex(X,XVars,InfoX,VX),
	vertex(Y,YVars,InfoY,VY),
	condition(XVars,YVars,Mode,InfoX,InfoY,nsiap,Conds),
	nsi_if_no_cond(Conds,VX,VY,Nsi,Tail).

nsi_if_no_cond(conds([],[],_),VX,VY,[nsi(IdX,IdY)|Nsi],Nsi):-
	vertex_info(VX,IdX,_,Info),
	vertex_info(VY,IdY,_,Info).
nsi_if_no_cond(_Cond,_VX,_VY,Nsi,Nsi).

translate_node_void(node(Num, Vars, _Granul, _Info),
                    node(Num, Vars, _Void, _Void)).
% ---------------------
% End check this part

% type goal_node(+goal_node).

goal_node(node(_,_,_,_)).

/****************************************************************************/
/*                              NSI checker                                 */
/****************************************************************************/

% joinset  <= ord_set(shset).

% compute_indep(+Gs, -Nsi_l) :- Nsi_l contains the pairs of
%   goal indexes found NSI in the list of goal nodes Gs.
% type compute_indep(list(goal_node), nsi_l).

compute_indep([G1|Gs], Nsi_l) :-
        Gs = [_|_], !,
        compute_indep0(G1, Gs, Nsi_l, Nsi_l_),
        compute_indep(Gs, Nsi_l_).
compute_indep([_], []).

compute_indep0(G1, Gs, Nsi_l, Nsi_l_):-
% change shfr by mshare
        G1 = node(Goal_p, Var_p, mshare(B_sh, B_fr), mshare(P_sh, P_fr)),
        compute_S_p(B_sh, B_fr, Var_p, S_p, S_p_fr),
        compute_join_sets(P_sh, Var_p, S_p, S_p_fr, JS),
        compute_indep(Gs, Goal_p, S_p, P_fr, JS, Nsi_l, Nsi_l_).

% compute_S_p(+B_sh, +B_fr, +Var_p, -S_p, -Sp_p_fr) :-
%   S_p = {L in B_sh | L /\ Var_p =/= 0},
%   S_p_fr = {L /\ B_fr | L in S_p}, and the elements of S_p_fr
%   correspond to those of S_p in the same position.
% type compute_S_p(sh_abs, fr_abs, vars, list(shset), list(frset)).

compute_S_p([], _, _, [], []).
compute_S_p([L|Ls], B_fr, Var_p, S_p, S_p_fr) :-
        ord_intersect(L, Var_p) ->
            S_p = [L|_S_p],
            ord_intersection(L, B_fr, L_fr),
            S_p_fr = [L_fr|_S_p_fr],
            compute_S_p(Ls, B_fr, Var_p, _S_p, _S_p_fr) ;
        compute_S_p(Ls, B_fr, Var_p, S_p, S_p_fr).

% compute_join_sets(+P_sh, +Var_p, +S_p, +S_p_fr, -JS) :-
%   JS = {Ps(L) | L in P_sh, L /\ Var_p =/= 0},
%   Ps(L) = { P | P =< S_p, L = \/ (N in P),
%             there not exists P' =< P such that L = \/ (N in P') and
%             for all different N1, N2 in P . N1 /\ N2 /\ B_fr = 0 }.
% type compute_join_sets(sh_abs, vars, list(shset), list(frset),
%                        list(list(joinset))).

compute_join_sets([], _, _, _, []).
compute_join_sets([L|Ls], Var_p, S_p, S_p_fr, JS) :-
        ord_intersect(L, Var_p) ->
            findall(P, join_set(S_p, S_p_fr, L, [], P), Ps),
            JS = [Ps|_JS],
            compute_join_sets(Ls, Var_p, S_p, S_p_fr, _JS) ;
        compute_join_sets(Ls, Var_p, S_p, S_p_fr, JS).

% nondeterminist join_set(+S_p, +S_p_fr, +L, +U_N, -P).
% type join_set(list(shset), list(frset), shset, shset, joinset).

% In the following clause == is used because of free variables in L/U_N
join_set(_, _, L, U_N, P) :- L == U_N, !, P = [].
join_set([N|Ns], [N_fr|N_frs], L, U_N, P) :-
        ord_subset(N, L),
        ord_disjoint(N_fr, U_N),
        ord_union_change(N, U_N, New_U_N), % New_U_N =/= U_N
        P = [N|_P],
        join_set(Ns, N_frs, L, New_U_N, _P).

join_set([_|Ns], [_|N_frs], L, U_N, P) :-
        join_set(Ns, N_frs, L, U_N, P).


% compute_indep(+Gs, +Goal_p, +S_p, +P_fr, +JS, -Nsi_l, -Nsi_l_) :-
%   Adds nsi(Goal_p, Goal_q) to the diff list (Nsi_l, Nsi_l_) for
%   each Goal_q in Gs found NSI from Goal_p.
% type compute_indep(list(goal_node), goal_index, list(shset), fr_abs,
%                    list(list(joinset)), nsi_l, nsi_l).

compute_indep([], _, _, _, _, Nsi_l_, Nsi_l_).
compute_indep([N_q|Gs], Goal_p, S_p, P_fr, JS, Nsi_l, Nsi_l_) :-
        N_q = node(Goal_q, Var_q, _,_),
        check_C1(S_p, P_fr, Var_q),
        check_C2(JS, Var_q), !,
        Nsi_l = [nsi(Goal_p, Goal_q)|Nsi_l0],
        compute_indep(Gs, Goal_p, S_p, P_fr, JS, Nsi_l0, Nsi_l_).
compute_indep([_|Gs], Goal_p, S_p, P_fr, JS, Nsi_l, Nsi_l_) :-
        compute_indep(Gs, Goal_p, S_p, P_fr, JS, Nsi_l, Nsi_l_).

% check_C1(+S_p, +P_fr, +Var_q) :-
%   Checks that condition C1 for non-strict independence hold.
% type check_C1(list(shset), fr_abs, vars).

check_C1([], _, _).
check_C1([L|Ls], P_fr, Var_q) :-
        check_C1_L(L, P_fr, Var_q), !,
        check_C1(Ls, P_fr, Var_q).

% check_C1_L(+L, +P_fr, +Var_q) :-
%   If L is in SH, checks that L intersects with P_fr.
check_C1_L(L,_P_fr, Var_q) :- ord_intersection(L, Var_q, []). % L not in SH
check_C1_L(L, P_fr,_Var_q) :- ord_intersect(L, P_fr).         % Condition C1

% check_C2(+JS, +Var_q) :- 
%   Checks that condition C2 for non-strict independence hold.
% type check_C2(list(list(joinset)), vars).

check_C2([], _).
check_C2([Ps|_JS], Var_q) :-
        check_join_sets(Ps, Var_q),
        check_C2(_JS, Var_q).

% check_join_sets(+Ps, +Var_q).
% type check_join_sets(list(joinset), vars).

check_join_sets([], _).
check_join_sets([P|Ps], Var_q) :-
        check_join_set(P, Var_q, 0),
        check_join_sets(Ps, Var_q).

% check_join_set(+P, +Var_q, +S) :- The number of sharing sets in P that
%   intersect with Var_q (thus they are of SH) is less than two, S is
%   the number of the already found.

check_join_set([], _, _).
check_join_set([N|Ns], Var_q, S) :-
        ord_intersect(N, Var_q) ->
            S = 0, % if S = 1 we already have found two
            check_join_set(Ns, Var_q, 1) ;
        check_join_set(Ns, Var_q, S).

/****************************************************************************/
/*                             URLP Annotator                               */
/****************************************************************************/

% list_exp <= list(par_list).
% par_list <= goal_node \/ list2(seq_list).
% seq_list <= goal_node \/ list2(par_list).
% list2(X) <= [X, X|list(X)]. % List of at least 2 elements
% % Some examples:
% % a, b & (c, d), e => [a, [b, [c, d]], e]
% % a & (b,c) => [[a, [b, c]]]


% type make_par_exp(+list_exp, +nsi_l, -list_exp).
% % Entry point: parallelize a body clause, given as a list of goal
% % nodes, returning a list expression in the formalism above.

make_par_exp([A], _Nsi_l, [A]) :- !.

make_par_exp([A,B|R], Nsi_l, P) :- 
        goal_node(A),
        goal_node(B), !,
        (
            are_nsi(A, B, Nsi_l) ->
                make_par_exp([[A,B]|R], Nsi_l, P) ;
            make_par_cont(A, B, R, Nsi_l, P)
        ).

make_par_exp([As, B|R], Nsi_l, P) :-
        % As has 2 or more elements
        goal_node(B), !,
        which_nsi(As, B, Nsi_l, AP, AN),
        (
            AN = [] ->
                make_par_exp([[B|As]|R], Nsi_l, P) ;
            AN = [X] ->
                append_goals(X, [B], X_B),
                make_par_exp(X_B, Nsi_l, NX_B),
                make_par_exp([[NX_B|AP]|R], Nsi_l, P) ;
            AP \== [] -> % AN \== As
                make_par_exp([[[AN, B]|AP]|R], Nsi_l, P) ;
            make_par_cont(As, B, R, Nsi_l, P)
        ).

make_par_exp([A, Bs|R], Nsi_l, P) :-
        % Bs has 2 or more elements
        % A can be a single goal or a parallel expression
        which_nsi_rev(Bs, A, Nsi_l, BP, BN),
        (
            BP \== [] -> % BN cannot be empty
                conc_goals(A, BN, S), % this is to keep the list2 property
                make_par_exp([[S|BP]|R], Nsi_l, P) ;
            make_par_cont(A, Bs, R, Nsi_l, P)
        ).


% type make_par_cont(+par_list, +par_list, +list_exp, +nsi_l, -list_exp).

make_par_cont(A, B, R, Nsi_l, P) :-
        make_par_exp([B|R], Nsi_l, P1),
        (
            P1 = [B|_] ->
                P = [A|P1] ;
            make_par_exp([A|P1], Nsi_l, P)
        ).


% type which_nsi(+par_list, +goal_node, +nsi_l, -par_list, -par_list).
% % which_nsi(PA, B, IA, DA) : given the parallel expression PA and the
% %   goal B, IA (DA) is the parallel expression formed with the elements
% %   of PA from which B is independent (dependent). 

which_nsi([], _, _, [], []).
which_nsi([A|As], B, Nsi_l, AP, AN) :-
        (
            are_nsi(A, B, Nsi_l) ->
                AP = [A|AP1], AN = AN1 ;
            AN = [A|AN1], AP = AP1
        ),
        which_nsi(As, B, Nsi_l, AP1, AN1).


% type which_nsi_rev(+par_list, +par_list, +nsi_l, -par_list, -par_list).
% % which_nsi_rev(PB, PA, IB, DB) : given the parallel expression PB and
% %   the goal or parallel expression PA, IB (DB) is the parallel expression
% %   formed with the elements of PB which are independent (dependent) from
% %   PA.

which_nsi_rev([], _, _, [], []).
which_nsi_rev([B|Bs], A, Nsi_l, BP, BN) :-
        (
            are_nsi(A, B, Nsi_l) ->
                BP = [B|BP1], BN = BN1 ;
            BN = [B|BN1], BP = BP1
        ),
        which_nsi_rev(Bs, A, Nsi_l, BP1, BN1).


% type are_nsi(+seq_list, +seq_list, +nsi_l)
% % are_nsi(PA, PB, Nsi_l) : parallel expression PB is independent from PA. 

are_nsi([], _, _) :- !.
are_nsi([A|B], C, Nsi_l) :- !,
        are_nsi(A, C, Nsi_l),
        are_nsi(B, C, Nsi_l).
are_nsi(A, [B|C], Nsi_l) :- !,
        are_nsi(A, B, Nsi_l),
        are_nsi(A, C, Nsi_l).
are_nsi(_, [], _) :- !.
are_nsi(A, B, Nsi_l) :-
        arg(1, A, A_i), % goal index
        arg(1, B, B_i), % goal index
        member(nsi(A_i,B_i), Nsi_l).


% type append_goals(+seq_list, +list(par_list), -list(par_list)).
% % append_goals(S, P, SP) : SP is the sequence of S and P.

append_goals(E, L, [E|L]) :- goal_node(E), !.
append_goals(L1,L2,L3) :- append(L1,L2,L3).

% type conc_goals(+par_list, +list(seq_list), -list(par_list)).
% % conc_goals(P, S, PS) : PS is the sequence of P and S.

conc_goals(A, BN, S) :-
        BN = [X] ->
            (
                goal_node(X) ->
                    S = [A,X] ;
                S = [A|X]
            );
        S = [A, BN].


% % This formalizes an &-Prolog parallel expression
% goal_par_exp <= seq_exp \/ par_exp.
% seq_exp <= goal \/ ( (goal \/ par_exp), goal_par_exp ).
% par_exp <= (seq_exp & goal_par_exp).

% type seq_exp(+seq_list, -goal_par_exp).
% % seq_exp(L, E) : E is the &-Prolog parallel expression equivalent to
% %   the list expression L.

seq_exp([X], P) :- !,
        par_exp(X, P).
seq_exp([X|Xs], (P, Ps)) :- !,
        % Xs \== []
        par_exp(X, P),
        seq_exp(Xs, Ps).
seq_exp(G, G).

% type par_exp(+par_list, -goal_par_exp).

par_exp([X], P) :- !,
        seq_exp(X, P).
par_exp([X|Xs], (P & Ps)) :- !,
        % Xs \== []
        seq_exp(X, P),
        par_exp(Xs, Ps).
par_exp(G, G).

/****************************************************************************/
/*                        Conditional parallelism                           */
/****************************************************************************/

% Entry point
put_tests([X1|Xs], E, Ren) :-
        put_tests(Xs, X1, [], E, Ren).

put_tests(X, Avs, E, Ren) :-
        X = [X1|Xs] ->
            put_tests(Xs, X1, Avs, E, Ren)
      ; E = X.

put_tests([], X1, Avs, E, Ren) :-
        put_tests_par(X1, Avs, E, Ren).
put_tests([X2|Xs], X1, Avs, E, Ren) :-
        put_tests(Xs, X1, X2, Avs, E, Ren).

put_tests([], X1, X2, Avs, E, Ren) :-
% nsi_test fails unless X1 and X2 are simple goals
        (   nsi_test(X1, X2, Avs, Ren, C, _) ->
                (   C = true ->
                        E = (X1 & X2)
                ;   E = (C -> X1 & X2 ; X1, X2)
                )
        ;   E = (E1, E2),
            put_tests_par(X1, Avs, E1, Ren),
            put_tests_par(X2, Avs, E2, Ren)
        ).
put_tests([X3|Xs], X1, X2, Avs, E, Ren) :-
        (   nsi_test(X1, X2, Avs, Ren, C, Avs1) ->
                (   C = true ->
                        E = ((X1 & X2), Ey_)
                ;   E = (C -> X1 & X2, Ey_ ; X1, En_),
                    put_tests(Xs, X2, X3, Avs, En_, Ren)
                ),
                put_tests(Xs, X3, Avs1, Ey_, Ren)
        ;   E = (E1, E_),
            put_tests_par(X1, Avs, E1, Ren),
            put_tests(Xs, X2, X3, Avs, E_, Ren)
        ).

put_tests_par(X, Avs, E, Ren) :-
        X = [X1|Xs] ->
            put_tests_par(Xs, X1, Avs, E, Ren)
      ; E = X.

put_tests_par([], X1, Avs, E, Ren) :-
        put_tests(X1, Avs, E, Ren).
put_tests_par([X2|Xs], X1, Avs, (E1 & E), Ren) :-
        put_tests(X1, Avs, E1, Ren),
        put_tests_par(Xs, X2, Avs, E, Ren).

% nsi_test(G1, G2, Avs, C, Avs_out): C is a test to ensure that G1 and G2
%   are nsi, having into account that the 'allvars/2' tests Avs have
%   succeeded, and Avs_out is Avs plus the 'allvars/2' tests in C.

nsi_test(G1, G2, Avs, Ren, C, Avs_out) :-
% change shfr by mshare
        G1 = node(_I_p, Var_p, mshare(B_sh, B_fr), mshare(P_sh, P_fr)),
        G2 = node(_I_q, Var_q, _, _),
        tests_sieve(Avs, B_sh, B_sh1),
        compute_SH_illegal(B_sh1, Var_p, Var_q, P_fr, SH_ill, B_sh2),
        valid_situation(B_fr, B_sh2),
        compute_tests1(SH_ill, B_sh2, Var_p, Var_q, P_fr, LC, LC2),
        compute_tests2(P_sh, Var_p, Var_q, B_fr, B_sh2, B_sh_, LC2),
        valid_situation(B_fr, B_sh_),
        make_test(LC, Avs, Ren, C, Avs_out).

tests_sieve([], Ls, Ls).
tests_sieve([Av|Avs], Ls0, Ls) :-
        test_sieve(Ls0, Av, Ls1),
        tests_sieve(Avs, Ls1, Ls).

test_sieve([], _, []).
test_sieve([L|Ls], C, Ls1) :-
        deactivates(C, L) ->
            test_sieve(Ls, C, Ls1)
      ; Ls1 = [L|Ls_],
        test_sieve(Ls, C, Ls_).

deactivates(allvars(X,Vs), L) :-
        ord_member(X, L),
        ord_intersection(L, Vs, []).
deactivates(sharedvars(X,Y,Vs), L) :-
        ord_member(X, L),
        ord_member(Y, L),
        ord_disjoint(L, Vs).

compute_SH_illegal([], _, _, _, [], []).
compute_SH_illegal([L|Ls], Var_p, Var_q, P_fr, SH_ill, B_sh2) :-
        illegal_shset(L, Var_p, Var_q, P_fr) ->
            SH_ill = [L|SH_ill_],
            compute_SH_illegal(Ls, Var_p, Var_q, P_fr, SH_ill_, B_sh2)
      ; B_sh2 = [L|B_sh2_],
        compute_SH_illegal(Ls, Var_p, Var_q, P_fr, SH_ill, B_sh2_).

illegal_shset(L, Var_p,  Var_q, P_fr) :-
        ord_intersect(L, Var_p),
        ord_intersect(L, Var_q),
        ord_disjoint(L, P_fr).

valid_situation(B_fr, B_sh) :-
        valid_situation(B_fr, [], B_sh), !.

valid_situation([], _, _).
valid_situation(Vs, Ws, [L|Ls]) :-
        ord_disjoint(L, Ws),
        ord_intersection(L, Vs, Vs_out), Vs_out = [_|_],
        ord_subtract(Vs, Vs_out, Vs2),
        ord_union(Ws, Vs_out, Ws2),
        valid_situation(Vs2, Ws2, Ls).
valid_situation(Vs, Ws, [_|Ls]) :-
        valid_situation(Vs, Ws, Ls).

compute_tests1([], _, _, _, _, Cs, Cs).
compute_tests1([L|Ls], B_sh_leg, Var_p, Var_q, P_fr, [C|Cs], Cs_) :-
        compute_test1(L, B_sh_leg, Var_p, Var_q, P_fr, C),
        test_sieve(Ls, C, Ls_),
        compute_tests1(Ls_, B_sh_leg, Var_p, Var_q, P_fr, Cs, Cs_).

compute_test1(L, B_sh_leg, Var_p, Var_q, P_fr, Av) :-
        av_test(L, B_sh_leg, Var_p, Var_q, P_fr, Av), !.
compute_test1(L, B_sh_leg, Var_p, Var_q, P_fr, Sv) :-
        sv_test(L, B_sh_leg, Var_p, Var_q, P_fr, Sv).

av_test(L, B_sh_leg, Var_p, Var_q, P_fr, Av) :-
        ord_intersection(Var_p, Var_q, Shvars),
        ord_intersection(L, Shvars, Lsh),
%%        reduce_all(Av0,
%%                   (member(X, Lsh), av_test1(B_sh_leg, X, P_fr, [], Av0)),
%%                   cheaper_av, Av).
        Lsh = [X|Xs],
        av_test1(B_sh_leg, X, P_fr, [], Av0),
        av_test_(Xs, B_sh_leg, P_fr, Av0, Av).

av_test_([], _, _, Av, Av).
av_test_([X|Xs], B_sh_leg, P_fr, Av0, Av) :-
        av_test1(B_sh_leg, X, P_fr, [], Av1),
        cheaper_av(Av0, Av1, Av2),
        av_test_(Xs, B_sh_leg, P_fr, Av2, Av).

av_test1([], X, _, F, allvars(X,F)).
av_test1([S|Ss], X, P_fr, F0, Av) :-
        ord_member(X, S) ->
            ord_intersection(S, P_fr, NF),
            ord_union(NF, F0, F1),
            av_test1(Ss, X, P_fr, F1, Av)
      ; av_test1(Ss, X, P_fr, F0, Av).

cheaper_av(Av1, Av2, Av) :-
        Av1 = allvars(_,F1),
        Av2 = allvars(_,F2),
        length(F1,L1),
        length(F2,L2),
        (
            L1 =< L2 ->
                Av = Av1
        ;   Av = Av2
        ).

sv_test(L, B_sh_leg, Var_p, Var_q, P_fr, Sv) :-
        ord_intersection(L, Var_p, L_p),
        ord_intersection(L, Var_q, L_q),
%%        reduce_all(Sv0,
%%                   (member(X, L_p), member(Y, L_q),
%%                    sv_test1(B_sh_leg, X, Y, P_fr, [], Sv0)),
%%                   cheaper_sv, Sv).
        sv_test_(L_p, L_q, B_sh_leg, P_fr, void, Sv).

sv_test_([], _, _, _, Sv, Sv).
sv_test_([X|Xs], Ys, B_sh_leg, P_fr, Sv0, Sv) :-
        sv_test__(Ys, X, B_sh_leg, P_fr, Sv0, Sv1),
        sv_test_(Xs, Ys, B_sh_leg, P_fr, Sv1, Sv).

sv_test__([], _, _, _, Sv, Sv).
sv_test__([Y|Ys], X, B_sh_leg, P_fr, Sv0, Sv) :-
        sv_test1(B_sh_leg, X, Y, P_fr, [], Sv1),
        cheaper_sv(Sv0, Sv1, Sv2),
        sv_test__(Ys, X, B_sh_leg, P_fr, Sv2, Sv).

sv_test1([], X, Y, _, F, sharedvars(X,Y,F)).
sv_test1([S|Ss], X, Y, P_fr, F0, Sv) :-
        ord_member(X, S),
        ord_member(Y, S) ->
            ord_intersection(S, P_fr, NF),
            ord_union(NF, F0, F1),
            sv_test1(Ss, X, Y, P_fr, F1, Sv)
      ; sv_test1(Ss, X, Y, P_fr, F0, Sv).

cheaper_sv(Sv1, Sv2, Sv) :-
        Sv1 = sharedvars(_,_,F1),
        Sv2 = sharedvars(_,_,F2),
        length(F1,L1),
        length(F2,L2),
        (   L1 =< L2 ->
                Sv = Sv1
        ;   Sv = Sv2
        ).
cheaper_sv(void, Sv, Sv).

compute_tests2([], _, _, _, B_sh, B_sh, []).
compute_tests2([L|Ls], Var_p, Var_q, B_fr, B_sh, B_sh_out, C2) :-
        compute_tests2_(L, Var_p, Var_q, B_fr, B_sh, C2_1),
        test_sieve(B_sh, C2_1, B_sh_),
        append(C2_1, C2_, C2),
        compute_tests2(Ls, Var_p, Var_q, B_fr, B_sh_, B_sh_out, C2_).

compute_tests2_(L, Var_p, Var_q, B_fr, B_sh, C2) :-
        ord_intersect(L, Var_p),
        ord_intersect(L, Var_q) ->
            findall(JS, join_set(B_sh, B_fr, Var_p, L, [], JS), JSs),
            joined_vars(JSs, Var_q, [], Jv),
            ord_intersection(Jv, B_fr, Jvfr),
            compute_tests2__(Jvfr, C2)
      ; C2 = [].

compute_tests2__([], []).
compute_tests2__([V1,V2|Vs], [allvars(V1,[V2])|C_]) :-
        compute_tests2__(Vs, V2, C_).

compute_tests2__([], _, []).
compute_tests2__([V2|Vs], V1,  [allvars(V1,[V2])|C_]) :-
        compute_tests2__(Vs, V2, C_).

join_set(_, _, _, L, U_N, JS) :- U_N = L, !, JS = [].
join_set([N|Ns], B_fr, Var_p, L, U_N, JS) :-
        ord_intersect(N, Var_p),
        ord_subset(N, L),
        ord_intersection(N, B_fr, N_fr),
        ord_disjoint(N_fr, U_N),
        ord_union_change(N, U_N, New_U_N), % New_U_N =/= U_N
        JS = [N|_JS],
        join_set(Ns, B_fr, Var_p, L, New_U_N, _JS).
join_set([_|Ns], B_fr, Var_p, L, U_N, JS) :-
        join_set(Ns, B_fr, Var_p, L, U_N, JS).

joined_vars([], _, Jv, Jv).
joined_vars([JS|JSs], Var_q, Jv0, Jv) :-
        joined_vars1(JS, Var_q, 0, [], Jv1),
        ord_union(Jv1, Jv0, Jv2),
        joined_vars(JSs, Var_q, Jv2, Jv).

joined_vars1([], _, I, Jv0, Jv) :-
        I >= 2 ->
            Jv = Jv0
      ; Jv = [].
joined_vars1([N|Ns], Var_q, I, Jv0, Jv) :-
        ord_intersect(N, Var_q) ->
            I1 is I+1,
            ord_union(N, Jv0, Jv1),
            joined_vars1(Ns, Var_q, I1, Jv1, Jv)
      ; joined_vars1(Ns, Var_q, I, Jv0, Jv).
        
make_test([], Avs, _, true, Avs).
make_test([C|Cs], Avs, Ren, E, Avs_out) :-
        add_av(C, Avs, Avs1),
        code_form(C, Ren, C1),
        make_test(Cs, C1, Avs1, Ren, E, Avs_out).

make_test([], C, Avs, _, C, Avs).
make_test([C|Cs], C0, Avs, Ren, (C0, E), Avs_out) :-
        add_av(C, Avs, Avs1),
        code_form(C, Ren, C1),
        make_test(Cs, C1, Avs1, Ren, E, Avs_out).

add_av(C, Avs, Avs1) :-
        C = allvars(_,_) ->
            Avs1 = [C|Avs]
      ; Avs1 = Avs.

code_form(allvars(X,[]), rename(Ns,Vs), C) :- !,
        find_name(Ns, Vs, X, V),
        C = (ground(V):noinfo).
code_form(allvars(X,L), rename(Ns,Vs), C) :- !,
        find_names([X|L], [V|Vs], Ns, Vs),
        C = (allvars(V,Vs):noinfo).
code_form(sharedvars(X,Y,[]), rename(Ns,Vs), C) :- !,
        find_name(Ns, Vs, X, V),
        find_name(Ns, Vs, Y, W),
        C = (indep(V,W):noinfo).
code_form(sharedvars(X,Y,L), rename(Ns,Vs), C) :- !,
        find_names([X,Y|L], [V,W|Vs], Ns, Vs),
        C = (sharedvars(V,W,Vs):noinfo).

find_names([], [], _, _).
find_names([X|Xs], [Y|Ys], Ns, Vs) :-
        find_name(Ns, Vs, X, Y),
        find_names(Xs, Ys, Ns, Vs).

%% reduce_all(T, G, C, R) :-
%%         findall(T, G, [X|Xs]),
%%         reduce(Xs, C, X, R).
%% 
%% reduce([X|Xs], C, R) :-
%% 	reduce(Xs, C, X, R).
%% 
%% reduce([], _, R, R).
%% reduce([Y|Ys], C, X, R) :-
%%         apply(C, X, Y, Z),
%%         reduce(Ys, C, Z, R).
%% 
%% apply(P, X, Y, Z) :-
%%         functor(G, P, 3),
%%         arg(1, G, X),
%%         arg(2, G, Y),
%%         arg(3, G, Z),
%%         call(G).


/****************************************************************************/
/*                   Renaming & Substituting variables                      */
/****************************************************************************/

ren_subst_goals((E1, Er), GInfo, GDict, RExp, Dict, NDict) :- !,
        ren_subst_goals(Er, GInfo, GDict, REr, Dict, Dict1),
        ren_subst_goals(E1, GInfo, GDict, REr, RExp, Dict1, NDict).
ren_subst_goals((T->P;S), GInfo, GDict, (T->RP;RS), Dict, NDict) :- !,
        ren_subst_goals(P, GInfo, GDict, RP, Dict, Dict1),
        ren_subst_goals(S, GInfo, GDict, RS, Dict1, NDict).
ren_subst_goals(ParExp, GInfo, GDict, RParExp, Dict, NDict) :-
        ParExp = (_&_), !,
        take_beta(ParExp, GInfo, Beta),
        ren_subst_each(ParExp, GInfo, GDict, NParExp, Vars, Dict, Dict1),
        ren_subst_var(NParExp, Vars, Beta, RParExp, Dict1, NDict).
ren_subst_goals(node(I, _, _, _), _GInfo, GDict, G, Dict, Dict) :-
        arg(I, GDict, G).

ren_subst_goals((E1, Er), GInfo, GDict, RExp_, RExp, Dict, NDict) :- !,
        ren_subst_goals(Er, GInfo, GDict, RExp_, REr, Dict, Dict1),
        ren_subst_goals(E1, GInfo, GDict, REr, RExp, Dict1, NDict).
ren_subst_goals((T->P;S), GInfo, GDict, RExp_, ((T->RP;RS),RExp_),
                Dict, NDict) :- !,
        ren_subst_goals(P, GInfo, GDict, RP, Dict, Dict1),
        ren_subst_goals(S, GInfo, GDict, RS, Dict1, NDict).
ren_subst_goals(ParExp, GInfo, GDict, RExp_, RParExp, Dict, NDict) :-
        ParExp = (_&_), !,
        take_beta(ParExp, GInfo, Beta),
        ren_subst_each(ParExp, GInfo, GDict, NParExp, Vars, Dict, Dict1),
        ren_subst_var(NParExp, Vars, Beta, RExp_, RParExp, Dict1, NDict).
ren_subst_goals(node(I, _, _, _), _GInfo, GDict, RExp_, (G, RExp_),
                Dict, Dict) :-
        arg(I, GDict, G).

take_beta(Gs, GInfo, Beta) :-
        take_beta(Gs, GInfo, 9999, _Leftmost, [], Beta).

take_beta((E & Es), GInfo, Left0, Left, B0, Beta) :-
        take_beta(E, GInfo, Left0, Left1, B0, B1),
        take_beta(Es, GInfo, Left1, Left, B1, Beta).
take_beta((E, _Es), GInfo, Left0, Left, B0, Beta) :-
        take_beta(E, GInfo, Left0, Left, B0, Beta).
take_beta((_T->_P;S1,_S), GInfo, Left0, Left, B0, Beta) :-
        take_beta(S1, GInfo, Left0, Left, B0, Beta).
take_beta(node(I, _, _, _), GInfo, Left0, Left, B0, Beta) :-
        member(node(I, _, IBeta, _), GInfo), !,
        (
            I < Left0 ->
                Left = I,
                Beta = IBeta
        ;   Left = Left0,
            Beta = B0
        ).

ren_subst_each((E & Es), GInfo, GDict, (RE & REs), [EVars|EsVars],
               Dict, NDict) :- !,
        take_vars(E, GInfo, [], EVars),
        ren_subst_goals(E, GInfo, GDict, RE, Dict, Dict1),
        ren_subst_each(Es, GInfo, GDict, REs, EsVars, Dict1, NDict).
ren_subst_each(E, GInfo, GDict, RE, [EVars], Dict, NDict) :-
        take_vars(E, GInfo, [], EVars),
        ren_subst_goals(E, GInfo, GDict, RE, Dict, NDict).

take_vars((E & Es), GInfo, Vars0, Vars) :-
        take_vars(E, GInfo, Vars0, Vars1),
        take_vars(Es, GInfo, Vars1, Vars).
take_vars((E, Es), GInfo, Vars0, Vars) :-
        take_vars(E, GInfo, Vars0, Vars1),
        take_vars(Es, GInfo, Vars1, Vars).
take_vars((_T->_P;S1,S), GInfo, Vars0, Vars) :-
        take_vars(S1, GInfo, Vars0, Vars1),
        take_vars(S, GInfo, Vars1, Vars).
take_vars(node(I, _, _, _), GInfo, Vars0, Vars) :-
        member(node(I, IVars, _, _), GInfo), !,
        ord_union(Vars0, IVars, Vars).

% change shfr by mshare
ren_subst_var(Gs, GsVars, mshare(B_sh, B_fr), RenGs, Dict, NDict) :-
        comp_SH(B_sh, GsVars, SH),
        comp_Vs(SH, B_fr, [], Vs),
        comp_ren_sv(Vs, SH, Gs, GsVars, RSss),
        group_by_goal(Gs, RSss, GRSs),
        process_goals(GRSs, Gs, Dict, NDict, RenGs0, BB, true, SUBV, SUBV_),
        RenGs = SUBV, rem_final_true((RenGs0,BB), SUBV_).

% change shfr by mshare
ren_subst_var(Gs, GsVars, mshare(B_sh, B_fr), RenGs_, RenGs, Dict, NDict) :-
        comp_SH(B_sh, GsVars, SH),
        comp_Vs(SH, B_fr, [], Vs),
        comp_ren_sv(Vs, SH, Gs, GsVars, RSss),
        group_by_goal(Gs, RSss, GRSs),
        process_goals(GRSs, Gs, Dict, NDict, RenGs0, BB, BB_, SUBV, SUBV_),
        RenGs = SUBV, SUBV_ = (RenGs0,BB), BB_ = RenGs_.

rem_final_true((A,B), X) :-
        rem_final_true(B, A, X).

rem_final_true(true, X, X).
rem_final_true((A,B), X, (X,Y)) :-
        rem_final_true(B, A, Y).


% type comp_SH(sh_abs, list(vars), ord_set(shset)).

comp_SH([], _Vs, []).

comp_SH([L|Ls], Vs, SH) :-
        shared_by(Vs, L) ->
            SH = [L|_SH],
            comp_SH(Ls, Vs, _SH) ;
        comp_SH(Ls, Vs, SH).

shared_by([V|Vs], L) :-
        ord_intersect(V, L) ->
            intersect_with(Vs, L) ;
        shared_by(Vs, L).

intersect_with([V|_], L) :-
        ord_intersect(V,L).
intersect_with([_|Vs], L) :-
        intersect_with(Vs, L).

% type comp_Vs(ord_set(shset), fr_abs, list(shset), list(shset)).

comp_Vs([], _, Vs, Vs).
comp_Vs([L|Ls], B_fr, TmpVs, Vs) :-
        ord_intersection(L, B_fr, L_fr),
        insert_V(L_fr, TmpVs, TmpVs1),
        comp_Vs(Ls, B_fr, TmpVs1, Vs).

insert_V([], Vs, Vs) :- !.
insert_V(L_fr, Vs, NewVs) :-
        insert_V1(Vs, L_fr, NewVs).

insert_V1([], L_fr, [L_fr]).
insert_V1([V|Vs], L_fr, NewVs) :-
        ord_intersect(V, L_fr) ->
            ord_union(V, L_fr, U),
            insert_V1(Vs, U, NewVs) ;
        NewVs = [V|NewVs_],
        insert_V1(Vs, L_fr, NewVs_).

% type comp_ren_sv(list(shset), ord_set(shset), par_exp, list(vars),
%                  list(list(rs(seq_list, list(ren(progvar) |
%                                              svf(progvar, progvar) |
%                                              sv(progvar,progvar)))))).

comp_ren_sv([], _SH, _Gs, _GsVars, []).
comp_ren_sv([V|Vs], SH, Gs, GsVars, [RSs|RSss]) :-
        comp_ren_sv1(V, SH, Gs, GsVars, RSs),
        comp_ren_sv(Vs, SH, Gs, GsVars, RSss).

comp_ren_sv1(V, SH, Gs, GsVars, RSs) :-
        comp_RV(SH, V, [], RV),
        comp_ren_sv2(Gs, GsVars, V, RV, RSs0),
        rm_most_costly(RSs0, RSs).

comp_RV([], V, RV0, RV) :-
        ord_subtract(RV0, V, RV).
comp_RV([L|Ls], V, RV0, RV) :-
        ord_intersect(L, V) ->
            ord_union(L, RV0, RV1),
            comp_RV(Ls, V, RV1, RV) ;
        comp_RV(Ls, V, RV0, RV).

comp_ren_sv2((G & Gs), [GVars|GsVars], V, RV, [rs(G,RS)|RSs]) :- !,
        ord_intersection(V, GVars, VG),
        ord_intersection(RV, GVars, RVG),
        V = [X|_],
        comp_ren_sv3(VG, RVG, X, RS),
        comp_ren_sv2(Gs, GsVars, V, RV, RSs).
comp_ren_sv2(G, [GVars], V, RV, [rs(G,RS)]) :-
        ord_intersection(V, GVars, VG),
        ord_intersection(RV, GVars, RVG),
        V = [X|_],
        comp_ren_sv3(VG, RVG, X, RS).

comp_ren_sv3([], [], _X, []) :- !.
comp_ren_sv3([], RVG, X, [r0(X)|RS]) :- !, % r0 -> for the back-bindings
        make_sv_list(RVG, X, RS).
comp_ren_sv3([X|VG1], RVG, _X, [ren(X)|RS]) :-
        make_svf_list(VG1, X, RS, RS1),
        make_sv_list(RVG, X, RS1).

make_svf_list([], _X, L, L).
make_svf_list([Y|Ys], X, [svf(X,Y)|RS], Tail) :-
        make_svf_list(Ys, X, RS, Tail).

make_sv_list([], _X, []).
make_sv_list([Y|Ys], X, [sv(X,Y)|RS]) :-
        make_sv_list(Ys, X, RS).

rm_most_costly([], []).
rm_most_costly([RS|RSs], RSOut) :-
        rm_most_costly(RSs, RS, RSOut).

rm_most_costly([], _Max, []).
rm_most_costly([RS|RSs], CurrMax, RSOut) :-
        more_costly(RS, CurrMax) ->
            RSOut = [CurrMax|RSOut1],
            rm_most_costly(RSs, RS, RSOut1) ;
        RSOut = [RS|RSOut1],
        rm_most_costly(RSs, CurrMax, RSOut1).

more_costly(rs(_G1, RS1), rs(_G2, RS2)) :-
        cost(RS1, 0, C1),
        cost(RS2, 0, C2),
        C1 > C2.

cost([], C, C).
cost([Tr|Trs], A, C) :-
        cost1(Tr, C1),
        A1 is A+C1,
        cost(Trs, A1, C).

cost1(r0(_), 0).
cost1(ren(_), 1).
cost1(svf(_,_), 30). % 1 * oo ~ 30
cost1(sv(_,_), 900). % 30 * oo ~ 900 

% type group_by_goal(par_exp,
%                    list(list(rs(seq_list,list(ren(progvar) |
%                                               svf(progvar, progvar) |
%                                               sv(progvar,progvar))))),
%                    list(rs(seq_list,list(ren(progvar) |
%                                          svf(progvar, progvar) |
%                                          sv(progvar,progvar)))),
%                    list(rs(seq_list,list(ren(progvar) |
%                                          svf(progvar, progvar) |
%                                          sv(progvar,progvar)))).

group_by_goal((G & Gs), RSss, [GRS|GRSs]) :- !,
        get_rs_goal(RSss, G, GRS),
        group_by_goal(Gs, RSss, GRSs).
group_by_goal(G, RSss, [GRS]) :-
        get_rs_goal(RSss, G, GRS).

get_rs_goal([], _G, []).
get_rs_goal([RSs|RSss], G, GRSs) :-
        get_rs_goal1(RSs, G, GRSs, GRSs_),
        get_rs_goal(RSss, G, GRSs_).

get_rs_goal1([], _G, GRSs_, GRSs_).
get_rs_goal1([rs(G, RSg)|RSs], G0, GRSs, GRSs_) :-
        G == G0, !,
        append(RSg, GRSs0, GRSs),
        get_rs_goal1(RSs, G, GRSs0, GRSs_).
get_rs_goal1([_RS|RSs], G, GRSs, GRSs_) :-
        get_rs_goal1(RSs, G, GRSs, GRSs_).

%%

process_goals([RS|RSs], Gs, Dict, NDict, RenGs, BB, BB_, SUBV, SUBV_) :-
        process_goals(RSs, RS, Gs, Dict, NDict, RenGs, BB, BB_, SUBV, SUBV_).

process_goals([], RS, G, Din, Dout, RenG, BB, BB_, SUBV, SUBV_) :-
        process_goal(RS, G, Din, Dout, RenG, BB, BB_, SUBV, SUBV_).
process_goals([RS1|RSs], RS, (G & Gs), Din, Dout, (RenG & RenGs),
               BB, BB_, SUBV, SUBV_ ) :-
        process_goal(RS, G, Din, Daux, RenG, BB, BB0, SUBV, SUBV0),
        process_goals(RSs, RS1, Gs, Daux, Dout, RenGs, BB0, BB_, SUBV0, SUBV_).

process_goal(RSg, G, Din, Dout, RenG, BB, BB_, SUBV, SUBV_) :-
        get_ren_sv_goal(RSg, [], [], REN, SV),
        make_back_bindings(REN, Din, Dout, LBB),
        list_to_seq(LBB, BB, BB_),
        make_subst_vars(SV, LBB, SUBV, SUBV_),
        rename_term(G, LBB, RenG).

get_ren_sv_goal([], REN, SV, REN, SV).
get_ren_sv_goal([r0(X)|Trs], REN0, SV0, REN, SV) :- !,
        insert_ren(REN0, X, REN1),
        get_ren_sv_goal(Trs, REN1, SV0, REN, SV).
get_ren_sv_goal([ren(X)|Trs], REN0, SV0, REN, SV) :- !,
        insert_ren(REN0, X, REN1),
        get_ren_sv_goal(Trs, REN1, SV0, REN, SV).
get_ren_sv_goal([svf(X,Y)|Trs], REN0, SV0, REN, SV) :- !,
        insert_subst(SV0, X, Y, SV1),
        insert_ren(REN0, Y, REN1),
        get_ren_sv_goal(Trs, REN1, SV1, REN, SV).
get_ren_sv_goal([sv(X,Y)|Trs], REN0, SV0, REN, SV) :- !,
        insert_subst(SV0, X, Y, SV1),
        insert_ren(REN0, Y, REN1),
        get_ren_sv_goal(Trs, REN1, SV1, REN, SV).

insert_ren([], X, [X]).
insert_ren([X|Xs], Y, [X|Zs]) :-
        X == Y ->
            Zs = Xs ;
        insert_ren(Xs, Y, Zs).

insert_subst([], X, Y, [sv([X],Y)]).
insert_subst([sv(A,B)|SVs], X, Y, SVs1) :-
        Y == B ->
            SVs1 = [sv([X|A], B)|SVs] ;
        SVs1 = [sv(A,B)|SVs_],
        insert_subst(SVs, X, Y, SVs_).

%%

make_back_bindings([], Dict, Dict, []).
make_back_bindings([X|Xs], Din, Dout, [(X = X_)|LBB]) :-
        Din = dic(Vs, Names),
        find_name(Vs, Names, X, NameX),
%        name(NameX, StrX),
        NameX = StrX,
        dlist(StrX, StrX_, [0'_ | S]), %']) Emacs trick
        make_suffix(S, StrX_, Names),
%        name(NameX_, StrX_),
        NameX_ = StrX_,
        insert_last(Vs, X_,  Vs1),
        insert_last(Names, NameX_, Names1),
        make_back_bindings(Xs, dic(Vs1,Names1), Dout, LBB).

list_to_seq([], BB_, BB_).
list_to_seq([U|Us], (U:noinfo, BB), BB_) :-
        list_to_seq(Us, BB, BB_).

make_suffix([], StrX, Names) :-
%        name(NameX, StrX),
        NameX = StrX,
        \+ member(NameX, Names), !.
make_suffix([0'_|S], StrX, Names) :- %']) Emacs trick
        make_suffix(S, StrX, Names).

% get_number(N, StrX_, Names, Number) :-
%         name(N, Number),
%         name(NameX, StrX_),
%         \+ member(NameX, Names), !.
% get_number(N, StrX_, Names, Number) :-
%         N1 is N+1,
%         get_number(N1, StrX_, Names, Number).

%%

make_subst_vars([], _, SUBV_, SUBV_).
make_subst_vars([sv(LX,Y)|SV], BB,
                (subst_vars(Y,NY,LX,NLX):noinfo,SUBV), SUBV_):-
        look_N(BB, Y, NY),
        look_Ns(LX, BB, NLX),
        make_subst_vars(SV, BB, SUBV, SUBV_).

look_Ns([], _, []).
look_Ns([X|Xs], BB, [NX|NXs]) :-
        look_N(BB, X, NX),
        look_Ns(Xs, BB, NXs).

look_N([(Y=NY)|BB], X, NX) :-
        X == Y ->
            NX = NY ;
        look_N(BB, X, NX).

look_N_or_unify([], X, NX) :-
        NX = X.
look_N_or_unify([(Y=NY)|BB], X, NX) :-
        X == Y ->
            NX = NY ;
        look_N_or_unify(BB, X, NX).

rename_term(V, BB, NV) :-
        var(V), !,
        look_N_or_unify(BB, V, NV).
rename_term(T, BB, NT) :- 
        functor(T, F, A),
        functor(NT, F, A),
        rename_term_args(A, T, BB, NT).

rename_term_args(0, _, _, _) :- !.
rename_term_args(A, T, BB, NT) :-
        arg(A, T, T_A),
        arg(A, NT, NT_A),
        rename_term(T_A, BB, NT_A),
        A1 is A-1,
        rename_term_args(A1, T, BB, NT).


:- pop_prolog_flag(multi_arity_warnings).

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

