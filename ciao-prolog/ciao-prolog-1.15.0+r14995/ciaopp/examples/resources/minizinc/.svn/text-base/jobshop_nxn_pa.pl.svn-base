:- module(_1,[main/5],[ciaopp(examples(resources(minizinc))),assertions,nativeprops,basicmodes,regtypes,isomodes]).

:- entry main(_1,_2,_3,_4,_5)
         : ( int(_1), dint(_2), int(_3), array(_4), var(_5) ).

:- true pred main(Size,End,Total,D,S)
         : ( int(Size), dint(End), int(Total), array(D), term(S) )
        => ( int(Size), dint(End), int(Total), array(D), array(S) ).

:- true pred main(Size,End,Total,D,S)
         : ( mshare([[S]]), var(S), ground([Size,End,Total,D]) )
        => ground([Size,End,Total,D,S]).

:- true pred main(Size,End,Total,D,S)
         : ( int(Size), dint(End), int(Total), array(D), var(S) )
        => ( int(Size), dint(End), int(Total), array(D), array(S) )
         + ( not_fails, covered ).

:- true pred main(Size,End,Total,D,S)
         : ( int(Size), dint(End), int(Total), array(D), var(S) )
        => ( int(Size), dint(End), int(Total), array(D), array(S), size(lb,Size,int(Size)), size(lb,End,size(End)), size(lb,Total,int(Total)), size(lb,D,size(D)), size(lb,S,0) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,2*exp(int(Size),2)), cost(lb,numvars,2*exp(int(Size),2)) ).

:- true pred main(Size,End,Total,D,S)
         : ( int(Size), dint(End), int(Total), array(D), var(S) )
        => ( int(Size), dint(End), int(Total), array(D), array(S), size(ub,Size,int(Size)), size(ub,End,size(End)), size(ub,Total,int(Total)), size(ub,D,size(D)), size(ub,S,inf) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*exp(int(Size),3)+2*exp(int(Size),2)), cost(ub,numvars,4*exp(int(Size),3)+2*exp(int(Size),2)) ).

main(Size,End,Total,D,S) :-
        minizinc_rt:(Ss is Size-1+1),
        build_array(S,Ss),
        init_s_i(Ss,Total,Size,S),
        minizinc_rt:(Ind_i is Size-1+1),
        forall1_i(Ind_i,Size,Size,S,D,End).

:- true pred init_s_i(Ind_i,_DU,_Uj,_S)
         : ( int(Ind_i), int(_DU), int(_Uj), array(_S) )
        => ( character_code(Ind_i), int(_DU), int(_Uj), array(_S) ).

:- true pred init_s_i(Ind_i,_DU,_Uj,_S)
         : ground([Ind_i,_DU,_Uj,_S])
        => ground([Ind_i,_DU,_Uj,_S]).

:- true pred init_s_i(Ind_i,_DU,_Uj,_S)
         : ( int(Ind_i), int(_DU), int(_Uj), array(_S) )
        => ( character_code(Ind_i), int(_DU), int(_Uj), array(_S) )
         + ( not_fails, covered ).

:- true pred init_s_i(Ind_i,_DU,_Uj,_S)
         : ( int(Ind_i), int(_DU), int(_Uj), array(_S) )
        => ( character_code(Ind_i), int(_DU), int(_Uj), array(_S), size(lb,Ind_i,int(Ind_i)), size(lb,_DU,int(_DU)), size(lb,_Uj,int(_Uj)), size(lb,_S,size(_S)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(_Uj)*int(Ind_i)) ).

:- true pred init_s_i(Ind_i,_DU,_Uj,_S)
         : ( int(Ind_i), int(_DU), int(_Uj), array(_S) )
        => ( character_code(Ind_i), int(_DU), int(_Uj), array(_S), size(ub,Ind_i,int(Ind_i)), size(ub,_DU,int(_DU)), size(ub,_Uj,int(_Uj)), size(ub,_S,size(_S)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(_Uj)*int(Ind_i)) ).

init_s_i(0,_DU,_Uj,_S) :- !.
init_s_i(Ind_i,DU,Uj,S) :-
        element_array(Ind_i,S,Si),
        minizinc_rt:(Siz_j is Uj-1+1),
        build_array(Si,Siz_j),
        init_s_ij(Siz_j,DU,Si),
        minizinc_rt:(NInd_i is Ind_i-1),
        init_s_i(NInd_i,DU,Uj,S).

:- true pred init_s_ij(Ind_j,_DU,_Si)
         : ( int(Ind_j), int(_DU), array(_Si) )
        => ( character_code(Ind_j), int(_DU), array(_Si) ).

:- true pred init_s_ij(Ind_j,_DU,_Si)
         : ground([Ind_j,_DU,_Si])
        => ground([Ind_j,_DU,_Si]).

:- true pred init_s_ij(Ind_j,_DU,_Si)
         : ( int(Ind_j), int(_DU), array(_Si) )
        => ( character_code(Ind_j), int(_DU), array(_Si) )
         + ( not_fails, covered ).

:- true pred init_s_ij(Ind_j,_DU,_Si)
         : ( int(Ind_j), int(_DU), array(_Si) )
        => ( character_code(Ind_j), int(_DU), array(_Si), size(lb,Ind_j,int(Ind_j)), size(lb,_DU,int(_DU)), size(lb,_Si,size(_Si)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind_j)) ).

:- true pred init_s_ij(Ind_j,_DU,_Si)
         : ( int(Ind_j), int(_DU), array(_Si) )
        => ( character_code(Ind_j), int(_DU), array(_Si), size(ub,Ind_j,int(Ind_j)), size(ub,_DU,int(_DU)), size(ub,_Si,size(_Si)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind_j)) ).

init_s_ij(0,_DU,_Si) :- !.
init_s_ij(Ind_j,DU,Si) :-
        element_dint(Ind_j,Si,Sij),
        in(Sij,0,DU),
        minizinc_rt:(NInd_j is Ind_j-1),
        init_s_ij(NInd_j,DU,Si).

:- true pred no_overlap(S1,D1,S2,D2)
         : ( dint(S1), int(D1), dint(S2), int(D2) )
        => ( dint(S1), int(D1), dint(S2), int(D2) ).

:- true pred no_overlap(S1,D1,S2,D2)
         : ground([S1,D1,S2,D2])
        => ground([S1,D1,S2,D2]).

:- true pred no_overlap(S1,D1,S2,D2)
         : ( dint(S1), int(D1), dint(S2), int(D2) )
        => ( dint(S1), int(D1), dint(S2), int(D2) )
         + ( not_fails, covered ).

:- true pred no_overlap(S1,D1,S2,D2)
         : ( dint(S1), int(D1), dint(S2), int(D2) )
        => ( dint(S1), int(D1), dint(S2), int(D2), size(lb,S1,size(S1)), size(lb,D1,int(D1)), size(lb,S2,size(S2)), size(lb,D2,int(D2)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,5), cost(lb,numvars,4) ).

:- true pred no_overlap(S1,D1,S2,D2)
         : ( dint(S1), int(D1), dint(S2), int(D2) )
        => ( dint(S1), int(D1), dint(S2), int(D2), size(ub,S1,size(S1)), size(ub,D1,int(D1)), size(ub,S2,size(S2)), size(ub,D2,int(D2)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5), cost(ub,numvars,4) ).

no_overlap(S1,D1,S2,D2) :-
        plus_dint_int_var(S1,D1,A),
        reif_leq_dint_dint_var(A,S2,B1),
        plus_dint_int_var(S2,D2,B),
        reif_leq_dint_dint_var(B,S1,B2),
        or_dint_dint_int(B1,B2,1).

%% %% :- trust pred forall1_i(Ind_i,_1,_2,_3,_4,_5)
%% %%    : ( int(Ind_i), int(_1), int(_2), term(_3), array(_4), dint(_5) )
%% %%   => ( int(Ind_i), int(_1), int(_2), term(_3), array(_4), dint(_5) ).

:- entry forall1_i(_1,_2,_3,_4,_5,_6)
         : ( int(_1), int(_2), int(_3), array(_4), array(_5), dint(_6) ).

:- check calls forall1_i(Ind_i,_1,_2,_3,_4,_5)
         : ( int(Ind_i), int(_1), int(_2), term(_3), array(_4), dint(_5) ).

:- trust success forall1_i(Ind_i,_1,_2,_3,_4,_5)
         : ( int(Ind_i), int(_1), int(_2), term(_3), array(_4), dint(_5) )
        => ( int(Ind_i), int(_1), int(_2), term(_3), array(_4), dint(_5) ).

:- true pred forall1_i(Ind_i,_1,_2,_3,_4,_5)
         : ( int(Ind_i), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind_i), int(_1), int(_2), array(_3), array(_4), dint(_5) ).

:- true pred forall1_i(Ind_i,_1,_2,_3,_4,_5)
         : ground([Ind_i,_1,_2,_3,_4,_5])
        => ground([Ind_i,_1,_2,_3,_4,_5]).

:- true pred forall1_i(Ind_i,_1,_2,_3,_4,_5)
         : ( int(Ind_i), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind_i), int(_1), int(_2), array(_3), array(_4), dint(_5) )
         + ( not_fails, covered ).

:- true pred forall1_i(Ind_i,_1,_2,_3,_4,_5)
         : ( int(Ind_i), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind_i), int(_1), int(_2), array(_3), array(_4), dint(_5), size(lb,Ind_i,int(Ind_i)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,2*(int(_2)*int(Ind_i))), cost(lb,numvars,int(_2)*int(Ind_i)) ).

:- true pred forall1_i(Ind_i,_1,_2,_3,_4,_5)
         : ( int(Ind_i), int(_1), int(_2), array(_3), array(_4), dint(_5) )
        => ( int(Ind_i), int(_1), int(_2), array(_3), array(_4), dint(_5), size(ub,Ind_i,int(Ind_i)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*(exp(int(_2),2)*int(Ind_i))+2*(int(_2)*int(Ind_i))), cost(ub,numvars,4*(exp(int(_2),2)*int(Ind_i))+int(_2)*int(Ind_i)) ).

forall1_i(0,_1,_2,_3,_4,_5) :- !.
forall1_i(Ind_i,Ui,Size,S,D,End) :-
        minizinc_rt:(Index_i is Ui-Ind_i+1),
        minizinc_rt:(Uj is Size-1),
        minizinc_rt:(Ind_j is Uj-1+1),
        forall1_j(Ind_j,Uj,Index_i,Size,S,D,End),
        minizinc_rt:(I is Index_i-1+1),
        element2d_dint(I,Size,S,S_i_size),
        element2d_int(I,Size,D,D_i_size),
        plus_dint_int_var(S_i_size,D_i_size,B),
        leq(B,End),
        minizinc_rt:(Ind2_j is Size-1+1),
        forall2_j(Ind2_j,Size,Index_i,Size,S,D),
        minizinc_rt:(NInd_i is Ind_i-1),
        forall1_i(NInd_i,Ui,Size,S,D,End).

:- true pred forall1_j(Ind_j,_1,_2,_3,_4,_5,_6)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5), dint(_6) )
        => ( character_code(Ind_j), character_code(_1), character_code(_2), int(_3), array(_4), array(_5), dint(_6) ).

:- true pred forall1_j(Ind_j,_1,_2,_3,_4,_5,_6)
         : ground([Ind_j,_1,_2,_3,_4,_5,_6])
        => ground([Ind_j,_1,_2,_3,_4,_5,_6]).

:- true pred forall1_j(Ind_j,_1,_2,_3,_4,_5,_6)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5), dint(_6) )
        => ( character_code(Ind_j), character_code(_1), character_code(_2), int(_3), array(_4), array(_5), dint(_6) )
         + ( not_fails, covered ).

:- true pred forall1_j(Ind_j,_1,_2,_3,_4,_5,_6)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5), dint(_6) )
        => ( character_code(Ind_j), character_code(_1), character_code(_2), int(_3), array(_4), array(_5), dint(_6), size(lb,Ind_j,int(Ind_j)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)), size(lb,_6,size(_6)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,2*int(Ind_j)), cost(lb,numvars,int(Ind_j)) ).

:- true pred forall1_j(Ind_j,_1,_2,_3,_4,_5,_6)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5), dint(_6) )
        => ( character_code(Ind_j), character_code(_1), character_code(_2), int(_3), array(_4), array(_5), dint(_6), size(ub,Ind_j,int(Ind_j)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)), size(ub,_6,size(_6)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,2*int(Ind_j)), cost(ub,numvars,int(Ind_j)) ).

forall1_j(0,_1,_2,_3,_4,_5,_6) :- !.
forall1_j(Ind_j,Uj,Index_i,Size,S,D,End) :-
        minizinc_rt:(Index_j is Uj-Ind_j+1),
        minizinc_rt:(I is Index_i-1+1),
        minizinc_rt:(J is Index_j-1+1),
        element2d_dint(I,J,S,Sij),
        element2d_int(I,J,D,Dij),
        minizinc_rt:(J1 is J+1),
        element2d_dint(I,J1,S,Sij1),
        plus_dint_int_var(Sij,Dij,A),
        leq(A,Sij1),
        minizinc_rt:(NInd_j is Ind_j-1),
        forall1_j(NInd_j,Uj,Index_i,Size,S,D,End).

:- true pred forall2_j(Ind_j,_1,_2,_3,_4,_5)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( character_code(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5) ).

:- true pred forall2_j(Ind_j,_1,_2,_3,_4,_5)
         : ground([Ind_j,_1,_2,_3,_4,_5])
        => ground([Ind_j,_1,_2,_3,_4,_5]).

:- true pred forall2_j(Ind_j,_1,_2,_3,_4,_5)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( character_code(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5) )
         + ( not_fails, covered ).

:- true pred forall2_j(Ind_j,_1,_2,_3,_4,_5)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( character_code(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5), size(lb,Ind_j,int(Ind_j)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_j(Ind_j,_1,_2,_3,_4,_5)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( character_code(Ind_j), int(_1), int(_2), int(_3), array(_4), array(_5), size(ub,Ind_j,int(Ind_j)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*(int(_3)*int(Ind_j))), cost(ub,numvars,4*(int(_3)*int(Ind_j))) ).

forall2_j(0,_1,_2,_3,_4,_5) :- !.
forall2_j(Ind_j,Uj,Index_i,Size,S,D) :-
        minizinc_rt:(Index_j is Uj-Ind_j+1),
        minizinc_rt:(Ind_k is Size-1+1),
        forall2_jk(Ind_k,Size,Index_i,Index_j,S,D),
        minizinc_rt:(NInd_j is Ind_j-1),
        forall2_j(NInd_j,Uj,Index_i,Size,S,D).

:- true pred forall2_jk(Ind_k,_1,_2,_3,_4,_5)
         : ( int(Ind_k), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( character_code(Ind_k), int(_1), int(_2), int(_3), array(_4), array(_5) ).

:- true pred forall2_jk(Ind_k,_1,_2,_3,_4,_5)
         : ground([Ind_k,_1,_2,_3,_4,_5])
        => ground([Ind_k,_1,_2,_3,_4,_5]).

:- true pred forall2_jk(Ind_k,_1,_2,_3,_4,_5)
         : ( int(Ind_k), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( character_code(Ind_k), int(_1), int(_2), int(_3), array(_4), array(_5) )
         + ( not_fails, covered ).

:- true pred forall2_jk(Ind_k,_1,_2,_3,_4,_5)
         : ( int(Ind_k), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( character_code(Ind_k), int(_1), int(_2), int(_3), array(_4), array(_5), size(lb,Ind_k,int(Ind_k)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_jk(Ind_k,_1,_2,_3,_4,_5)
         : ( int(Ind_k), int(_1), int(_2), int(_3), array(_4), array(_5) )
        => ( character_code(Ind_k), int(_1), int(_2), int(_3), array(_4), array(_5), size(ub,Ind_k,int(Ind_k)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*int(Ind_k)), cost(ub,numvars,4*int(Ind_k)) ).

forall2_jk(0,_1,_2,_3,_4,_5) :- !.
forall2_jk(Ind_k,Uk,Index_i,Index_j,S,D) :-
        minizinc_rt:(Index_k is Uk-Ind_k+1),
        where_forall2_jk(Index_i,Index_j,Index_k,S,D),
        minizinc_rt:(NInd_k is Ind_k-1),
        forall2_jk(NInd_k,Uk,Index_i,Index_j,S,D).

:- true pred where_forall2_jk(Index_i,Index_j,Index_k,S,D)
         : ( int(Index_i), int(Index_j), int(Index_k), array(S), array(D) )
        => ( int(Index_i), int(Index_j), int(Index_k), array(S), array(D) ).

:- true pred where_forall2_jk(Index_i,Index_j,Index_k,S,D)
         : ground([Index_i,Index_j,Index_k,S,D])
        => ground([Index_i,Index_j,Index_k,S,D]).

:- true pred where_forall2_jk(Index_i,Index_j,Index_k,S,D)
         : ( int(Index_i), int(Index_j), int(Index_k), array(S), array(D) )
        => ( int(Index_i), int(Index_j), int(Index_k), array(S), array(D) )
         + ( not_fails, covered ).

:- true pred where_forall2_jk(Index_i,Index_j,Index_k,S,D)
         : ( int(Index_i), int(Index_j), int(Index_k), array(S), array(D) )
        => ( int(Index_i), int(Index_j), int(Index_k), array(S), array(D), size(lb,Index_i,int(Index_i)), size(lb,Index_j,int(Index_j)), size(lb,Index_k,int(Index_k)), size(lb,S,size(S)), size(lb,D,size(D)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred where_forall2_jk(Index_i,Index_j,Index_k,S,D)
         : ( int(Index_i), int(Index_j), int(Index_k), array(S), array(D) )
        => ( int(Index_i), int(Index_j), int(Index_k), array(S), array(D), size(ub,Index_i,int(Index_i)), size(ub,Index_j,int(Index_j)), size(ub,Index_k,int(Index_k)), size(ub,S,size(S)), size(ub,D,size(D)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5), cost(ub,numvars,4) ).

where_forall2_jk(Index_i,Index_j,Index_k,S,D) :-
        Index_k<Index_j,
        !,
        element2d_dint(Index_j,Index_i,S,Sji),
        element2d_int(Index_j,Index_i,D,Dji),
        element2d_dint(Index_k,Index_i,S,Ski),
        element2d_int(Index_k,Index_i,D,Dki),
        no_overlap(Sji,Dji,Ski,Dki).
where_forall2_jk(_Index_i,_Index_j,_Index_k,_S,_D).


