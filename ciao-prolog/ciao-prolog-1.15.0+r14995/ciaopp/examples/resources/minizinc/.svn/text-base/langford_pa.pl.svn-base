:- module(_1,[main/4],[ciaopp(examples(resources(minizinc))),assertions,nativeprops,basicmodes,regtypes,isomodes]).

:- entry main(_1,_2,_3,_4)
         : ( int(_1), int(_2), var(_3), var(_4) ).

:- true pred main(N,K,Pos,Num)
         : ( int(N), int(K), term(Pos), term(Num) )
        => ( int(N), int(K), array(Pos), array(Num) ).

:- true pred main(N,K,Pos,Num)
         : ( mshare([[Pos],[Pos,Num],[Num]]), var(Pos), var(Num), ground([N,K]) )
        => ground([N,K,Pos,Num]).

:- true pred main(N,K,Pos,Num)
         : ( int(N), int(K), var(Pos), var(Num) )
        => ( int(N), int(K), array(Pos), array(Num) )
         + ( not_fails, covered ).

:- true pred main(N,K,Pos,Num)
         : ( int(N), int(K), var(Pos), var(Num) )
        => ( int(N), int(K), array(Pos), array(Num), size(lb,N,int(N)), size(lb,K,int(K)), size(lb,Pos,0), size(lb,Num,0) )
         + ( cost(lb,alldiff,2), cost(lb,constraints,3*(exp(int(K),2)*exp(int(N),2))+int(K)*int(N)-int(N)), cost(lb,numvars,3*(exp(int(K),2)*exp(int(N),2))+3*(int(K)*int(N))-int(N)) ).

:- true pred main(N,K,Pos,Num)
         : ( int(N), int(K), var(Pos), var(Num) )
        => ( int(N), int(K), array(Pos), array(Num), size(ub,N,int(N)), size(ub,K,int(K)), size(ub,Pos,inf), size(ub,Num,inf) )
         + ( cost(ub,alldiff,2), cost(ub,constraints,3*(exp(int(K),2)*exp(int(N),2))+int(K)*int(N)-int(N)), cost(ub,numvars,3*(exp(int(K),2)*exp(int(N),2))+3*(int(K)*int(N))-int(N)) ).

main(N,K,Pos,Num) :-
        minizinc_rt:(Size_Pos is N*K-1+1),
        minizinc_rt:(DU_Pos is N*K),
        build_array(Pos,Size_Pos),
        init_pos_i(Size_Pos,1,DU_Pos,Pos),
        minizinc_rt:(Ind1_i is N-1+1),
        forall1_i(Ind1_i,N,K,Pos),
        all_different_(Pos,Size_Pos),
        minizinc_rt:(Size_Num is N*K-1+1),
        minizinc_rt:(DU_Num is N*K),
        build_array(Num,Size_Num),
        init_num_i(Size_Num,1,DU_Num,Num),
        all_different_(Num,Size_Num),
        minizinc_rt:(Ind2_i is N-1+1),
        forall2_i(Ind2_i,N,N,K,Num).

:- true pred init_pos_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), array(_A) ).

:- true pred init_pos_i(Ind,_DL,_DU,_A)
         : ground([Ind,_DL,_DU,_A])
        => ground([Ind,_DL,_DU,_A]).

:- true pred init_pos_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), array(_A) )
         + ( not_fails, covered ).

:- true pred init_pos_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), array(_A), size(lb,Ind,int(Ind)), size(lb,_DL,int(_DL)), size(lb,_DU,int(_DU)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind)) ).

:- true pred init_pos_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), int(_DU), array(_A), size(ub,Ind,int(Ind)), size(ub,_DL,int(_DL)), size(ub,_DU,int(_DU)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind)) ).

init_pos_i(0,_DL,_DU,_A) :- !.
init_pos_i(Ind,DL,DU,A) :-
        element_dint(Ind,A,Ai),
        in(Ai,DL,DU),
        minizinc_rt:(NInd is Ind-1),
        init_pos_i(NInd,DL,DU,A).

:- true pred forall1_i(Ind_i,_1,_2,_3)
         : ( int(Ind_i), int(_1), int(_2), array(_3) )
        => ( character_code(Ind_i), character_code(_1), character_code(_2), array(_3) ).

:- true pred forall1_i(Ind_i,_1,_2,_3)
         : ground([Ind_i,_1,_2,_3])
        => ground([Ind_i,_1,_2,_3]).

:- true pred forall1_i(Ind_i,_1,_2,_3)
         : ( int(Ind_i), int(_1), int(_2), array(_3) )
        => ( character_code(Ind_i), character_code(_1), character_code(_2), array(_3) )
         + ( not_fails, covered ).

:- true pred forall1_i(Ind_i,_1,_2,_3)
         : ( int(Ind_i), int(_1), int(_2), array(_3) )
        => ( character_code(Ind_i), character_code(_1), character_code(_2), array(_3), size(lb,Ind_i,int(Ind_i)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,int(_2)*int(Ind_i)-int(Ind_i)), cost(lb,numvars,int(_2)*int(Ind_i)-int(Ind_i)) ).

:- true pred forall1_i(Ind_i,_1,_2,_3)
         : ( int(Ind_i), int(_1), int(_2), array(_3) )
        => ( character_code(Ind_i), character_code(_1), character_code(_2), array(_3), size(ub,Ind_i,int(Ind_i)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,int(_2)*int(Ind_i)-int(Ind_i)), cost(ub,numvars,int(_2)*int(Ind_i)-int(Ind_i)) ).

forall1_i(0,_1,_2,_3) :- !.
forall1_i(Ind_i,Ui,K,Pos) :-
        minizinc_rt:(Index_i is Ui-Ind_i+1),
        minizinc_rt:(Uj is K-1),
        minizinc_rt:(Ind_j is Uj-1+1),
        forall1_ij(Ind_j,Uj,Index_i,K,Pos),
        minizinc_rt:(NInd_i is Ind_i-1),
        forall1_i(NInd_i,Ui,K,Pos).

:- true pred forall1_ij(Ind_j,_1,_2,_3,_4)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4) )
        => ( character_code(Ind_j), character_code(_1), character_code(_2), character_code(_3), array(_4) ).

:- true pred forall1_ij(Ind_j,_1,_2,_3,_4)
         : ground([Ind_j,_1,_2,_3,_4])
        => ground([Ind_j,_1,_2,_3,_4]).

:- true pred forall1_ij(Ind_j,_1,_2,_3,_4)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4) )
        => ( character_code(Ind_j), character_code(_1), character_code(_2), character_code(_3), array(_4) )
         + ( not_fails, covered ).

:- true pred forall1_ij(Ind_j,_1,_2,_3,_4)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4) )
        => ( character_code(Ind_j), character_code(_1), character_code(_2), character_code(_3), array(_4), size(lb,Ind_j,int(Ind_j)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,int(Ind_j)), cost(lb,numvars,int(Ind_j)) ).

:- true pred forall1_ij(Ind_j,_1,_2,_3,_4)
         : ( int(Ind_j), int(_1), int(_2), int(_3), array(_4) )
        => ( character_code(Ind_j), character_code(_1), character_code(_2), character_code(_3), array(_4), size(ub,Ind_j,int(Ind_j)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,int(Ind_j)), cost(ub,numvars,int(Ind_j)) ).

forall1_ij(0,_1,_2,_3,_4) :- !.
forall1_ij(Ind_j,Uj,Index_i,K,Pos) :-
        minizinc_rt:(Index_j is Uj-Ind_j+1),
        minizinc_rt:(A is K*(Index_i-1)+Index_j+1),
        minizinc_rt:(B is K*(Index_i-1)+Index_j),
        minizinc_rt:(Ia is A-1+1),
        minizinc_rt:(Ib is B-1+1),
        element(Ia,Pos,Pos_a),
        element(Ib,Pos,Pos_b),
        minus_dint_dint_var(Pos_a,Pos_b,D),
        minizinc_rt:(C is Index_i+1),
        eq(D,C),
        minizinc_rt:(NInd_j is Ind_j-1),
        forall1_ij(NInd_j,Uj,Index_i,K,Pos).

:- true pred init_num_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( character_code(Ind), rt0(_DL), int(_DU), array(_A) ).

:- true pred init_num_i(Ind,_DL,_DU,_A)
         : ground([Ind,_DL,_DU,_A])
        => ground([Ind,_DL,_DU,_A]).

:- true pred init_num_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( character_code(Ind), rt0(_DL), int(_DU), array(_A) )
         + ( not_fails, covered ).

:- true pred init_num_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( character_code(Ind), rt0(_DL), int(_DU), array(_A), size(lb,Ind,int(Ind)), size(lb,_DL,int(_DL)), size(lb,_DU,int(_DU)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind)) ).

:- true pred init_num_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), int(_DU), array(_A) )
        => ( character_code(Ind), rt0(_DL), int(_DU), array(_A), size(ub,Ind,int(Ind)), size(ub,_DL,int(_DL)), size(ub,_DU,int(_DU)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind)) ).

init_num_i(0,_DL,_DU,_A) :- !.
init_num_i(Ind,DL,DU,A) :-
        element_dint(Ind,A,Ai),
        in(Ai,DL,DU),
        minizinc_rt:(NInd is Ind-1),
        init_num_i(NInd,DL,DU,A).

:- true pred forall2_i(Ind_i,_1,_2,_3,_4)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4) )
        => ( character_code(Ind_i), int(_1), int(_2), int(_3), array(_4) ).

:- true pred forall2_i(Ind_i,_1,_2,_3,_4)
         : ground([Ind_i,_1,_2,_3,_4])
        => ground([Ind_i,_1,_2,_3,_4]).

:- true pred forall2_i(Ind_i,_1,_2,_3,_4)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4) )
        => ( character_code(Ind_i), int(_1), int(_2), int(_3), array(_4) )
         + ( not_fails, covered ).

:- true pred forall2_i(Ind_i,_1,_2,_3,_4)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4) )
        => ( character_code(Ind_i), int(_1), int(_2), int(_3), array(_4), size(lb,Ind_i,int(Ind_i)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,3*(exp(int(_3),2)*int(_2)*int(Ind_i))), cost(lb,numvars,3*(exp(int(_3),2)*int(_2)*int(Ind_i))) ).

:- true pred forall2_i(Ind_i,_1,_2,_3,_4)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4) )
        => ( character_code(Ind_i), int(_1), int(_2), int(_3), array(_4), size(ub,Ind_i,int(Ind_i)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,3*(exp(int(_3),2)*int(_2)*int(Ind_i))), cost(ub,numvars,3*(exp(int(_3),2)*int(_2)*int(Ind_i))) ).

forall2_i(0,_1,_2,_3,_4) :- !.
forall2_i(Ind_i,Ui,N,K,Num) :-
        minizinc_rt:(Index_i is Ui-Ind_i+1),
        minizinc_rt:(Ind_j is K-1+1),
        forall2_ij(Ind_j,K,Index_i,N,K,Num),
        minizinc_rt:(NInd_i is Ind_i-1),
        forall2_i(NInd_i,Ui,N,K,Num).

:- true pred forall2_ij(Ind_j,_1,_2,_3,_4,_5)
         : ( int(Ind_j), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( character_code(Ind_j), int(_1), int(_2), int(_3), int(_4), array(_5) ).

:- true pred forall2_ij(Ind_j,_1,_2,_3,_4,_5)
         : ground([Ind_j,_1,_2,_3,_4,_5])
        => ground([Ind_j,_1,_2,_3,_4,_5]).

:- true pred forall2_ij(Ind_j,_1,_2,_3,_4,_5)
         : ( int(Ind_j), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( character_code(Ind_j), int(_1), int(_2), int(_3), int(_4), array(_5) )
         + ( not_fails, covered ).

:- true pred forall2_ij(Ind_j,_1,_2,_3,_4,_5)
         : ( int(Ind_j), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( character_code(Ind_j), int(_1), int(_2), int(_3), int(_4), array(_5), size(lb,Ind_j,int(Ind_j)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,int(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,3*(int(_4)*int(_3)*int(Ind_j))), cost(lb,numvars,3*(int(_4)*int(_3)*int(Ind_j))) ).

:- true pred forall2_ij(Ind_j,_1,_2,_3,_4,_5)
         : ( int(Ind_j), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( character_code(Ind_j), int(_1), int(_2), int(_3), int(_4), array(_5), size(ub,Ind_j,int(Ind_j)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,int(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,3*(int(_4)*int(_3)*int(Ind_j))), cost(ub,numvars,3*(int(_4)*int(_3)*int(Ind_j))) ).

forall2_ij(0,_1,_2,_3,_4,_5) :- !.
forall2_ij(Ind_j,Uj,Index_i,N,K,Num) :-
        minizinc_rt:(Index_j is Uj-Ind_j+1),
        minizinc_rt:(Up is N*K),
        minizinc_rt:(Ind_p is Up-1+1),
        forall2_ijp(Ind_p,Up,Index_i,Index_j,K,Num),
        minizinc_rt:(NInd_j is Ind_j-1),
        forall2_ij(NInd_j,Uj,Index_i,N,K,Num).

:- true pred forall2_ijp(Ind_p,_1,_2,_3,_4,_5)
         : ( int(Ind_p), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( character_code(Ind_p), int(_1), int(_2), int(_3), int(_4), array(_5) ).

:- true pred forall2_ijp(Ind_p,_1,_2,_3,_4,_5)
         : ground([Ind_p,_1,_2,_3,_4,_5])
        => ground([Ind_p,_1,_2,_3,_4,_5]).

:- true pred forall2_ijp(Ind_p,_1,_2,_3,_4,_5)
         : ( int(Ind_p), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( character_code(Ind_p), int(_1), int(_2), int(_3), int(_4), array(_5) )
         + ( not_fails, covered ).

:- true pred forall2_ijp(Ind_p,_1,_2,_3,_4,_5)
         : ( int(Ind_p), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( character_code(Ind_p), int(_1), int(_2), int(_3), int(_4), array(_5), size(lb,Ind_p,int(Ind_p)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,int(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,3*int(Ind_p)), cost(lb,numvars,3*int(Ind_p)) ).

:- true pred forall2_ijp(Ind_p,_1,_2,_3,_4,_5)
         : ( int(Ind_p), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( character_code(Ind_p), int(_1), int(_2), int(_3), int(_4), array(_5), size(ub,Ind_p,int(Ind_p)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,int(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,3*int(Ind_p)), cost(ub,numvars,3*int(Ind_p)) ).

forall2_ijp(0,_1,_2,_3,_4,_5) :- !.
forall2_ijp(Ind_p,Up,Index_i,Index_j,K,Num) :-
        minizinc_rt:(Index_p is Up-Ind_p+1),
        minizinc_rt:(A is K*(Index_i-1)+Index_j),
        minizinc_rt:(Ia is A-1+1),
        element(Ia,Num,Num_a),
        reif_eq_dint_int_var(Num_a,Index_p,B1),
        minizinc_rt:(Ip is Index_p-1+1),
        element(Ip,Num,Num_p),
        reif_eq_dint_int_var(Num_p,A,B2),
        equiv(B1,B2,1),
        minizinc_rt:(NInd_p is Ind_p-1),
        forall2_ijp(NInd_p,Up,Index_i,Index_j,K,Num).


:- regtype rt0/1.

rt0(1).


