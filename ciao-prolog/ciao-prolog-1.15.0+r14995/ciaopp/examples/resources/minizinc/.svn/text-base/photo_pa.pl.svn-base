:- module(_1,[main/6],[ciaopp(examples(resources(minizinc))),assertions,nativeprops,basicmodes,regtypes,isomodes]).

:- entry main(_1,_2,_3,_4,_5,_6)
         : ( int(_1), int(_2), array(_3), var(_4), var(_5), var(_6) ).

:- true pred main(N_names,N_prefs,Prefs,Ful,Pos,Sat)
         : ( int(N_names), int(N_prefs), array(Prefs), term(Ful), term(Pos), term(Sat) )
        => ( int(N_names), int(N_prefs), array(Prefs), array(Ful), array(Pos), int(Sat) ).

:- true pred main(N_names,N_prefs,Prefs,Ful,Pos,Sat)
         : ( mshare([[Ful],[Ful,Pos],[Ful,Pos,Sat],[Ful,Sat],[Pos],[Pos,Sat],[Sat]]), var(Ful), var(Pos), var(Sat), ground([N_names,N_prefs,Prefs]) )
        => ground([N_names,N_prefs,Prefs,Ful,Pos,Sat]).

:- true pred main(N_names,N_prefs,Prefs,Ful,Pos,Sat)
         : ( int(N_names), int(N_prefs), array(Prefs), var(Ful), var(Pos), var(Sat) )
        => ( int(N_names), int(N_prefs), array(Prefs), array(Ful), array(Pos), int(Sat) )
         + ( not_fails, covered ).

:- true pred main(N_names,N_prefs,Prefs,Ful,Pos,Sat)
         : ( int(N_names), int(N_prefs), array(Prefs), var(Ful), var(Pos), var(Sat) )
        => ( int(N_names), int(N_prefs), array(Prefs), array(Ful), array(Pos), int(Sat), size(lb,N_names,int(N_names)), size(lb,N_prefs,int(N_prefs)), size(lb,Prefs,size(Prefs)), size(lb,Ful,0), size(lb,Pos,0), size(lb,Sat,0) )
         + ( cost(lb,alldiff,1), cost(lb,constraints,5*int(N_prefs)+1), cost(lb,numvars,6*int(N_prefs)+int(N_names)) ).

:- true pred main(N_names,N_prefs,Prefs,Ful,Pos,Sat)
         : ( int(N_names), int(N_prefs), array(Prefs), var(Ful), var(Pos), var(Sat) )
        => ( int(N_names), int(N_prefs), array(Prefs), array(Ful), array(Pos), int(Sat), size(ub,N_names,int(N_names)), size(ub,N_prefs,int(N_prefs)), size(ub,Prefs,size(Prefs)), size(ub,Ful,inf), size(ub,Pos,inf), size(ub,Sat,bot) )
         + ( cost(ub,alldiff,1), cost(ub,constraints,5*int(N_prefs)+1), cost(ub,numvars,6*int(N_prefs)+int(N_names)) ).

main(N_names,N_prefs,Prefs,Ful,Pos,Sat) :-
        minizinc_rt:(U_Pos is N_names-1),
        minizinc_rt:(Size_Pos is U_Pos-0+1),
        minizinc_rt:(DU_Pos is N_names-1),
        build_array(Pos,Size_Pos),
        init_pos_i(Size_Pos,0,DU_Pos,Pos),
        minizinc_rt:(Size_Ful is N_prefs-1+1),
        build_array(Ful,Size_Ful),
        init_ful_i(Size_Ful,0,1,Ful),
        minizinc_rt:(Ind_i is N_prefs-1+1),
        forall_i(Ind_i,N_prefs,N_names,N_prefs,Prefs,Pos,Ful),
        minizinc_rt:(Ind_sum is N_prefs-1+1),
        sum_i(Ind_sum,N_prefs,Ful,Sum),
        eq_int(Sum,Sat),
        all_different_(Pos,Size_Pos),
        minizinc_rt:(I0 is 0-1+1),
        element(I0,Pos,Pos_0),
        minizinc_rt:(I1 is 1-1+1),
        element(I1,Pos,Pos_1),
        less(Pos_0,Pos_1).

:- true pred sum_i(Ind,_1,_2,Sum)
         : ( int(Ind), int(_1), array(_2), term(Sum) )
        => ( int(Ind), int(_1), array(_2), int(Sum) ).

:- true pred sum_i(Ind,_1,_2,Sum)
         : ( mshare([[Sum]]), var(Sum), ground([Ind,_1,_2]) )
        => ground([Ind,_1,_2,Sum]).

:- true pred sum_i(Ind,_1,_2,Sum)
         : ( int(Ind), int(_1), array(_2), var(Sum) )
        => ( int(Ind), int(_1), array(_2), int(Sum) )
         + ( not_fails, covered ).

:- true pred sum_i(Ind,_1,_2,Sum)
         : ( int(Ind), int(_1), array(_2), var(Sum) )
        => ( int(Ind), int(_1), array(_2), int(Sum), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,size(_2)), size(lb,Sum,0) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred sum_i(Ind,_1,_2,Sum)
         : ( int(Ind), int(_1), array(_2), var(Sum) )
        => ( int(Ind), int(_1), array(_2), int(Sum), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,size(_2)), size(ub,Sum,inf) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,0) ).

sum_i(0,_1,_2,0) :- !.
sum_i(Ind,U,Ful,Sum) :-
        minizinc_rt:(Index_i is U-Ind+1),
        minizinc_rt:(I is Index_i-1+1),
        element_int(I,Ful,Ful_i),
        minizinc_rt:(NInd is Ind-1),
        sum_i(NInd,U,Ful,NSum),
        plus_int(Ful_i,NSum,Sum).

:- true pred init_pos_i(Ind_i,_DL,_DU,_A)
         : ( int(Ind_i), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind_i), rt0(_DL), int(_DU), array(_A) ).

:- true pred init_pos_i(Ind_i,_DL,_DU,_A)
         : ground([Ind_i,_DL,_DU,_A])
        => ground([Ind_i,_DL,_DU,_A]).

:- true pred init_pos_i(Ind_i,_DL,_DU,_A)
         : ( int(Ind_i), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind_i), rt0(_DL), int(_DU), array(_A) )
         + ( not_fails, covered ).

:- true pred init_pos_i(Ind_i,_DL,_DU,_A)
         : ( int(Ind_i), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind_i), rt0(_DL), int(_DU), array(_A), size(lb,Ind_i,int(Ind_i)), size(lb,_DL,int(_DL)), size(lb,_DU,int(_DU)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind_i)) ).

:- true pred init_pos_i(Ind_i,_DL,_DU,_A)
         : ( int(Ind_i), rt0(_DL), int(_DU), array(_A) )
        => ( int(Ind_i), rt0(_DL), int(_DU), array(_A), size(ub,Ind_i,int(Ind_i)), size(ub,_DL,int(_DL)), size(ub,_DU,int(_DU)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind_i)) ).

init_pos_i(0,_DL,_DU,_A) :- !.
init_pos_i(Ind_i,DL,DU,A) :-
        element_dint(Ind_i,A,Ai),
        in(Ai,DL,DU),
        minizinc_rt:(NInd_i is Ind_i-1),
        init_pos_i(NInd_i,DL,DU,A).

:- true pred init_ful_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), rt5(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), rt5(_DU), array(_A) ).

:- true pred init_ful_i(Ind,_DL,_DU,_A)
         : ground([Ind,_DL,_DU,_A])
        => ground([Ind,_DL,_DU,_A]).

:- true pred init_ful_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), rt5(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), rt5(_DU), array(_A) )
         + ( not_fails, covered ).

:- true pred init_ful_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), rt5(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), rt5(_DU), array(_A), size(lb,Ind,int(Ind)), size(lb,_DL,int(_DL)), size(lb,_DU,int(_DU)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind)) ).

:- true pred init_ful_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), rt5(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), rt5(_DU), array(_A), size(ub,Ind,int(Ind)), size(ub,_DL,int(_DL)), size(ub,_DU,int(_DU)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind)) ).

init_ful_i(0,_DL,_DU,_A) :- !.
init_ful_i(Ind,DL,DU,A) :-
        element_dint(Ind,A,Ai),
        in(Ai,DL,DU),
        minizinc_rt:(NInd is Ind-1),
        init_ful_i(NInd,DL,DU,A).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6) )
        => ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6) ).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6)
         : ground([Ind_i,_1,_2,_3,_4,_5,_6])
        => ground([Ind_i,_1,_2,_3,_4,_5,_6]).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6) )
        => ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6) )
         + ( not_fails, covered ).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6) )
        => ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), size(lb,Ind_i,int(Ind_i)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)), size(lb,_6,size(_6)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,5*int(Ind_i)), cost(lb,numvars,5*int(Ind_i)) ).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6) )
        => ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), size(ub,Ind_i,int(Ind_i)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)), size(ub,_6,size(_6)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,5*int(Ind_i)), cost(ub,numvars,5*int(Ind_i)) ).

forall_i(0,_1,_2,_3,_4,_5,_6) :- !.
forall_i(Ind_i,Ui,N_names,N_prefs,Prefs,Pos,Ful) :-
        minizinc_rt:(Index_i is Ui-Ind_i+1),
        minizinc_rt:(I is Index_i-1+1),
        minizinc_rt:(J0 is 0-0+1),
        element2d_int(I,J0,Prefs,Prefs_i_0),
        minizinc_rt:(J1 is 1-0+1),
        element2d_int(I,J1,Prefs,Prefs_i_1),
        type_decl_int(Pa),
        Pa=Prefs_i_0,
        type_decl_int(Pb),
        Pb=Prefs_i_1,
        element(Pa,Pos,Pos_a),
        element(Pb,Pos,Pos_b),
        minus_dint_dint_var(Pos_b,Pos_a,Dab),
        reif_eq_dint_int_var(Dab,1,Rab),
        minus_dint_dint_var(Pos_a,Pos_b,Dba),
        reif_eq_dint_int_var(Dba,1,Rba),
        xor_dint_dint_var(Rab,Rba,Xor),
        element_dint(I,Ful,Ful_i),
        eq_dint(Ful_i,Xor),
        minizinc_rt:(NInd_i is Ind_i-1),
        forall_i(NInd_i,Ui,N_names,N_prefs,Prefs,Pos,Ful).


:- regtype rt0/1.

rt0(0).


:- regtype rt5/1.

rt5(1).


