:- module(_1,[main/2],[ciaopp(examples(resources(minizinc))),assertions,nativeprops,basicmodes,regtypes,isomodes]).

:- entry main(_1,_2)
         : ( int(_1), var(_2) ).

:- true pred main(S,P)
         : ( int(S), term(P) )
        => ( int(S), array(P) ).

:- true pred main(S,P)
         : ( mshare([[P]]), var(P), ground([S]) )
        => ground([S,P]).

:- true pred main(S,P)
         : ( int(S), var(P) )
        => ( int(S), array(P) )
         + ( not_fails, covered ).

:- true pred main(S,P)
         : ( int(S), var(P) )
        => ( int(S), array(P), size(lb,S,int(S)), size(lb,P,0) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,exp(exp(int(S),2),3)-exp(int(S),4)), cost(lb,numvars,exp(int(S),4)) ).

:- true pred main(S,P)
         : ( int(S), var(P) )
        => ( int(S), array(P), size(ub,S,int(S)), size(ub,P,inf) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,exp(exp(int(S),2),3)+exp(int(S),6)-exp(int(S),4)), cost(ub,numvars,exp(int(S),4)) ).

main(S,P) :-
        minizinc_rt:(N is S*S),
        minizinc_rt:(Size is N-1+1),
        build_array(P,Size),
        init_i(Size,N,N,P),
        minizinc_rt:(Index1 is N-1+1),
        forall1_i(Index1,N,P),
        minizinc_rt:(Index2 is S-1+1),
        forall2_a(Index2,S,P).

:- true pred init_i(Ind,_DU,_Uj,_A)
         : ( int(Ind), int(_DU), int(_Uj), array(_A) )
        => ( int(Ind), int(_DU), int(_Uj), array(_A) ).

:- true pred init_i(Ind,_DU,_Uj,_A)
         : ground([Ind,_DU,_Uj,_A])
        => ground([Ind,_DU,_Uj,_A]).

:- true pred init_i(Ind,_DU,_Uj,_A)
         : ( int(Ind), int(_DU), int(_Uj), array(_A) )
        => ( int(Ind), int(_DU), int(_Uj), array(_A) )
         + ( not_fails, covered ).

:- true pred init_i(Ind,_DU,_Uj,_A)
         : ( int(Ind), int(_DU), int(_Uj), array(_A) )
        => ( int(Ind), int(_DU), int(_Uj), array(_A), size(lb,Ind,int(Ind)), size(lb,_DU,int(_DU)), size(lb,_Uj,int(_Uj)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(_Uj)*int(Ind)) ).

:- true pred init_i(Ind,_DU,_Uj,_A)
         : ( int(Ind), int(_DU), int(_Uj), array(_A) )
        => ( int(Ind), int(_DU), int(_Uj), array(_A), size(ub,Ind,int(Ind)), size(ub,_DU,int(_DU)), size(ub,_Uj,int(_Uj)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(_Uj)*int(Ind)) ).

init_i(0,_DU,_Uj,_A) :- !.
init_i(Ind,DU,Uj,A) :-
        element_array(Ind,A,Ai),
        minizinc_rt:(Size is Uj-1+1),
        build_array(Ai,Size),
        init_ij(Size,DU,Ai),
        minizinc_rt:(NInd is Ind-1),
        init_i(NInd,DU,Uj,A).

:- true pred init_ij(Ind,_DU,_A)
         : ( int(Ind), int(_DU), array(_A) )
        => ( int(Ind), int(_DU), array(_A) ).

:- true pred init_ij(Ind,_DU,_A)
         : ground([Ind,_DU,_A])
        => ground([Ind,_DU,_A]).

:- true pred init_ij(Ind,_DU,_A)
         : ( int(Ind), int(_DU), array(_A) )
        => ( int(Ind), int(_DU), array(_A) )
         + ( not_fails, covered ).

:- true pred init_ij(Ind,_DU,_A)
         : ( int(Ind), int(_DU), array(_A) )
        => ( int(Ind), int(_DU), array(_A), size(lb,Ind,int(Ind)), size(lb,_DU,int(_DU)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind)) ).

:- true pred init_ij(Ind,_DU,_A)
         : ( int(Ind), int(_DU), array(_A) )
        => ( int(Ind), int(_DU), array(_A), size(ub,Ind,int(Ind)), size(ub,_DU,int(_DU)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind)) ).

init_ij(0,_DU,_A) :- !.
init_ij(Ind,DU,A) :-
        element_dint(Ind,A,Ai),
        in(Ai,1,DU),
        minizinc_rt:(NInd is Ind-1),
        init_ij(NInd,DU,A).

:- true pred forall1_i(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2) ).

:- true pred forall1_i(Ind,_1,_2)
         : ground([Ind,_1,_2])
        => ground([Ind,_1,_2]).

:- true pred forall1_i(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2) )
         + ( not_fails, covered ).

:- true pred forall1_i(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,size(_2)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,exp(int(_1),2)*int(Ind)-int(_1)*int(Ind)), cost(lb,numvars,0) ).

:- true pred forall1_i(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,size(_2)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,exp(int(_1),2)*int(Ind)-int(_1)*int(Ind)), cost(ub,numvars,0) ).

forall1_i(0,_1,_2) :- !.
forall1_i(Ind,U,P) :-
        minizinc_rt:(Index_i is U-Ind+1),
        minizinc_rt:(Ind_j is U-1+1),
        forall1_ij(Ind_j,U,Index_i,P),
        minizinc_rt:(NInd is Ind-1),
        forall1_i(NInd,U,P).

:- true pred forall1_ij(Ind,_1,_2,_3)
         : ( int(Ind), int(_1), int(_2), array(_3) )
        => ( int(Ind), int(_1), int(_2), array(_3) ).

:- true pred forall1_ij(Ind,_1,_2,_3)
         : ground([Ind,_1,_2,_3])
        => ground([Ind,_1,_2,_3]).

:- true pred forall1_ij(Ind,_1,_2,_3)
         : ( int(Ind), int(_1), int(_2), array(_3) )
        => ( int(Ind), int(_1), int(_2), array(_3) )
         + ( not_fails, covered ).

:- true pred forall1_ij(Ind,_1,_2,_3)
         : ( int(Ind), int(_1), int(_2), array(_3) )
        => ( int(Ind), int(_1), int(_2), array(_3), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,-exp(int(Ind),2)+2*(int(_1)*int(Ind))-int(Ind)), cost(lb,numvars,0) ).

:- true pred forall1_ij(Ind,_1,_2,_3)
         : ( int(Ind), int(_1), int(_2), array(_3) )
        => ( int(Ind), int(_1), int(_2), array(_3), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,-exp(int(Ind),2)+2*(int(_1)*int(Ind))-int(Ind)), cost(ub,numvars,0) ).

forall1_ij(0,_1,_2,_3) :- !.
forall1_ij(Ind,U,Index_i,P) :-
        minizinc_rt:(Index_j is U-Ind+1),
        minizinc_rt:(Uk is Index_j-1),
        minizinc_rt:(Ind_k is Uk-1+1),
        forall1_ijk(Ind_k,Uk,Index_i,Index_j,P),
        minizinc_rt:(NInd is Ind-1),
        forall1_ij(NInd,U,Index_i,P).

:- true pred forall1_ijk(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4) ).

:- true pred forall1_ijk(Ind,_1,_2,_3,_4)
         : ground([Ind,_1,_2,_3,_4])
        => ground([Ind,_1,_2,_3,_4]).

:- true pred forall1_ijk(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
         + ( not_fails, covered ).

:- true pred forall1_ijk(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,2*int(Ind)), cost(lb,numvars,0) ).

:- true pred forall1_ijk(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,2*int(Ind)), cost(ub,numvars,0) ).

forall1_ijk(0,_1,_2,_3,_4) :- !.
forall1_ijk(Ind,Uk,Index_i,Index_j,P) :-
        minizinc_rt:(Index_k is Uk-Ind+1),
        minizinc_rt:(I is Index_i-1+1),
        minizinc_rt:(J is Index_j-1+1),
        minizinc_rt:(K is Index_k-1+1),
        element_array(I,P,Pi),
        element_dint(J,Pi,Pij),
        element_dint(K,Pi,Pik),
        neq(Pij,Pik),
        element_array(J,P,Pj),
        element_dint(I,Pj,Pji),
        element_array(K,P,Pk),
        element_dint(I,Pk,Pki),
        neq(Pji,Pki),
        minizinc_rt:(NInd is Ind-1),
        forall1_ijk(NInd,Uk,Index_i,Index_j,P).

:- true pred forall2_a(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2) ).

:- true pred forall2_a(Ind,_1,_2)
         : ground([Ind,_1,_2])
        => ground([Ind,_1,_2]).

:- true pred forall2_a(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2) )
         + ( not_fails, covered ).

:- true pred forall2_a(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,size(_2)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_a(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,size(_2)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,exp(int(_1),5)*int(Ind)), cost(ub,numvars,0) ).

forall2_a(0,_1,_2) :- !.
forall2_a(Ind,S,P) :-
        minizinc_rt:(Index_a is S-Ind+1),
        minizinc_rt:(Ind_o is S-1+1),
        forall2_ao(Ind_o,S,Index_a,P),
        minizinc_rt:(NInd is Ind-1),
        forall2_a(NInd,S,P).

:- true pred forall2_ao(Ind,_1,_2,_3)
         : ( int(Ind), int(_1), int(_2), array(_3) )
        => ( int(Ind), int(_1), int(_2), array(_3) ).

:- true pred forall2_ao(Ind,_1,_2,_3)
         : ground([Ind,_1,_2,_3])
        => ground([Ind,_1,_2,_3]).

:- true pred forall2_ao(Ind,_1,_2,_3)
         : ( int(Ind), int(_1), int(_2), array(_3) )
        => ( int(Ind), int(_1), int(_2), array(_3) )
         + ( not_fails, covered ).

:- true pred forall2_ao(Ind,_1,_2,_3)
         : ( int(Ind), int(_1), int(_2), array(_3) )
        => ( int(Ind), int(_1), int(_2), array(_3), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_ao(Ind,_1,_2,_3)
         : ( int(Ind), int(_1), int(_2), array(_3) )
        => ( int(Ind), int(_1), int(_2), array(_3), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,exp(int(_1),4)*int(Ind)), cost(ub,numvars,0) ).

forall2_ao(0,_1,_2,_3) :- !.
forall2_ao(Ind,S,Index_a,P) :-
        minizinc_rt:(Index_o is S-Ind+1),
        minizinc_rt:(Ind_a1 is S-1+1),
        forall2_aoa1(Ind_a1,S,Index_a,Index_o,P),
        minizinc_rt:(NInd is Ind-1),
        forall2_ao(NInd,S,Index_a,P).

:- true pred forall2_aoa1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4) ).

:- true pred forall2_aoa1(Ind,_1,_2,_3,_4)
         : ground([Ind,_1,_2,_3,_4])
        => ground([Ind,_1,_2,_3,_4]).

:- true pred forall2_aoa1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
         + ( not_fails, covered ).

:- true pred forall2_aoa1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_aoa1(Ind,_1,_2,_3,_4)
         : ( int(Ind), int(_1), int(_2), int(_3), array(_4) )
        => ( int(Ind), int(_1), int(_2), int(_3), array(_4), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,exp(int(_1),3)*int(Ind)), cost(ub,numvars,0) ).

forall2_aoa1(0,_1,_2,_3,_4) :- !.
forall2_aoa1(Ind,S,Index_a,Index_o,P) :-
        minizinc_rt:(Index_a1 is S-Ind+1),
        minizinc_rt:(Ind_o1 is S-1+1),
        forall2_aoa1o1(Ind_o1,S,Index_a,Index_o,Index_a1,P),
        minizinc_rt:(NInd is Ind-1),
        forall2_aoa1(NInd,S,Index_a,Index_o,P).

:- true pred forall2_aoa1o1(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), array(_5) ).

:- true pred forall2_aoa1o1(Ind,_1,_2,_3,_4,_5)
         : ground([Ind,_1,_2,_3,_4,_5])
        => ground([Ind,_1,_2,_3,_4,_5]).

:- true pred forall2_aoa1o1(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), array(_5) )
         + ( not_fails, covered ).

:- true pred forall2_aoa1o1(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), array(_5), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,int(_4)), size(lb,_5,size(_5)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_aoa1o1(Ind,_1,_2,_3,_4,_5)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), array(_5) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), array(_5), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,int(_4)), size(ub,_5,size(_5)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,exp(int(_1),2)*int(Ind)), cost(ub,numvars,0) ).

forall2_aoa1o1(0,_1,_2,_3,_4,_5) :- !.
forall2_aoa1o1(Ind,S,Index_a,Index_o,Index_a1,P) :-
        minizinc_rt:(Index_o1 is S-Ind+1),
        minizinc_rt:(Ind_a2 is S-1+1),
        forall2_aoa1o1a2(Ind_a2,S,Index_a,Index_o,Index_a1,Index_o1,P),
        minizinc_rt:(NInd is Ind-1),
        forall2_aoa1o1(NInd,S,Index_a,Index_o,Index_a1,P).

:- true pred forall2_aoa1o1a2(Ind,_1,_2,_3,_4,_5,_6)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6) ).

:- true pred forall2_aoa1o1a2(Ind,_1,_2,_3,_4,_5,_6)
         : ground([Ind,_1,_2,_3,_4,_5,_6])
        => ground([Ind,_1,_2,_3,_4,_5,_6]).

:- true pred forall2_aoa1o1a2(Ind,_1,_2,_3,_4,_5,_6)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6) )
         + ( not_fails, covered ).

:- true pred forall2_aoa1o1a2(Ind,_1,_2,_3,_4,_5,_6)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,int(_4)), size(lb,_5,int(_5)), size(lb,_6,size(_6)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_aoa1o1a2(Ind,_1,_2,_3,_4,_5,_6)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,int(_4)), size(ub,_5,int(_5)), size(ub,_6,size(_6)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,int(_1)*int(Ind)), cost(ub,numvars,0) ).

forall2_aoa1o1a2(0,_1,_2,_3,_4,_5,_6) :- !.
forall2_aoa1o1a2(Ind,S,Index_a,Index_o,Index_a1,Index_o1,P) :-
        minizinc_rt:(Index_a2 is S-Ind+1),
        'forall2_aoa1o1a2/7/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2),
        minizinc_rt:(NInd is Ind-1),
        forall2_aoa1o1a2(NInd,S,Index_a,Index_o,Index_a1,Index_o1,P).

:- true pred 'forall2_aoa1o1a2/7/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2)
         : ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), array(P), int(Index_a2) )
        => ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), array(P), int(Index_a2) ).

:- true pred 'forall2_aoa1o1a2/7/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2)
         : ground([S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2])
        => ground([S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2]).

:- true pred 'forall2_aoa1o1a2/7/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2)
         : ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), array(P), int(Index_a2) )
        => ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), array(P), int(Index_a2) )
         + ( not_fails, covered ).

:- true pred 'forall2_aoa1o1a2/7/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2)
         : ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), array(P), int(Index_a2) )
        => ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), array(P), int(Index_a2), size(lb,S,int(S)), size(lb,Index_a,int(Index_a)), size(lb,Index_o,int(Index_o)), size(lb,Index_a1,int(Index_a1)), size(lb,Index_o1,int(Index_o1)), size(lb,P,size(P)), size(lb,Index_a2,int(Index_a2)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred 'forall2_aoa1o1a2/7/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2)
         : ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), array(P), int(Index_a2) )
        => ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), array(P), int(Index_a2), size(ub,S,int(S)), size(ub,Index_a,int(Index_a)), size(ub,Index_o,int(Index_o)), size(ub,Index_a1,int(Index_a1)), size(ub,Index_o1,int(Index_o1)), size(ub,P,size(P)), size(ub,Index_a2,int(Index_a2)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,int(S)), cost(ub,numvars,0) ).

'forall2_aoa1o1a2/7/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2) :-
        Index_a2=\=Index_a1,
        !,
        minizinc_rt:(Ind_o2 is S-1+1),
        forall2_aoa1o1a2o2(Ind_o2,S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P).
'forall2_aoa1o1a2/7/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,P,Index_a2).

:- true pred forall2_aoa1o1a2o2(Ind,_1,_2,_3,_4,_5,_6,_7)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7) ).

:- true pred forall2_aoa1o1a2o2(Ind,_1,_2,_3,_4,_5,_6,_7)
         : ground([Ind,_1,_2,_3,_4,_5,_6,_7])
        => ground([Ind,_1,_2,_3,_4,_5,_6,_7]).

:- true pred forall2_aoa1o1a2o2(Ind,_1,_2,_3,_4,_5,_6,_7)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7) )
         + ( not_fails, covered ).

:- true pred forall2_aoa1o1a2o2(Ind,_1,_2,_3,_4,_5,_6,_7)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,int(_4)), size(lb,_5,int(_5)), size(lb,_6,int(_6)), size(lb,_7,size(_7)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred forall2_aoa1o1a2o2(Ind,_1,_2,_3,_4,_5,_6,_7)
         : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7) )
        => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,int(_4)), size(ub,_5,int(_5)), size(ub,_6,int(_6)), size(ub,_7,size(_7)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,int(Ind)), cost(ub,numvars,0) ).

forall2_aoa1o1a2o2(0,_1,_2,_3,_4,_5,_6,_7) :- !.
forall2_aoa1o1a2o2(Ind,S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P) :-
        minizinc_rt:(Index_o2 is S-Ind+1),
        'forall2_aoa1o1a2o2/8/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2),
        minizinc_rt:(NInd is Ind-1),
        forall2_aoa1o1a2o2(NInd,S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P).

:- true pred 'forall2_aoa1o1a2o2/8/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2)
         : ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), int(Index_a2), array(P), int(Index_o2) )
        => ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), int(Index_a2), array(P), int(Index_o2) ).

:- true pred 'forall2_aoa1o1a2o2/8/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2)
         : ground([S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2])
        => ground([S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2]).

:- true pred 'forall2_aoa1o1a2o2/8/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2)
         : ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), int(Index_a2), array(P), int(Index_o2) )
        => ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), int(Index_a2), array(P), int(Index_o2) )
         + ( not_fails, covered ).

:- true pred 'forall2_aoa1o1a2o2/8/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2)
         : ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), int(Index_a2), array(P), int(Index_o2) )
        => ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), int(Index_a2), array(P), int(Index_o2), size(lb,S,int(S)), size(lb,Index_a,int(Index_a)), size(lb,Index_o,int(Index_o)), size(lb,Index_a1,int(Index_a1)), size(lb,Index_o1,int(Index_o1)), size(lb,Index_a2,int(Index_a2)), size(lb,P,size(P)), size(lb,Index_o2,int(Index_o2)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred 'forall2_aoa1o1a2o2/8/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2)
         : ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), int(Index_a2), array(P), int(Index_o2) )
        => ( int(S), int(Index_a), int(Index_o), int(Index_a1), int(Index_o1), int(Index_a2), array(P), int(Index_o2), size(ub,S,int(S)), size(ub,Index_a,int(Index_a)), size(ub,Index_o,int(Index_o)), size(ub,Index_a1,int(Index_a1)), size(ub,Index_o1,int(Index_o1)), size(ub,Index_a2,int(Index_a2)), size(ub,P,size(P)), size(ub,Index_o2,int(Index_o2)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,1), cost(ub,numvars,0) ).

'forall2_aoa1o1a2o2/8/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2) :-
        Index_o2=\=Index_o1,
        !,
        minizinc_rt:(Indexi1 is(Index_a-1)*S+Index_a1),
        minizinc_rt:(Indexj1 is(Index_o-1)*S+Index_o1),
        minizinc_rt:(Indexi2 is(Index_a-1)*S+Index_a2),
        minizinc_rt:(Indexj2 is(Index_o-1)*S+Index_o2),
        minizinc_rt:(I1 is Indexi1-1+1),
        minizinc_rt:(J1 is Indexj1-1+1),
        minizinc_rt:(I2 is Indexi2-1+1),
        minizinc_rt:(J2 is Indexj2-1+1),
        element2d_dint(I1,J1,P,P1ij),
        element2d_dint(I2,J2,P,P2ij),
        neq_dint_dint(P1ij,P2ij).
'forall2_aoa1o1a2o2/8/2/$disj/1'(S,Index_a,Index_o,Index_a1,Index_o1,Index_a2,P,Index_o2).


