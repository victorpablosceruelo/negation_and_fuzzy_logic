:- module(_1,[main/2],[ciaopp(examples(resources(minizinc))),assertions,nativeprops,basicmodes,regtypes,isomodes]).

:- entry main(_1,_2)
         : ( int(_1), var(_2) ).

:- true pred main(N,Q)
         : ( int(N), term(Q) )
        => ( int(N), array(Q) ).

:- true pred main(N,Q)
         : ( mshare([[Q]]), var(Q), ground([N]) )
        => ground([N,Q]).

:- true pred main(N,Q)
         : ( int(N), var(Q) )
        => ( int(N), array(Q) )
         + ( not_fails, covered ).

:- true pred main(N,Q)
         : ( int(N), var(Q) )
        => ( int(N), array(Q), size(lb,N,int(N)), size(lb,Q,0) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,3.5*exp(int(N),2)-3.5*int(N)), cost(lb,numvars,2.0*exp(int(N),2)-int(N)) ).

:- true pred main(N,Q)
         : ( int(N), var(Q) )
        => ( int(N), array(Q), size(ub,N,int(N)), size(ub,Q,inf) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,3.5*exp(int(N),2)-3.5*int(N)), cost(ub,numvars,2.0*exp(int(N),2)-int(N)) ).

main(N,Q) :-
        minizinc_rt:(Size is N-1+1),
        build_array(Q,Size),
        init_q(Size,N,Q),
        minizinc_rt:(Index1 is N-1+1),
        forall_i(Index1,N,Q).

:- true pred init_q(Ind,_DU,_A)
         : ( int(Ind), int(_DU), array(_A) )
        => ( int(Ind), int(_DU), array(_A) ).

:- true pred init_q(Ind,_DU,_A)
         : ground([Ind,_DU,_A])
        => ground([Ind,_DU,_A]).

:- true pred init_q(Ind,_DU,_A)
         : ( int(Ind), int(_DU), array(_A) )
        => ( int(Ind), int(_DU), array(_A) )
         + ( not_fails, covered ).

:- true pred init_q(Ind,_DU,_A)
         : ( int(Ind), int(_DU), array(_A) )
        => ( int(Ind), int(_DU), array(_A), size(lb,Ind,int(Ind)), size(lb,_DU,int(_DU)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind)) ).

:- true pred init_q(Ind,_DU,_A)
         : ( int(Ind), int(_DU), array(_A) )
        => ( int(Ind), int(_DU), array(_A), size(ub,Ind,int(Ind)), size(ub,_DU,int(_DU)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind)) ).

init_q(0,_DU,_A) :- !.
init_q(Ind,DU,A) :-
        element_dint(Ind,A,Ai),
        in(Ai,1,DU),
        minizinc_rt:(NInd is Ind-1),
        init_q(NInd,DU,A).

:- true pred forall_i(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2) ).

:- true pred forall_i(Ind,_1,_2)
         : ground([Ind,_1,_2])
        => ground([Ind,_1,_2]).

:- true pred forall_i(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2) )
         + ( not_fails, covered ).

:- true pred forall_i(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,size(_2)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,3.5*exp(int(Ind),2)-3.5*int(Ind)), cost(lb,numvars,2.0*exp(int(Ind),2)-2.0*int(Ind)) ).

:- true pred forall_i(Ind,_1,_2)
         : ( int(Ind), int(_1), array(_2) )
        => ( int(Ind), int(_1), array(_2), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,size(_2)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,3.5*exp(int(Ind),2)-3.5*int(Ind)), cost(ub,numvars,2.0*exp(int(Ind),2)-2.0*int(Ind)) ).

forall_i(0,_1,_2) :- !.
forall_i(Ind,Uij,A) :-
        minizinc_rt:(Index_i is Uij-Ind+1),
        minizinc_rt:(Ind_j is Uij-(Index_i+1)+1),
        forall_ij(Ind_j,Uij,Index_i,A),
        minizinc_rt:(NInd is Ind-1),
        forall_i(NInd,Uij,A).

:- true pred forall_ij(Ind_j,_1,_2,_3)
         : ( int(Ind_j), int(_1), int(_2), array(_3) )
        => ( int(Ind_j), int(_1), int(_2), array(_3) ).

:- true pred forall_ij(Ind_j,_1,_2,_3)
         : ground([Ind_j,_1,_2,_3])
        => ground([Ind_j,_1,_2,_3]).

:- true pred forall_ij(Ind_j,_1,_2,_3)
         : ( int(Ind_j), int(_1), int(_2), array(_3) )
        => ( int(Ind_j), int(_1), int(_2), array(_3) )
         + ( not_fails, covered ).

:- true pred forall_ij(Ind_j,_1,_2,_3)
         : ( int(Ind_j), int(_1), int(_2), array(_3) )
        => ( int(Ind_j), int(_1), int(_2), array(_3), size(lb,Ind_j,int(Ind_j)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,7*int(Ind_j)), cost(lb,numvars,4*int(Ind_j)) ).

:- true pred forall_ij(Ind_j,_1,_2,_3)
         : ( int(Ind_j), int(_1), int(_2), array(_3) )
        => ( int(Ind_j), int(_1), int(_2), array(_3), size(ub,Ind_j,int(Ind_j)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,7*int(Ind_j)), cost(ub,numvars,4*int(Ind_j)) ).

forall_ij(0,_1,_2,_3) :- !.
forall_ij(Ind_j,Uij,Index_i,A) :-
        minizinc_rt:(Index_j is Uij-Ind_j+1),
        minizinc_rt:(I is Index_i-1+1),
        element(I,A,A_i),
        minizinc_rt:(J is Index_j-1+1),
        element(J,A,A_j),
        no_attack(Index_i,Index_j,A_i,A_j),
        minizinc_rt:(NInd_j is Ind_j-1),
        forall_ij(NInd_j,Uij,Index_i,A).

:- true pred no_attack(I,J,QI,QJ)
         : ( int(I), int(J), dint(QI), dint(QJ) )
        => ( int(I), int(J), dint(QI), dint(QJ) ).

:- true pred no_attack(I,J,QI,QJ)
         : ground([I,J,QI,QJ])
        => ground([I,J,QI,QJ]).

:- true pred no_attack(I,J,QI,QJ)
         : ( int(I), int(J), dint(QI), dint(QJ) )
        => ( int(I), int(J), dint(QI), dint(QJ) )
         + ( not_fails, covered ).

:- true pred no_attack(I,J,QI,QJ)
         : ( int(I), int(J), dint(QI), dint(QJ) )
        => ( int(I), int(J), dint(QI), dint(QJ), size(lb,I,int(I)), size(lb,J,int(J)), size(lb,QI,size(QI)), size(lb,QJ,size(QJ)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,7), cost(lb,numvars,4) ).

:- true pred no_attack(I,J,QI,QJ)
         : ( int(I), int(J), dint(QI), dint(QJ) )
        => ( int(I), int(J), dint(QI), dint(QJ), size(ub,I,int(I)), size(ub,J,int(J)), size(ub,QI,size(QI)), size(ub,QJ,size(QJ)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,7), cost(ub,numvars,4) ).

no_attack(I,J,QI,QJ) :-
        neq(QI,QJ),
        plus_dint_int_var(QI,I,A),
        plus_dint_int_var(QJ,J,B),
        neq(A,B),
        minus_dint_int_var(QI,I,C),
        minus_dint_int_var(QJ,J,D),
        neq(C,D).


