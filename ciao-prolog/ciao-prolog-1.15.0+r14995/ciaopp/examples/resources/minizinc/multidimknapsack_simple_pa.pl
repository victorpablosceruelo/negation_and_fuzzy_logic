:- module(_1,[main/7],[ciaopp(examples(resources(minizinc))),assertions,nativeprops,basicmodes,regtypes,isomodes]).

:- entry main(_1,_2,_3,_4,_5,_6,_7)
         : ( int(_1), int(_2), array(_3), array(_4), array(_5), var(_6), var(_7) ).

:- true pred main(N,M,Profits,Capacities,Weights,X,Sum)
         : ( int(N), int(M), array(Profits), array(Capacities), array(Weights), term(X), term(Sum) )
        => ( int(N), int(M), array(Profits), array(Capacities), array(Weights), array(X), int(Sum) ).

:- true pred main(N,M,Profits,Capacities,Weights,X,Sum)
         : ( mshare([[X],[X,Sum],[Sum]]), var(X), var(Sum), ground([N,M,Profits,Capacities,Weights]) )
        => ground([N,M,Profits,Capacities,Weights,X,Sum]).

:- true pred main(N,M,Profits,Capacities,Weights,X,Sum)
         : ( int(N), int(M), array(Profits), array(Capacities), array(Weights), var(X), var(Sum) )
        => ( int(N), int(M), array(Profits), array(Capacities), array(Weights), array(X), int(Sum) )
         + ( not_fails, covered ).

:- true pred main(N,M,Profits,Capacities,Weights,X,Sum)
         : ( int(N), int(M), array(Profits), array(Capacities), array(Weights), var(X), var(Sum) )
        => ( int(N), int(M), array(Profits), array(Capacities), array(Weights), array(X), int(Sum), size(lb,N,int(N)), size(lb,M,int(M)), size(lb,Profits,size(Profits)), size(lb,Capacities,size(Capacities)), size(lb,Weights,size(Weights)), size(lb,X,0), size(lb,Sum,0) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,int(M)*int(N)), cost(lb,numvars,int(M)*int(N)+int(N)) ).

:- true pred main(N,M,Profits,Capacities,Weights,X,Sum)
         : ( int(N), int(M), array(Profits), array(Capacities), array(Weights), var(X), var(Sum) )
        => ( int(N), int(M), array(Profits), array(Capacities), array(Weights), array(X), int(Sum), size(ub,N,int(N)), size(ub,M,int(M)), size(ub,Profits,size(Profits)), size(ub,Capacities,size(Capacities)), size(ub,Weights,size(Weights)), size(ub,X,inf), size(ub,Sum,inf) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,int(M)*int(N)), cost(ub,numvars,int(M)*int(N)+int(N)) ).

main(N,M,Profits,Capacities,Weights,X,Sum) :-
        minizinc_rt:(Size is N-1+1),
        build_array(X,Size),
        init_i(Size,0,1,X),
        minizinc_rt:(Ind_i is M-1+1),
        forall_i(Ind_i,M,N,M,Profits,Capacities,Weights,X),
        sum_i(N,N,Profits,X,Sum).

:- true pred init_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), rt1(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), rt1(_DU), array(_A) ).

:- true pred init_i(Ind,_DL,_DU,_A)
         : ground([Ind,_DL,_DU,_A])
        => ground([Ind,_DL,_DU,_A]).

:- true pred init_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), rt1(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), rt1(_DU), array(_A) )
         + ( not_fails, covered ).

:- true pred init_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), rt1(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), rt1(_DU), array(_A), size(lb,Ind,int(Ind)), size(lb,_DL,int(_DL)), size(lb,_DU,int(_DU)), size(lb,_A,size(_A)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,int(Ind)) ).

:- true pred init_i(Ind,_DL,_DU,_A)
         : ( int(Ind), rt0(_DL), rt1(_DU), array(_A) )
        => ( int(Ind), rt0(_DL), rt1(_DU), array(_A), size(ub,Ind,int(Ind)), size(ub,_DL,int(_DL)), size(ub,_DU,int(_DU)), size(ub,_A,size(_A)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,int(Ind)) ).

init_i(0,_DL,_DU,_A) :- !.
init_i(Ind,DL,DU,A) :-
        element_dint(Ind,A,Ai),
        in(Ai,DL,DU),
        minizinc_rt:(NInd is Ind-1),
        init_i(NInd,DL,DU,A).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6,_7)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), array(_7) )
        => ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), array(_7) ).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6,_7)
         : ground([Ind_i,_1,_2,_3,_4,_5,_6,_7])
        => ground([Ind_i,_1,_2,_3,_4,_5,_6,_7]).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6,_7)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), array(_7) )
        => ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), array(_7) )
         + ( not_fails, covered ).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6,_7)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), array(_7) )
        => ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), array(_7), size(lb,Ind_i,int(Ind_i)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,size(_4)), size(lb,_5,size(_5)), size(lb,_6,size(_6)), size(lb,_7,size(_7)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,int(_2)*int(Ind_i)), cost(lb,numvars,int(_2)*int(Ind_i)) ).

:- true pred forall_i(Ind_i,_1,_2,_3,_4,_5,_6,_7)
         : ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), array(_7) )
        => ( int(Ind_i), int(_1), int(_2), int(_3), array(_4), array(_5), array(_6), array(_7), size(ub,Ind_i,int(Ind_i)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,size(_4)), size(ub,_5,size(_5)), size(ub,_6,size(_6)), size(ub,_7,size(_7)) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,int(_2)*int(Ind_i)), cost(ub,numvars,int(_2)*int(Ind_i)) ).

forall_i(0,_1,_2,_3,_4,_5,_6,_7) :- !.
forall_i(Ind_i,Ui,N,M,Profits,Capacities,Weights,X) :-
        minizinc_rt:(Index_i is Ui-Ind_i+1),
        minizinc_rt:(Ind_j is N-1+1),
        sum_j(Ind_j,N,Index_i,Weights,X,Sum),
        minizinc_rt:(I is Index_i-1+1),
        element_int(I,Capacities,Ci),
        leq_dint_int(Sum,Ci),
        minizinc_rt:(NInd_i is Ind_i-1),
        forall_i(NInd_i,Ui,N,M,Profits,Capacities,Weights,X).

:- true pred sum_j(Ind,_1,_2,_3,_4,Sum)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), term(Sum) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(Sum) ).

:- true pred sum_j(Ind,_1,_2,_3,_4,Sum)
         : ( mshare([[Sum]]), var(Sum), ground([Ind,_1,_2,_3,_4]) )
        => ground([Ind,_1,_2,_3,_4,Sum]).

:- true pred sum_j(Ind,_1,_2,_3,_4,Sum)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), var(Sum) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(Sum) )
         + ( not_fails, covered ).

:- true pred sum_j(Ind,_1,_2,_3,_4,Sum)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), var(Sum) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(Sum), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,size(_3)), size(lb,_4,size(_4)), size(lb,Sum,0) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,int(Ind)), cost(lb,numvars,int(Ind)) ).

:- true pred sum_j(Ind,_1,_2,_3,_4,Sum)
         : ( int(Ind), int(_1), int(_2), array(_3), array(_4), var(Sum) )
        => ( int(Ind), int(_1), int(_2), array(_3), array(_4), dint(Sum), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,size(_3)), size(ub,_4,size(_4)), size(ub,Sum,inf) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,int(Ind)), cost(ub,numvars,int(Ind)) ).

sum_j(0,_1,_2,_3,_4,0..0) :- !.
sum_j(Ind,U,Index_i,Weights,X,Sum) :-
        minizinc_rt:(Index_j is U-Ind+1),
        minizinc_rt:(J is Index_j-1+1),
        minizinc_rt:(I is Index_i-1+1),
        element2d_int(J,I,Weights,Wji),
        element_int(J,X,Xj),
        mult_int(Wji,Xj,P),
        minizinc_rt:(NInd is Ind-1),
        sum_j(NInd,U,Index_i,Weights,X,NSum),
        plus_int_dint_var(P,NSum,Sum).

:- true pred sum_i(Ind,_1,_2,_3,Sum)
         : ( int(Ind), int(_1), array(_2), array(_3), term(Sum) )
        => ( int(Ind), int(_1), array(_2), array(_3), int(Sum) ).

:- true pred sum_i(Ind,_1,_2,_3,Sum)
         : ( mshare([[Sum]]), var(Sum), ground([Ind,_1,_2,_3]) )
        => ground([Ind,_1,_2,_3,Sum]).

:- true pred sum_i(Ind,_1,_2,_3,Sum)
         : ( int(Ind), int(_1), array(_2), array(_3), var(Sum) )
        => ( int(Ind), int(_1), array(_2), array(_3), int(Sum) )
         + ( not_fails, covered ).

:- true pred sum_i(Ind,_1,_2,_3,Sum)
         : ( int(Ind), int(_1), array(_2), array(_3), var(Sum) )
        => ( int(Ind), int(_1), array(_2), array(_3), int(Sum), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,size(_2)), size(lb,_3,size(_3)), size(lb,Sum,0) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred sum_i(Ind,_1,_2,_3,Sum)
         : ( int(Ind), int(_1), array(_2), array(_3), var(Sum) )
        => ( int(Ind), int(_1), array(_2), array(_3), int(Sum), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,size(_2)), size(ub,_3,size(_3)), size(ub,Sum,inf) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,0) ).

sum_i(0,_1,_2,_3,0) :- !.
sum_i(Ind,U,Profits,X,Sum) :-
        minizinc_rt:(Index is U-Ind+1),
        minizinc_rt:(J is Index-1+1),
        element_int(J,Profits,Pj),
        element_int(J,X,Xj),
        mult_int(Pj,Xj,P),
        minizinc_rt:(NInd is Ind-1),
        sum_i(NInd,U,Profits,X,NSum),
        plus_int(P,NSum,Sum).


:- regtype rt0/1.

rt0(0).


:- regtype rt1/1.

rt1(1).


