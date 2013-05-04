:- module(_1,[main/4],['.'(minizinc),assertions,regtypes,nativeprops,isomodes,basicmodes]).

:- entry main(_1,_2,_3,_4)
         : ( int(_1), array(_2), array(_3), var(_4) ).

:- true pred main(N,Profits,X,Sum)
         : ( int(N), array(Profits), array(X), term(Sum) )
        => ( rt0(N), array(Profits), array(X), rt0(Sum) ).

:- true pred main(N,Profits,X,Sum)
         : ( mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum]]), var(Sum), ground([N]) )
        => ( mshare([[Profits],[Profits,X],[X]]), ground([N,Sum]) ).

:- true pred main(N,Profits,X,Sum)
         : ( int(N), array(Profits), array(X), var(Sum) )
        => ( rt0(N), array(Profits), array(X), rt0(Sum), size(lb,N,int(N)), size(lb,Profits,size(Profits)), size(lb,X,size(X)), size(lb,Sum,int(N)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred main(N,Profits,X,Sum)
         : ( int(N), array(Profits), array(X), var(Sum) )
        => ( rt0(N), array(Profits), array(X), rt0(Sum), size(ub,N,int(N)), size(ub,Profits,size(Profits)), size(ub,X,size(X)), size(ub,Sum,bot) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,0) ).

main(N,Profits,X,Sum) :-
        true(
               (
                int(N),
                array(Profits),
                array(X),
                term(Sum)
               )
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum]]),
                var(Sum),
                ground([N])
               )
            ),
        sum_i(N,Profits,X,Sum).

:- true pred sum_i(J,_1,_2,Sum)
         : ( int(J), array(_1), array(_2), term(Sum) )
        => ( rt0(J), array(_1), array(_2), rt0(Sum) ).

:- true pred sum_i(J,_1,_2,Sum)
         : ( mshare([[_1],[_1,_2],[_1,_2,Sum],[_1,Sum],[_2],[_2,Sum],[Sum]]), var(Sum), ground([J]) )
        => ( mshare([[_1],[_1,_2],[_2]]), ground([J,Sum]) ).

:- true pred sum_i(J,_1,_2,Sum)
         : ( int(J), array(_1), array(_2), var(Sum) )
        => ( rt0(J), array(_1), array(_2), rt0(Sum), size(lb,J,int(J)), size(lb,_1,size(_1)), size(lb,_2,size(_2)), size(lb,Sum,int(J)) )
         + ( cost(lb,alldiff,0), cost(lb,constraints,0), cost(lb,numvars,0) ).

:- true pred sum_i(J,_1,_2,Sum)
         : ( int(J), array(_1), array(_2), var(Sum) )
        => ( rt0(J), array(_1), array(_2), rt0(Sum), size(ub,J,int(J)), size(ub,_1,size(_1)), size(ub,_2,size(_2)), size(ub,Sum,bot) )
         + ( cost(ub,alldiff,0), cost(ub,constraints,0), cost(ub,numvars,0) ).

sum_i(0,_1,_2,0).
sum_i(J,Profits,X,Sum) :-
        true(
               (
                int(J),
                array(Profits),
                array(X),
                term(Sum),
                term(Pj),
                term(Xj),
                term(_P),
                term(NJ),
                term(NSum)
               )
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum],[Pj],[Xj],[_P],[NJ],[NSum]]),
                var(Sum),
                var(Pj),
                var(Xj),
                var(_P),
                var(NJ),
                var(NSum),
                ground([J])
               )
            ),
        J>0,
        true(
               (
                int(J),
                array(Profits),
                array(X),
                term(Sum),
                term(Pj),
                term(Xj),
                term(_P),
                term(NJ),
                term(NSum)
               )
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum],[Pj],[Xj],[_P],[NJ],[NSum]]),
                var(Sum),
                var(Pj),
                var(Xj),
                var(_P),
                var(NJ),
                var(NSum),
                ground([J])
               )
            ),
        element_int(J,Profits,Pj),
        true(
               (
                int(J),
                array(Profits),
                array(X),
                term(Sum),
                int(Pj),
                term(Xj),
                term(_P),
                term(NJ),
                term(NSum)
               )
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum],[Xj],[_P],[NJ],[NSum]]),
                var(Xj),
                var(_P),
                var(NJ),
                var(NSum),
                ground([J,Pj])
               )
            ),
        element_int(J,X,Xj),
        true(
               (
                int(J),
                array(Profits),
                array(X),
                term(Sum),
                int(Pj),
                int(Xj),
                term(_P),
                term(NJ),
                term(NSum)
               )
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum],[_P],[NJ],[NSum]]),
                var(_P),
                var(NJ),
                var(NSum),
                ground([J,Pj,Xj])
               )
            ),
        leq(Xj,Pj),
        true(
               (
                int(J),
                array(Profits),
                array(X),
                term(Sum),
                int(Pj),
                int(Xj),
                term(_P),
                term(NJ),
                term(NSum)
               )
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum],[_P],[NJ],[NSum]]),
                var(_P),
                var(NJ),
                var(NSum),
                ground([J,Pj,Xj])
               )
            ),
        mult_int_dint_var(Pj,Xj,_P),
        true(
               fails
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum],[NJ],[NSum]]),
                var(NJ),
                var(NSum),
                ground([J,Pj,Xj,_P])
               )
            ),
        minizinc_rt:(NJ is J-1),
        true(
               fails
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum],[NSum]]),
                var(NSum),
                ground([J,Pj,Xj,_P,NJ])
               )
            ),
        sum_i(NJ,Profits,X,NSum),
        true(
               fails
            ),
        true(
               (
                mshare([[Profits],[Profits,X],[Profits,X,Sum],[Profits,Sum],[X],[X,Sum],[Sum]]),
                ground([J,Pj,Xj,_P,NJ,NSum])
               )
            ),
        minizinc_rt:(Sum is NSum+1+_P).


:- regtype rt0/1.

rt0(0).


