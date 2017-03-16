:- module( _example1, [callme/2], [assertions , nativeprops] ).


:- trust calls callme_1(X,A,Juan,X_F,A_F,Juan_F)
         : ( int(X), int(A), int(Juan) ).

:- trust success callme_1(X,A,Juan,X_F,A_F,Juan_F)
        => ( int(X), int(A), int(Juan), int(X_F), int(A_F), int(Juan_F) ).

callme_1(X_0,A_0,Juan_0,X_1,A_0,Juan_0) :-
        Juan_0\==2,
        callme(8,X_aux_r_l_l_r_callme_1_callme_0),
        callme(X_aux_r_l_l_r_callme_1_callme_0,X_aux_r_l_l_r_callme_0),
        X_aux_r_l_l_0 is 5*X_aux_r_l_l_r_callme_0,
        X_aux_r_l_0 is X_aux_r_l_l_0+5,
        X_aux_r_r_0 is 8*8,
        X_aux_r_0 is X_aux_r_l_0*X_aux_r_r_0,
        X_aux_0 is X_0+X_aux_r_0,
        X_1 is X_aux_0 .
callme_1(_X_0,_A_0,Juan_0,X_1,A_1,Juan_0) :-
        Juan_0=2,
        Juan_0\==2,
        A_1 is 6,
        X_1 is A_1 .
callme_1(X_0,A_0,Juan_0,X_0,A_0,Juan_1) :-
        Juan_0=2,
        Juan_0=2,
        Juan_1 is 9 .

:- trust calls callme(A,Callme)
         : ( int(A), var(Callme) ).

:- trust success callme(A,Callme)
        => ( int(A), int(Callme) ).

callme(A_0,Callme_0) :-
        Juan_0 is 4,
        X_0 is 0,
        callme_1(X_0,A_0,Juan_0,X_1,_137445,Juan_1),
        X_aux_0 is X_1+Juan_1,
        X_2 is X_aux_0,
        Callme_0 is X_2 .


