:- module( _pow, [pow/3], [assertions , nativeprops] ).


:- trust calls pow_1(N,Pow_1)
         : ( int(N), var(Pow_1) ).

:- trust success pow_1(N,Pow_1)
        => ( int(N), num(Pow_1) ).

pow_1(N_0,Pow_1_0) :-
        N_0=0,
        Pow_1_0 is 1.0 .
pow_1(N_0,_Pow_1_0) :-
        N_0\==0 .

:- trust calls pow_2(N,X,P,Pow_2)
         : ( int(N), num(X), num(P), var(Pow_2) ).

:- trust success pow_2(N,X,P,Pow_2)
        => ( int(N), num(X), num(P), num(Pow_2) ).

pow_2(N_0,_X_0,P_0,Pow_2_0) :-
        Cond_l_0 is N_0 mod 2,
        Cond_l_0=0,
        Pow_2_aux_0 is P_0*P_0,
        Pow_2_0 is Pow_2_aux_0 .
pow_2(N_0,X_0,P_0,Pow_2_0) :-
        Cond_l_0 is N_0 mod 2,
        Cond_l_0\==0,
        Pow_2_aux_l_0 is X_0*P_0,
        Pow_2_aux_0 is Pow_2_aux_l_0*P_0,
        Pow_2_0 is Pow_2_aux_0 .

:- trust calls pow(X,N,Pow)
         : ( num(X), int(N), var(Pow) ).

:- trust success pow(X,N,Pow)
        => ( num(X), int(N), num(Pow) ).

pow(X_0,N_0,Pow_0) :-
        pow_1(N_0,Pow_aux_pow_1_0),
        Pow_0 is Pow_aux_pow_1_0,
        P_aux_pow_2_0 is N_0//2,
        pow(X_0,P_aux_pow_2_0,P_aux_pow_0),
        P_0 is P_aux_pow_0,
        pow_2(N_0,X_0,P_0,Pow_aux_pow_2_0),
        Pow_0 is Pow_aux_pow_2_0 .


