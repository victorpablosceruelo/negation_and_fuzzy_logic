:- module( _Math2, [log/2 , recfact/2 , fib/2 , gcd/3 , sum1toN/2], [assertions , nativeprops] ).


:- entry sum1toN_1(Sum,N,Sum_F,N_F)
         : ( int(Sum), int(N) ).

:- trust success sum1toN_1(Sum,N,Sum_F,N_F)
        => ( int(Sum), int(N), int(Sum_F), int(N_F) ).

sum1toN_1(Sum_0,N_0,Sum_2,N_2) :-
        N_0>=1,
        Sum_aux_0 is Sum_0+N_0,
        Sum_1 is Sum_aux_0,
        N_aux_0 is N_0-1,
        N_1 is N_aux_0,
        sum1toN_1(Sum_1,N_1,Sum_2,N_2) .
sum1toN_1(Sum_0,N_0,Sum_0,N_0) :-
        N_0<1 .

:- entry sum1toN(N,Sum1toN)
         : ( int(N), var(Sum1toN) ).

:- trust success sum1toN(N,Sum1toN)
        => ( int(N), int(Sum1toN) ).

sum1toN(N_0,Sum1toN_0) :-
        Sum_0 is 0,
        sum1toN_1(Sum_0,N_0,Sum_1,_163999),
        Sum1toN_0 is Sum_1 .

:- entry gcd_1(M,N,M_F,N_F)
         : ( int(M), int(N) ).

:- trust success gcd_1(M,N,M_F,N_F)
        => ( int(M), int(N), int(M_F), int(N_F) ).

gcd_1(M_0,N_0,M_1,N_1) :-
        M_0<N_0,
        T_0 is M_0,
        M_1 is N_0,
        N_1 is T_0 .
gcd_1(M_0,N_0,M_0,N_0) :-
        M_0>=N_0 .

:- entry gcd_2(N,R,Gcd_2)
         : ( int(N), int(R), var(Gcd_2) ).

:- trust success gcd_2(N,R,Gcd_2)
        => ( int(N), int(R), int(Gcd_2) ).

gcd_2(N_0,R_0,Gcd_2_0) :-
        R_0=0,
        Gcd_2_0 is N_0 .
gcd_2(N_0,R_0,Gcd_2_0) :-
        R_0\==0,
        gcd(N_0,R_0,Gcd_2_aux_gcd_0),
        Gcd_2_0 is Gcd_2_aux_gcd_0 .

:- entry gcd(M,N,Gcd)
         : ( int(M), int(N), var(Gcd) ).

:- trust success gcd(M,N,Gcd)
        => ( int(M), int(N), int(Gcd) ).

gcd(M_0,N_0,Gcd_0) :-
        gcd_1(M_0,N_0,M_1,N_1),
        R_aux_0 is M_1 mod N_1,
        R_0 is R_aux_0,
        gcd_2(N_1,R_0,Gcd_aux_gcd_2_0),
        Gcd_0 is Gcd_aux_gcd_2_0 .

:- entry fib_1(N,Fib_1)
         : ( int(N), var(Fib_1) ).

:- trust success fib_1(N,Fib_1)
        => ( int(N), int(Fib_1) ).

fib_1(N_0,Fib_1_0) :-
        N_0=0,
        Fib_1_0 is 0 .
fib_1(N_0,Fib_1_0) :-
        N_0\==0,
        N_0=1,
        Fib_1_0 is 1 .
fib_1(N_0,Fib_1_0) :-
        N_0\==0,
        N_0\==1,
        Fib_1_aux_l_fib_1_0 is N_0-1,
        fib(Fib_1_aux_l_fib_1_0,Fib_1_aux_l_fib_0),
        Fib_1_aux_r_fib_1_0 is N_0-2,
        fib(Fib_1_aux_r_fib_1_0,Fib_1_aux_r_fib_0),
        Fib_1_aux_0 is Fib_1_aux_l_fib_0+Fib_1_aux_r_fib_0,
        Fib_1_0 is Fib_1_aux_0 .

:- entry fib(N,Fib)
         : ( int(N), var(Fib) ).

:- trust success fib(N,Fib)
        => ( int(N), int(Fib) ).

fib(N_0,Fib_0) :-
        fib_1(N_0,Fib_aux_fib_1_0),
        Fib_0 is Fib_aux_fib_1_0 .

:- entry recfact_1(N,Recfact_1)
         : ( int(N), var(Recfact_1) ).

:- trust success recfact_1(N,Recfact_1)
        => ( int(N), int(Recfact_1) ).

recfact_1(N_0,Recfact_1_0) :-
        N_0=1,
        Recfact_1_0 is 1 .
recfact_1(N_0,Recfact_1_0) :-
        N_0\==1,
        Recfact_1_aux_r_recfact_1_0 is N_0-1,
        recfact(Recfact_1_aux_r_recfact_1_0,Recfact_1_aux_r_recfact_0),
        Recfact_1_aux_0 is N_0*Recfact_1_aux_r_recfact_0,
        Recfact_1_0 is Recfact_1_aux_0 .

:- entry recfact(N,Recfact)
         : ( int(N), var(Recfact) ).

:- trust success recfact(N,Recfact)
        => ( int(N), int(Recfact) ).

recfact(N_0,Recfact_0) :-
        recfact_1(N_0,Recfact_aux_recfact_1_0),
        Recfact_0 is Recfact_aux_recfact_1_0 .

:- entry log_1(N,Log_1)
         : ( int(N), var(Log_1) ).

:- trust success log_1(N,Log_1)
        => ( int(N), int(Log_1) ).

log_1(N_0,Log_1_0) :-
        N_0=1,
        Log_1_0 is 0 .
log_1(N_0,Log_1_0) :-
        N_0\==1,
        Log_1_aux_r_log_1_0 is N_0//2,
        log(Log_1_aux_r_log_1_0,Log_1_aux_r_log_0),
        Log_1_aux_0 is 1+Log_1_aux_r_log_0,
        Log_1_0 is Log_1_aux_0 .

:- entry log(N,Log)
         : ( int(N), var(Log) ).

:- trust success log(N,Log)
        => ( int(N), int(Log) ).

log(N_0,Log_0) :-
        log_1(N_0,Log_aux_log_1_0),
        Log_0 is Log_aux_log_1_0 .






