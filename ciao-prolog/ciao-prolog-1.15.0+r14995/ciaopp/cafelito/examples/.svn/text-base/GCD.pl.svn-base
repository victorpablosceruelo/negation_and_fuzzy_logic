:- module( _gcd, [gcd/3], [assertions , nativeprops] ).


gcd_1(M_0,N_0,M_0,N_0) :-
        M_0>=N_0 .

:- trust calls gcd_2(N,R,Gcd_2)
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

:- trust calls gcd(M,N,Gcd)
         : ( int(M), int(N), var(Gcd) ).

:- trust success gcd(M,N,Gcd)
        => ( int(M), int(N), int(Gcd) ).

gcd(M_0,N_0,Gcd_0) :-
        gcd_1(M_0,N_0,M_1,N_1),
        R_aux_0 is M_1 mod N_1,
        R_0 is R_aux_0,
        gcd_2(N_1,R_0,Gcd_aux_gcd_2_0),
        Gcd_0 is Gcd_aux_gcd_2_0 .





