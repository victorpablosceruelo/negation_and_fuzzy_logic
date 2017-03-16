:-module(example2,[sum1toN/2,gcd/3,recfact/2],[assertions,nativeprops]).

:-entry sum1toN_1(Sum,N,Sum_F,N_F):(int(Sum),int(N)).
:-success sum1toN_1(Sum,N,Sum_F,N_F)=>(int(Sum),int(N),int(Sum_F),int(N_F)).

sum1toN_1(Sum_0,N_0,Sum_2,N_2) :-
        N_0>=1,
        Sum_aux_0 is Sum_0+N_0,
        Sum_1 is Sum_aux_0,
        N_aux_0 is N_0-1,
        N_1 is N_aux_0,
        sum1toN_1(Sum_1,N_1,Sum_2,N_2) .
sum1toN_1(Sum_0,N_0,Sum_0,N_0) :-
        N_0<1,
        true .


:-entry sum1toN(N,Sum1toN):(int(N),var(Sum1toN)).
:-success sum1toN(N,Sum1toN)=>(int(N),int(Sum1toN)).

sum1toN(N_0,Sum1toN_0) :-
        Sum_0 is 0,
        sum1toN_1(Sum_0,N_0,Sum_1,_N_1),
        Sum1toN_0 is Sum_1 .


:-entry gcd_1(M,N,M_F,N_F):(int(M),int(N)).
:-success gcd_1(M,N,M_F,N_F)=>(int(M),int(N),int(M_F),int(N_F)).

gcd_1(M_0,N_0,M_1,N_1) :-
        M_0<N_0,
        T_0 is M_0,
        M_1 is N_0,
        N_1 is T_0 .
gcd_1(M_0,N_0,M_0,N_0) :-
        M_0>=N_0,
        true .


:-entry gcd_2(N,R,Gcd_2):(int(N),int(R)).
:-success gcd_2(N,R,Gcd_2)=>(int(N),int(R),int(Gcd_2)).

gcd_2(N_0,R_0,Gcd_2_0) :-
        R_0=0,
        Gcd_2_0 is N_0 .
gcd_2(N_0,R_0,Gcd_2_0) :-
        R_0\==0,
        gcd(N_0,R_0,Gcd_2_aux_gcd_0),
        Gcd_2_0 is Gcd_2_aux_gcd_0 .


:-entry gcd(M,N,Gcd):(int(M),int(N),var(Gcd)).
:-success gcd(M,N,Gcd)=>(int(M),int(N),int(Gcd)).

gcd(M_0,N_0,Gcd_0) :-
        gcd_1(M_0,N_0,M_1,N_1),
        R_aux_0 is M_1 mod N_1,
        R_0 is R_aux_0,
        gcd_2(N_1,R_0,Gcd_aux_gcd_2_0),
        Gcd_0 is Gcd_aux_gcd_2_0 .


:-trust pred recfact_1(N,Recfact_1): int(N) =>(int(N),int(Recfact_1)).

recfact_1(N_0,Recfact_1_0) :-
        N_0=1,
        Recfact_1_0 is 1 .
recfact_1(N_0,Recfact_1_0) :-
        N_0\==1,
        Recfact_1_aux_r_recfact_1_0 is N_0-1,
        recfact(Recfact_1_aux_r_recfact_1_0,Recfact_1_aux_r_recfact_0),
        Recfact_1_aux_0 is N_0*Recfact_1_aux_r_recfact_0,
        Recfact_1_0 is Recfact_1_aux_0 .

:- entry recfact(A,B):int(A).
:- trust pred recfact(N,Recfact):(int(N),var(Recfact))=>(int(N),int(Recfact)).

recfact(N_0,Recfact_0) :-
        recfact_1(N_0,Recfact_aux_recfact_1_0),
        Recfact_0 is Recfact_aux_recfact_1_0 .

