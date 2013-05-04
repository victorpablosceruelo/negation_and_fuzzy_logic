:-module('Math1',[fib/2,recfact/2,log/2],[regtypes,assertions,nativeprops]).

:-entry fib_1_1(N,Fib_1_1):int(N).
:-success fib_1_1(N,Fib_1_1)=>(int(N),int(Fib_1_1)).

fib_1_1(N_0,Fib_1_1_0) :-
        N_0=1,
        Fib_1_1_0 is 1 .
fib_1_1(N_0,Fib_1_1_0) :-
        N_0\==1,
        Fib_1_1_aux_l_fib_1_0 is N_0-1,
        fib(Fib_1_1_aux_l_fib_1_0,Fib_1_1_aux_l_fib_0),
        Fib_1_1_aux_r_fib_1_0 is N_0-2,
        fib(Fib_1_1_aux_r_fib_1_0,Fib_1_1_aux_r_fib_0),
        Fib_1_1_aux_0 is Fib_1_1_aux_l_fib_0+Fib_1_1_aux_r_fib_0,
        Fib_1_1_0 is Fib_1_1_aux_0 .


:-entry fib_1(N,Fib_1):int(N).
:-success fib_1(N,Fib_1)=>(int(N),int(Fib_1)).

fib_1(N_0,Fib_1_0) :-
        N_0=0,
        Fib_1_0 is 0 .
fib_1(N_0,Fib_1_0) :-
        N_0\==0,
        fib_1_1(N_0,Fib_1_aux_fib_1_1_0),
        Fib_1_0 is Fib_1_aux_fib_1_1_0 .


:-entry fib(N,Fib):(int(N),var(Fib)).
:-success fib(N,Fib)=>(int(N),int(Fib)).

fib(N_0,Fib_0) :-
        fib_1(N_0,Fib_aux_fib_1_0),
        Fib_0 is Fib_aux_fib_1_0 .


% :-entry ack_1_1_1(M,Ack_1_1_1):long(M).
% :-success ack_1_1_1(M,Ack_1_1_1)=>(long(M),long(Ack_1_1_1)).

% ack_1_1_1(M_0,Ack_1_1_1_0) :-
%         M_0=1,
%         Ack_1_1_1_0 is 2 .
% ack_1_1_1(M_0,Ack_1_1_1_0) :-
%         M_0\==1,
%         Ack_1_1_1_aux_0 is M_0+2,
%         Ack_1_1_1_0 is Ack_1_1_1_aux_0 .


% :-entry ack_1_1(M,N,Ack_1_1):(long(M),int(N)).
% :-success ack_1_1(M,N,Ack_1_1)=>(long(M),int(N),long(Ack_1_1)).

% ack_1_1(M_0,N_0,Ack_1_1_0) :-
%         N_0=0,
%         ack_1_1_1(M_0,Ack_1_1_aux_ack_1_1_1_0),
%         Ack_1_1_0 is Ack_1_1_aux_ack_1_1_1_0 .
% ack_1_1(M_0,N_0,Ack_1_1_0) :-
%         N_0\==0,
%         Ack_1_1_aux_ack_1_ack_1_0 is M_0-1,
%         ack(Ack_1_1_aux_ack_1_ack_1_0,N_0,Ack_1_1_aux_ack_1_ack_0),
%         Ack_1_1_aux_ack_2_0 is N_0-1,
%         ack(Ack_1_1_aux_ack_1_ack_0,Ack_1_1_aux_ack_2_0,Ack_1_1_aux_ack_0),
%         Ack_1_1_0 is Ack_1_1_aux_ack_0 .


% :-entry ack_1(M,N,Ack_1):(long(M),int(N)).
% :-success ack_1(M,N,Ack_1)=>(long(M),int(N),long(Ack_1)).

% ack_1(M_0,N_0,Ack_1_0) :-
%         M_0=0,
%         Ack_1_0 is 1 .
% ack_1(M_0,N_0,Ack_1_0) :-
%         M_0\==0,
%         ack_1_1(M_0,N_0,Ack_1_aux_ack_1_1_0),
%         Ack_1_0 is Ack_1_aux_ack_1_1_0 .


% :-entry ack(M,N,Ack):(long(M),int(N),var(Ack)).
% :-success ack(M,N,Ack)=>(long(M),int(N),long(Ack)).

% ack(M_0,N_0,Ack_0) :-
%         ack_1(M_0,N_0,Ack_aux_ack_1_0),
%         Ack_0 is Ack_aux_ack_1_0 .


:-entry recfact_1(N,Recfact_1):int(N).
:-success recfact_1(N,Recfact_1)=>(int(N),int(Recfact_1)).

recfact_1(N_0,Recfact_1_0) :-
        N_0=1,
        Recfact_1_0 is 1 .
recfact_1(N_0,Recfact_1_0) :-
        N_0\==1,
        Recfact_1_aux_r_recfact_1_0 is N_0-1,
        recfact(Recfact_1_aux_r_recfact_1_0,Recfact_1_aux_r_recfact_0),
        Recfact_1_aux_0 is N_0*Recfact_1_aux_r_recfact_0,
        Recfact_1_0 is Recfact_1_aux_0 .


:-entry recfact(N,Recfact):(int(N),var(Recfact)).
:-success recfact(N,Recfact)=>(int(N),int(Recfact)).

recfact(N_0,Recfact_0) :-
        recfact_1(N_0,Recfact_aux_recfact_1_0),
        Recfact_0 is Recfact_aux_recfact_1_0 .


% :-entry pow1_1(X,N,Pow1_1):(double(X),int(N)).
% :-success pow1_1(X,N,Pow1_1)=>(double(X),int(N),double(Pow1_1)).

% pow1_1(X_0,N_0,Pow1_1_0) :-
%         N_0=0,
%         Pow1_1_0 is 1.0 .
% pow1_1(X_0,N_0,Pow1_1_0) :-
%         N_0\==0,
%         Pow1_1_aux_r_pow1_2_0 is N_0-1,
%         pow1(X_0,Pow1_1_aux_r_pow1_2_0,Pow1_1_aux_r_pow1_0),
%         Pow1_1_aux_0 is X_0*Pow1_1_aux_r_pow1_0,
%         Pow1_1_0 is Pow1_1_aux_0 .


% :-entry pow1(X,N,Pow1):(double(X),int(N),var(Pow1)).
% :-success pow1(X,N,Pow1)=>(double(X),int(N),double(Pow1)).

% pow1(X_0,N_0,Pow1_0) :-
%         pow1_1(X_0,N_0,Pow1_aux_pow1_1_0),
%         Pow1_0 is Pow1_aux_pow1_1_0 .


% :-entry pow_1(N,Pow_1):int(N).
% :-success pow_1(N,Pow_1)=>(int(N),double(Pow_1)).

% pow_1(N_0,Pow_1_0) :-
%         N_0=0,
%         Pow_1_0 is 1.0 .
% pow_1(N_0,Pow_1_0) :-
%         N_0\==0,
%         true .


% :-entry pow_2(N,X,P,Pow_2):(int(N),double(X),double(P)).
% :-success pow_2(N,X,P,Pow_2)=>(int(N),double(X),double(P),double(Pow_2)).

% pow_2(N_0,X_0,P_0,Pow_2_0) :-
%         Cond_l_0 is N_0 mod 2,
%         Cond_l_0=0,
%         Pow_2_aux_0 is P_0*P_0,
%         Pow_2_0 is Pow_2_aux_0 .
% pow_2(N_0,X_0,P_0,Pow_2_0) :-
%         Cond_l_0 is N_0 mod 2,
%         Cond_l_0\==0,
%         Pow_2_aux_l_0 is X_0*P_0,
%         Pow_2_aux_0 is Pow_2_aux_l_0*P_0,
%         Pow_2_0 is Pow_2_aux_0 .


% :-entry pow(X,N,Pow):(double(X),int(N),var(Pow)).
% :-success pow(X,N,Pow)=>(double(X),int(N),double(Pow)).

% pow(X_0,N_0,Pow_0) :-
%         pow_1(N_0,Pow_aux_pow_1_0),
%         Pow_0 is Pow_aux_pow_1_0,
%         P_aux_pow_2_0 is N_0//2,
%         pow(X_0,P_aux_pow_2_0,P_aux_pow_0),
%         P_0 is P_aux_pow_0,
%         pow_2(N_0,X_0,P_0,Pow_aux_pow_2_0),
%         Pow_0 is Pow_aux_pow_2_0 .


:-entry log_1(N,Log_1):int(N).
:-success log_1(N,Log_1)=>(int(N),int(Log_1)).

log_1(N_0,Log_1_0) :-
        N_0=1,
        Log_1_0 is 0 .
log_1(N_0,Log_1_0) :-
        N_0\==1,
        Log_1_aux_r_log_1_0 is N_0//2,
        log(Log_1_aux_r_log_1_0,Log_1_aux_r_log_0),
        Log_1_aux_0 is 1+Log_1_aux_r_log_0,
        Log_1_0 is Log_1_aux_0 .


:-entry log(N,Log):(int(N),var(Log)).
:-success log(N,Log)=>(int(N),int(Log)).

log(N_0,Log_0) :-
        log_1(N_0,Log_aux_log_1_0),
        Log_0 is Log_aux_log_1_0 .



