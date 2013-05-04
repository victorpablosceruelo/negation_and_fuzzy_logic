:-module('Sum1toN',[sum1toN_1/3],[assertions,nativeprops]).

:-entry sum1toN_1(Sum,N,Sum_F):(int(Sum),int(N)).
:-success sum1toN_1(Sum,N,Sum_F)=>(int(Sum),int(N),int(Sum_F)).

sum1toN_1(Sum_0,N_0,Sum_2) :-
        N_0>=1,
        Sum_aux_0 is Sum_0+N_0,
        Sum_1 is Sum_aux_0,
        N_aux_0 is N_0-1,
        N_1 is N_aux_0,
        sum1toN_1(Sum_1,N_1,Sum_2) .
sum1toN_1(Sum_0,N_0,Sum_0) :-
        N_0<1.



% :-entry sum1toN(N,Sum1toN):(int(N),var(Sum1toN)).
% :-success sum1toN(N,Sum1toN)=>(int(N),int(Sum1toN)).

% sum1toN(N_0,Sum1toN_0) :-
        
%         sum1toN_1(0,N_0,Sum1toN_0).

