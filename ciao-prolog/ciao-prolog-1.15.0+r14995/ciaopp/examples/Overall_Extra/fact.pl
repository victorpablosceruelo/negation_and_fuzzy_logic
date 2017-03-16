:- module(_,[ft/2,ft1/2],[assertions,fsyntax,regtypes]).

:- entry ft/2 : num * var.

ft(0) := 1.
ft(N) := N * ~ft(N-1) :- N > 0.

%% ft(0,1).
%% ft(N,R) :- 
%% 	N>0, 
%% 	N1 is N-1, 
%% 	ft(N1,R1), 
%% 	R is R1*N.














%% ft(N) := N * ~ft(--N) :- N > 0.
