:- module(kk,_,[assertions]).

:- use_module(mm).

% The embedded debugger skips the first call to j/1
% (Probably because debugging is done by program transformation and the 
% does not know it has to deal with j/1 in this module).

main(Xs) :- j(Xs).
