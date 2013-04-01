%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               EXAMPLES  FOR PACKAGE CNEGF                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%:- module(proof5,_,[assertions, basicmodes, regtypes]).
:- module(_proof2,_,[assertions]).

:- entry no_falla(A) : (ground(A)).
:- entry no_sabemos(A,L) : (ground(A)).

% falla(_X):- fail.
no_falla(_X).
no_sabemos(X,L):- mi_member(X,L).

mi_member(X,[X|_L]):- !.
mi_member(X,[_Y|L]):- 
 	mi_member(X,L).
