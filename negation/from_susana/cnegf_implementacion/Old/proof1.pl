:- module(proof1,[impar/1,
                  digito_par1/1,
                  digito_par2/1,
		  digito_par3/1],[]).

:- use_module(.(neg)).
:- use_package(.(finsol)).

impar(X):-
	Y is X rem 2,
	Y\==0.

mi_member(X,[X|_L]).
mi_member(X,[_Y|L]):-
     mi_member(X,L).

digito_par1(X):-
     mi_member(X,[0,1,2,3,4,5,6,7,8,9]),
     neg(impar(X)).  % por cnegf(impar(X))

digito_par2(X):-
     neg(impar(X)),  % por cnegf(impar(X))
     mi_member(X,[0,1,2,3,4,5,6,7,8,9]). 

% :- entry digito_par3(A) : (ground(A)).
digito_par3(X):-
     neg(impar(X)). % igual sin entry y por 
                    % cnegf(impar(X)) con entry


