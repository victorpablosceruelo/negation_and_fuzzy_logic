
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               EXAMPLES  FOR PACKAGE CNEG                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(pruebas_cneg,[impar/1,no_es_impar/1],[cneg]).

impar(s(0)).
impar(s(s(X))):-
        impar(X).

no_es_impar(X):-
	cneg(impar(X)).
