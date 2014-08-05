:- module(ex_modifier,_,[rfuzzy, clpr, pkgs_output_debug]).
% Compilation time debug can be activated  by adding to the packages list [rfuzzy, clpr] the package pkgs_output_debug.
% Running time debug can be activated removing the comment marker % at the beginning of the following line.
% :- activate_rfuzzy_debug.

% CRISP FUNCTIONS
equal(X,X).
greater(X,Y):- X.>.Y.

% QUANTIFIERS
define_modifier(a_little/2, TV_In, TV_Out) :-
	TV_Out * TV_Out .=. TV_In.

define_modifier(too_much/2, TV_In, TV_Out) :-
	TV_Out .=. TV_In * TV_In * TV_In.
