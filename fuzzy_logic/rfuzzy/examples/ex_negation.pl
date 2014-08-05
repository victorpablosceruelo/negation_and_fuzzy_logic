:- module(ex_negation,_,[rfuzzy, clpr, pkgs_output_debug]).
% Compilation time debug can be activated  by adding to the packages list [rfuzzy, clpr] the package pkgs_output_debug.
% Running time debug can be activated removing the comment marker % at the beginning of the following line.
% :- activate_rfuzzy_debug.

% CRISP FUNCTIONS
equal(X,X).
greater(X,Y):- X.>.Y.

% QUANTIFIERS
define_negation_op(godel_neg/2, TV_In, TV_Out) :-
	((TV_In .=. 0, TV_Out .=. 1) ; (\+(TV_In .=. 0), TV_Out .=. 0)).
