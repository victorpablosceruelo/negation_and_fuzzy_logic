:- module(ex_connective,_,[rfuzzy, clpr, pkgs_output_debug]).
% Compilation time debug can be activated  by adding to the packages list [rfuzzy, clpr] the package pkgs_output_debug.
% Running time debug can be activated removing the comment marker % at the beginning of the following line.
% :- activate_rfuzzy_debug.

% CRISP FUNCTIONS
equal(X,X).
greater(X,Y):- X.>.Y.

% AGGREGATORS
define_connective(max_with_min_a_half/3, TV_In_1, TV_In_2, TV_Out) :-
	max(TV_In_1, TV_In_2, TV_Aux), min(TV_Aux, 0.5, TV_Out).

define_connective(special_prod/3, TV_In_1, TV_In_2, TV_Out) :-
	TV_Out .=. TV_In_1 * TV_In_2.
