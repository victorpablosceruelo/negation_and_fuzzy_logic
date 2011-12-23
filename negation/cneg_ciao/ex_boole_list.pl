:- module(ex_boole_list,_,[.(cneg), .(debugger_pkg)]).

boole(0).
boole(1).

binary_list([]).
binary_list([Y|L]):-
 	boole(Y),  
 	binary_list(L).

no_binary_list(X) :- cneg_rt([], binary_list(X)).