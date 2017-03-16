:- module(mm,_,[assertions]).
:- use_package(debug).

:- spy j/1.

j([]).
j([X|Xs]) :- 
         display(X),
	 j(Xs),
         nl.
