
:- module(findall, [collateral_percent/3], [assertions]).

:- use_module(library(aggregates), [findall/3]).

:- entry collateral_percent/3.

 amount(mortgage,client1,1200).
 
 collateral_percent(Type,Client,Xs) :-
	findall(X,(collateral(Collateral,Type),
		   amount(Collateral,Client,X)),
		Xs).

 collateral(mortgage,illiquid).

