:- module(_,[inc_all/2],[assertions,fsyntax]). 

:- entry inc_all(_,_) : {list, ground} * var.

inc_all( [] )    := [].
inc_all( [H|T] ) := [ H+1 | ~inc_all(T) ]. 

