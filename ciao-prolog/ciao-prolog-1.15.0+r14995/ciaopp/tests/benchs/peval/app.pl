

:- module( app, [append/3], [assertions] ).

:- entry append([1,2|T1],[4],Cs).

%:- entry append([1,2,3|L],[4],Cs).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).


