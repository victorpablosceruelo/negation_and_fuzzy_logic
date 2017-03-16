:- module( _app, [append/3], [assertions] ).

%:- entry append(A,3,Cs). % : (var(Cs))+ rename(app_ab(As,Bs,Cs)).

:- entry append([a,b],Bs,Cs) : ground(Bs).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).


