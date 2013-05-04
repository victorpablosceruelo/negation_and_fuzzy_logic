:- module(mmatrix,[mmultiply/3],[assertions]).

:- use_module(multiply, [multiply/3]).

%:- entry mmultiply(X,Y,Z): ( var(Z), list(X,list(num)), list(X,list(num)) ). 
:- entry mmultiply(X,Y,Z): ( var(Z), ground(X), ground(Y)).

mmultiply([],_,[]).
mmultiply([V0|Rest], V1, [Result|Others]):-  
	mmultiply(Rest, V1, Others),
	multiply(V1,V0,Result).
