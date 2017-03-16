:- module( app, [app/3], [assertions] ).

:- entry app(L,[1,2,3,4],Cs).

app([],X,X).
app([H|X],Y,[H|Z]):- 
	app(X,Y,Z).


