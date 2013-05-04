:-module(produc,[init/3],[assertions,regtypes]).

plus(X,Y,Z):- ground(X),ground(Y),!,Z is X + Y. 
plus(X,Y,Z):- ground(Y),ground(Z),!,X is Z - Y.
plus(X,Y,Z):- ground(X),ground(Z),!,Y is Z - X.
produc(X,Z,Res):- Y =X,plus(X,Y,Tmp),plus(Z,Tmp,Res).
init(X,Z,Res):-X=1,produc(X,Z,Res).

:- entry init(X,Z,Res):ground(Z).

%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

