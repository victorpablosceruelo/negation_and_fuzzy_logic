:-module(dapp,[dapp/4],[assertions,regtypes]).
dapp(X,Y,Z,Res):-app(X,Y,Tmp),app(Tmp,Z,Res).
app([],Y,Y).
app([X|Xs],Y,[X|Res]):-app(Xs,Y,Res).
