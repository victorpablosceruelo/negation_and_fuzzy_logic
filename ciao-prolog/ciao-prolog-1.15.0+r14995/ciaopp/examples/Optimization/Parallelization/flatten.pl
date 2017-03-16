:- module(flatten,[flatten/2],[assertions,nativeprops]).

:- entry flatten/2 : ground * var.

flatten(Xs,Ys) :- flatten_(Xs,Ys,[]).

flatten_([], Xs, Xs).
flatten_([X|Xs],Ys,Zs) :-
        flatten_(X,Ys,Ys1),
        flatten_(Xs,Ys1,Zs).
flatten_(X, [X|Xs], Xs) :-
        atomic(X), X \== [].
























% :- entry flatten(X,Y) : (mshare([[X],[Y]]),var([Y]),linear([X])).


