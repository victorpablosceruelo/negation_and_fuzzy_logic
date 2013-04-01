:- module(merge,_,[assertions]).
:- use_package(clpq).
%:- use_module(neg,[neg/1]).
%:- use_module(cnegf,[cnegf/1]).

:- entry merge(A,B,C) : (list(A,num), list(B,num), list(C,num)).
:- entry not_merge(A,B,C) : (list(A,num), list(B,num), list(c,num)).

unif(X,X).

merge([X|Xs],[Y|Ys],[X|Zs]) :- X < Y, !, merge(Xs,[Y|Ys],Zs).
merge([X|Xs],[Y|Ys],[X,Y|Zs]) :- unif(X,Y), !, merge(Xs,Ys,Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]) :- X > Y, !, merge([X|Xs],Ys,Zs).
merge(Xs,[],Xs) :- !.
merge([],Ys,Ys) :- !.

%not_merge(X,Y,Z) :-  cnegf(merge(X,Y,Z)).

merge1([X|Xs],[Y|Ys],[X|Zs]) :- X .<. Y,  merge1(Xs,[Y|Ys],Zs).
merge1([X|Xs],[Y|Ys],[X,Y|Zs]) :- X .=. Y,  merge1(Xs,Ys,Zs).
merge1([X|Xs],[Y|Ys],[Y|Zs]) :- X .>. Y,  merge1([X|Xs],Ys,Zs).
merge1(Xs,[],Xs):- !.
merge1([],Ys,Ys):- !.

%not_merge1(X,Y,Z) :-  cnegf(merge1(X,Y,Z)).
