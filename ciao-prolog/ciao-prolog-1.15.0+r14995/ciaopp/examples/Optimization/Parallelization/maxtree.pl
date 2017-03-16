:- module(maxtree,[maxtree/2],[assertions]).

:- entry maxtree(X,Y) : (ground(X),var(Y)).

maxtree(Tree,NewTree) :-
	maxtree_(Tree,Max,Max,NewTree).

maxtree_(void,_,0,void).
maxtree_(tree(Lbl,Lft,Rgt),Max,MaxSoFar,tree(Max,NewLft,NewRgt)) :-
	maxtree_(Lft,Max,MaxLft,NewLft),
	maxtree_(Rgt,Max,MaxRgt,NewRgt),
	max(Lbl,MaxLft,MaxRgt,MaxSoFar).

max(A,B,C,A) :- A >= B, A >= C.
max(A,B,C,B) :- B >= A, B >= C.
max(A,B,C,C) :- C >= A, C >= B.

%% input(tree(5,
%% 	tree(3,void,void),
%% 	tree(7,
%% 	   tree(4,void,void),
%% 	   tree(1,void,void)))).

