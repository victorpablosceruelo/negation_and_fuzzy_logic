:- module(trees,_).

tree(empty).
tree(tree(LC,_,RC)) :- tree(LC),tree(RC).

belongs(X,tree(_,X,_)).
belongs(X,tree(LC,Y,_RC)) :- X \= Y,belongs(X,LC).
belongs(X,tree(_LC,Y,RC)) :- X \= Y,belongs(X,RC).

in(empty,[]).
in(tree(LC,X,RC),L) :- in(LC,L1),in(RC,L2),app(L1,[X|L2],L).

% Sorted trees
sorted_tree(T) :- in(T,L),sorted_list(L).

insert(X,empty,tree(empty,X,empty)).
insert(X,tree(LC,X,RC),tree(LC,X,RC)).
insert(X,tree(LC,Y,RC),tree(LC_p,Y,RC)) :- X < Y,insert(X,LC,LC_p).
insert(X,tree(LC,Y,RC),tree(LC,Y,RC_p)) :- X > Y,insert(X,RC,RC_p).


% Example: tree(tree(empty,1,tree(empty,2,empty)),4,tree(empty,6,tree(empty,8,empty))).

% Auxiliar predicates
sorted_list([]).
sorted_list([_]).
sorted_list([X,Y|R]) :- X < Y,sorted_list([Y|R]).

app([],L,L).
app([X|R],L,[X|RL]) :- app(R,L,RL).
