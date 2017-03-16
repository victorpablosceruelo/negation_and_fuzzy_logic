:- module(foo,[main/1],[assertions]).

 
:- true comp main(X,Y) : list(X) + equiv(main_list(X,Y)).
:- true comp main(X,Y) : tree(X) + equiv(main_tree(X,Y)).

%% main(X):- q(a,b,Z).
%% main(X):- q(b,c,Z).

main(Z):-
main_list(Z):-
main_tree(Z):-
	p(X,Y), q(X,Y,Z).

p(a,b).
p(c,d).

q(a,b,0).
q(c,d,0).
q(a,d,1).
q(c,b,1).
