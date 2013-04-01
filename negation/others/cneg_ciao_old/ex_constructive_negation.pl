% :- module(ex_constructive_negation,_,[.(cneg)]).
:- module(ex_constructive_negation,_,[.(cneg), .(debugger_pkg)]).

% This module still fails to load.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             EXAMPLES  FOR PACKAGE CNEG               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NUMBERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boole(0).
boole(1).

digit(0).
digit(s(0)).
digit(s(s(0))).
digit(s(s(s(0)))).
digit(s(s(s(s(0))))).
digit(s(s(s(s(s(0)))))).
digit(s(s(s(s(s(s(0))))))).
digit(s(s(s(s(s(s(s(0)))))))).
digit(s(s(s(s(s(s(s(s(0))))))))).

positive(0). 
positive(s(X)):-
	positive(X).  

number1(0).
number1(s(X)):-
	number1(X).

greater(s(X),0):-
	number1(X).
greater(s(X),s(Y)):-
	greater(X,Y).

% TEST

no_boole(X):- cneg_rt([], boole(X)).
no_boole2(X):- cneg_rt([], (boole(X), boole(X))).
no_greater_than_3(X):- digit(X), cneg_rt([], greater(X,s(s(s(0))))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% QUEENS                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% queens(N,Qs) returns in Qs the column where we must place each of N
% queens in a Checkerboard of NxN assuming each of them is in a different
% row. For example :   
%  queens(s(s(s(s(0)))),[s(s(0)),s(s(s(s(0)))),s(0),s(s(s(0)))])
% means that the 4 queens are placed in positions (1,2),(2,4),(3,1) and
% (4,3). 
queens(N, Qs):- 
        queens_list(N, Ns),
        queens_try(Ns, [], Qs).     % To place, placed, result

% queens_try(Ns,[],Qs) returns in Qs a permutation of the columns represented
% in Ns such as there will be secure position for placing queens in them.
% queens_try(Unplaced, Placed, Qs) appends to Placed columns the columns of 
% Unplaced in a secure way for all the queens.
queens_try([], Qs, Qs). 
queens_try([X|Unplaced], Placed, Qs):-
        select(Q, [X|Unplaced], NewUnplaced),
        no_attack(Placed, Q, s(0)),
        queens_try(NewUnplaced, [Q|Placed], Qs).
 
% select(X, Ys, Zs) X is an element of Ys and Zs is Ys except X
select(X, [X|Ys], Ys).
select(X, [Y|Ys], [Y|Zs]):-
        select(X, Ys, Zs).

% no_attack(Q, Safe) checks that a queen in the next row to the ones placed
% in Safe doesn't attack all the queens of Save if we place it in column Q
% no_attack1(Safe,Q,Nb) checks that a queen in the next row to the ones placed
% in Safe doesn't attack all the queens of Save fron any diagonal with a 
% distance Nb if we place it in column Q
no_attack([], _Queen, _Nb).
no_attack([Y|Ys], Queen, Nb):-
	add(Y,Nb,YNb),
        disequality(Queen,YNb, []),
	no_attack_down(Y,Nb,Queen), 
        add(Nb,s(0),Nb1),
        no_attack(Ys, Queen, Nb1).

no_attack_down(Y,Nb,Queen):-
	greater(Y,Nb),
	subst(Y,Nb,NbY),
	disequality(Queen,NbY, []).
no_attack_down(Y,Nb,_Queen):-
	greater(Nb,Y).
no_attack_down(Y,Nb,Queen):-
	Y=Nb, 
	disequality(Queen,0, []).

queens_list(0, []).
queens_list(N, [N|Ns]):-
        greater(N,0),
        subst(N,s(0),N1),
        queens_list(N1, Ns).


add(0,X,X). 
add(s(X),Y,s(Z)):-  
	add(X,Y,Z).

subst(Z,X,Y):-
	greater(Z,X),
	add(X,Y,Z).
subst(X,X,0).
 
% TESTS

queens1(Q) :- queens(s(0),Q).
queens2(Q) :- queens(s(s(0)),Q).
queens3(Q) :- queens(s(s(s(0))),Q).
queens4(Q) :- queens(s(s(s(s(0)))),Q).

no_queens1(Q):- cneg_rt([], queens(s(0),Q)). % Q =/= [s(0)]
no_queens2(Q):- cneg_rt([], queens(s(s(0)),Q)). % True always
no_queens3(Q):- cneg_rt([], queens(s(s(s(0))),Q)). % True always
no_queens4(Q):- cneg_rt([], queens(s(s(s(0))),Q)). % 10^4 sols.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVEN AND ODD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sum(0,X,X).
sum(s(X),Y,s(Z)):- sum(X,Y,Z).

even(X):- sum(Y,Y,X).

% TEST

odd1(X):- cneg_rt([], even(X)).
odd2(X):- number1(X), cneg_rt([], even(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSERT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member1(X,[X|_Ys]).
member1(X,[_Y|Ys]):- member1(X,Ys).

insert(X,Xs,[X|Xs]):- cneg_rt([], member1(X,Xs)).
% insert(X,Xs,Xs):- member1(X,Xs).

% TEST

test1_insert(X,L):- insert(X,[3,4],L).
test2_insert(X,Y,L2):- insert(X,[Y],L2).

%%%%%%%%%%%%%%%%%%%%%%%
% GRAPHS
%%%%%%%%%%%%%%%%%%%%%%%

node(a).
node(b).
node(c).
node(d).

path(a,b).
path(a,c).

connected(X, Y) :- node(X), node(Y), 
	(
	    path(X,Y)
	; 
	    path(Y, X)
	).

% TEST
aux_connected(X) :- connected(X,_Y).
test_connected(X):- cneg_rt([], aux_connected(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BARTAK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(a,f(Z)):- t(Z).
p(f(Z),b):- t(Z).
t(c).

% TEST

no_p(X,Y):- cneg_rt([], p(X,Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SYMMETRIC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

symmetric(o).
symmetric(f1(X)):- symmetric(X).
symmetric(f2(X,Y)):- mirror(X,Y).

mirror(o,o).
mirror(f1(X),f1(Y)):- mirror(X,Y).
mirror(f2(X,Y),f2(Z,W)):- mirror(X,W), mirror(Y,Z).

% TEST

no_mirror(Z):- cneg_rt([], symmetric(Z)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DUPLICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_duplicates([X|Y]):- member1(X,Y). 
has_duplicates([_|Y]):- has_duplicates(Y).

list_of_digits([ ]).
list_of_digits([X|Y]):- digit1(X),list_of_digits(Y).

digit1(1).
digit1(2).
digit1(3).

% TEST

test_has_duplicates(L):- L=[_,_,_], cneg_rt([], has_duplicates(L)), list_of_digits(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DUPLICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjoint([],_).
disjoint([X|L1],L2):- cneg_rt([], member1(X,L2)), disjoint(L1,L2).

% TEST

test_disjoint1(X,Y):- list_of_digits([X,Y]),disjoint([1,2,3],[X,Y]).
test_disjoint2(X,Y):- list_of_digits([X,Y]),disjoint([1,2],[X,Y]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OTHER EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%

parent1(john, mary).
parent1(john, peter).
parent1(mary, joe).
parent1(peter, susan).

ancestor(X, Y):-
 	parent1(X, Y).
ancestor(X, Y):-
	parent1(Z, Y),
	ancestor(X, Z).

ancestor2(X, Y) :-
	parent1(X, Y).
ancestor2(X, Y) :-
	ancestor2(X, Z),
	ancestor2(Z, Y).

%peano(0,0).
%peano(N,s(P1)):-
%	N > 0,
%	N1 is N-1,
%	peano(N1,P1).

natural1(0).
natural1(s(X)):-
	natural1(X).

parent2(bob,mary).
parent2(mary,joan).

grandparent2(Y,X):- 
    parent2(Y,Z),
    parent2(Z,X).

no_grandparent2(Y,X):- cneg_rt([], grandparent2(Y,X)).

grandparent3(X, Y) :- grandparent3(X, Z), grandparent3(Z, Y).
grandparent3(X, Y) :- parent2(X,Y).

no_grandparent3(X, Y) :- cneg_rt([], grandparent3(X,Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

