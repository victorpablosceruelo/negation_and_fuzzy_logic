:- module(queensPeano_cnegf,_,[.(cnegf)]).
 
%:- module(queensPeano, [queens/2]).

:- use_module(dist,[dist/2]).  

  
% :- set_prolog_flag(multi_arity_warnings, off).

% queens(N,Qs) returns in Qs the column where we must place each of N
% queens in a Checkerboard of NxN assuming each of them is in a different
% row. For example :   
%  queens(s(s(s(s(0)))),[s(s(0)),s(s(s(s(0)))),s(0),s(s(s(0)))])
% means that the 4 queens are placed in positions (1,2),(2,4),(3,1) and
% (4,3). 
queens(N, Qs):- 
        queens_list(N, Ns),
        queens1(Ns, [], Qs).     % To place, placed, result

% queens1(Ns,[],Qs) returns in Qs a permutation of the columns represented
% in Ns such as there will be secure position for placing queens in them.
% queens1(Unplaced, Placed, Qs) appends to Placed columns the columns of 
% Unplaced in a secure way for all the queens.
queens1([], Qs, Qs). 
queens1([X|Unplaced], Placed, Qs):-
        select(Q, [X|Unplaced], NewUnplaced),
        no_attack(Q, Placed),
        queens1(NewUnplaced, [Q|Placed], Qs).
 
% select(X, Ys, Zs) X is an element of Ys and Zs is Ys except X
select(X, [X|Ys], Ys).
select(X, [Y|Ys], [Y|Zs]):-
        select(X, Ys, Zs).

% no_attack(Q, Safe) checks that a queen in the next row to the ones placed
% in Safe doesn't attack all the queens of Save if we place it in column Q
no_attack(Q, Safe):- no_attack1(Safe, Q, s(0)). 
 
% no_attack1(Safe,Q,Nb) checks that a queen in the next row to the ones placed
% in Safe doesn't attack all the queens of Save fron any diagonal with a 
% distance Nb if we place it in column Q
no_attack1([], _Queen, _Nb).
no_attack1([Y|Ys], Queen, Nb):-
	add(Y,Nb,YNb),
        dist(Queen,YNb),
	no_attack_down(Y,Nb,Queen), 
        add(Nb,s(0),Nb1),
        no_attack1(Ys, Queen, Nb1).

no_attack_down(Y,Nb,Queen):-
	greater(Y,Nb),
	subst(Y,Nb,NbY),
	dist(Queen,NbY).
no_attack_down(Y,Nb,_Queen):-
	greater(Nb,Y).
no_attack_down(Y,Nb,Queen):-
	Y=Nb, 
	dist(Queen,0).

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
 
number1(0).
number1(s(X)):-
	number1(X).

greater(s(X),0):-
	number1(X).
greater(s(X),s(Y)):-
	greater(X,Y).
 
