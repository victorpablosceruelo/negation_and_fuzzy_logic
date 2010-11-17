%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(queens, _,[]).
 
:- use_module(neg).
:- use_module(dist,[dist/2]).

queens(N, Qs):-
        queens_list(N, Ns),
        queens1(Ns, [], Qs).     

queens1([], Qs, Qs).
queens1([X|Unplaced], Placed, Qs):-
        select(Q, [X|Unplaced], NewUnplaced),
        no_attack(Q, Placed),
        queens1(NewUnplaced, [Q|Placed], Qs).
 
no_attack(Q, Safe):- no_attack1(Safe, Q, s(0)). 
 
no_attack1([], _Queen, _Nb).
no_attack1([Y|Ys], Queen, Nb):-
	add(Y,Nb,YNb),
        dist(Queen,YNb),
 	subst(Y,Nb,NbY),
        dist(Queen,NbY),
        add(Nb,s(0),Nb1),
        no_attack1(Ys, Queen, Nb1).

select(X, [X|Ys], Ys).
select(X, [Y|Ys], [Y|Zs]):-
        select(X, Ys, Zs).

select1(X, Y, Z):- 
	Y=[X|Ys], 
	Z=Ys.
select1(X, Y1, Z):-
	Y1=[Y|Ys], 
	Z= [Y|Zs],
        select1(X, Ys, Zs).

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
subst(Z,X,neg):-
	greater(X,Z).
subst(X,X,0).

greater(s(_),0).
greater(s(X),s(Y)):-
	greater(X,Y).

greater1(X1,0):-
	X1=s(_).
greater1(X1,Y1):-
	X1=s(X),
	Y1=s(Y),
	greater1(X,Y).

no_queens(N,Q):-
 	neg(queens(N,Q)).
no_queens1(N,L,Q):-
 	neg(queens1(N,L,Q)).

no_queens_list(N,Q):-
 	neg(queens_list(N,Q)).
no_greater(X,Y):-
 	neg(greater(X,Y)).

no_greater1(X,Y):-
 	neg(greater1(X,Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
