:- module(queensPeano,_,[.(cneg)]).
 
%:- module(queensPeano, [queens/2]).

%:- use_module(dist,[dist/2]). 

  
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

select1(X, Y, Z):- 
	Y=[X|Ys], 
	Z=Ys.
select1(X, Y1, Z):-
	Y1=[Y|Ys], 
	Z=[Y|Zs],
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
subst(X,X,0).
 
number(0).
number(s(X)):-
	number(X).

greater(s(X),0):-
	number(X).
greater(s(X),s(Y)):-
	greater(X,Y).
 
greater1(X1,0):-
	X1=s(_).
greater1(X1,Y1):-
	X1=s(X),
	Y1=s(Y),
	greater1(X,Y).

%queens(0,Q).
%queens(s(0),Q).
%queens(s(s(0)),Q).
%queens(s(s(s(0))),Q).
%queens(s(s(s(s(0)))),Q). 
%queens(s(s(s(s(s(0))))),Q).
 
no_queens(N,Q):-
 	cneg(queens(N,Q)).
no_queens1(N,L,Q):-
 	cneg(queens1(N,L,Q)).

no_queens_list(N,Q):-
 	cneg(queens_list(N,Q)).
no_greater(X,Y):-
 	cneg(greater(X,Y)).

no_greater1(X,Y):-
 	cneg(greater1(X,Y)).

  
 
p9(X):-X=s(_).

p91(s(_)).

no_p9(X):-
	cneg(p9(X)).
no_p91(X):-
   	cneg(p91(X)). 


no_subst(N,X,N1):-
  	cneg(subst(N,X,N1)).


p11(X):-
	greater(s(0),0),
	subst(s(0),s(0),Y),			   
	queens_list(Y,Z),
	select(H,[s(0)|Z],J),
	no_attack(H,[]),
	queens1(J,[H],X).
p12(X):-
	greater(s(0),0),
%	subst(s(0),s(0),Y),			   
	queens_list(0,Z),
	select(H,[s(0)|Z],J),
	no_attack(H,[]),
	queens1(J,[H],X).
p13(X):-
	greater(s(0),0),		   
	queens_list(0,Z),
	select(H,[s(0)|Z],J),
	no_attack(H,[]),
	queens1(J,[H],X).
p14(X):-
	greater(s(0),0),		   
	select(H,[s(0)],J),
	no_attack(H,[]),
	queens1(J,[H],X).

p15(X):-
	greater(s(0),0),		   
	select(H,[s(0)],J),
	no_attack(H,[]),
	queens1(J,[H],X).

p16(X):-	   
	select(H,[s(0)],J),
	no_attack(H,[]),
	queens1(J,[H],X).

p18(X):-	   
	selectK(H,[s(0)],J),
	queens1(J,[H],X).

p10(X):-
	greater(s(0),0),		   
	no_attack(s(0),[]),
	queens1([],[s(0)],X).

no_p10(X):-
	cneg(p10(X)).
no_p11(X):-
	cneg(p11(X)).
no_p12(X):-
	cneg(p12(X)).
no_p13(X):-
	cneg(p13(X)).
no_p14(X):-
	cneg(p14(X)).

no_p15(X):- 
	cneg(p15(X)). 

no_p16(X):-
	cneg(p16(X)).

no_p17(X):-
	cneg(p17(X)). 
no_p18(X):-
	cneg(p18(X)). 
no_p19(X):-
	cneg(p19(X)). 
  
no_select(H,I,J):-
	cneg(select(H,I,J)).
 
no_no_attack(H,L):-
       	cneg(no_attack(H,L)).

selectK(s(0),[s(0)],[]).

queens1K([],[s(0)],[s(0)]).

select19(s(0)).

queens19([],Q,Q).
queens19([X|U],R,e).

p19(X):-
	select19(H),
	queens19([],[H],X).
get(w).
 
put([],Q,Q).
put([_|_],_,e).

p20(X):-
	get(H),
	put([],[H],X).
no_p20(X):-
	cneg(p20(X)).
p21(X):-
	get(H),
	put([],H,X).
no_p21(X):-
	cneg(p21(X)).
no_p22(X):-
	cneg(p22(X)).

p22(X):-		   
	queens_list(0,Z),
	select(H,[s(0)|Z],J),
	no_attack(H,[]),
	queens1(J,[H],X).

no_p23(X):-
	cneg(p23(X)).

p23(J):-		   
	queens_list(0,Z),
	select(s(0),[s(0)|Z],J).
	%queens1(J,[s(0)],X).

no_p24(X):-
	cneg(p24(X)).

p24(J):-		   
	get1(Z),
	put1(Z,J).

get1([]).

put1(Ys, Ys).
put1(Ys, [Y|Zs]):-
        put1(Ys, Zs).

no_p25(X):-
	cneg(p25(X)).

p25(J):-		   
	get1(Z),
	put2([Z],J). 

put2([Ys], Ys).
put2([_|Ys], [Y|Zs]):-
        put2(Ys, Zs).
p26(J):-		    
	get1(Z),
	select(s(0),[s(0)|Z],J).

no_p26(X):-
	cneg(p26(X)).

no_select1(X,Y,Z):-
	cneg(select1(X,Y,Z)).

no_p27(X):-
	cneg(p27(X)).
 
p27(J):-		   
	select(s(0),[s(0)],J). 

no_p28(X):-
	cneg(p28(X)).

p28(J):-		   
	get1(Z),
	select(s(0),[s(0)|Z],J).

no_p17b(X):-
	cneg(p17b(X)).

p17b(X):-	   
	select(H,[s(0)],J),
	queens17b(J,[H],X).

queens17b([],[H],[H]).
 
p17(X):-	   
	select(H,[s(0)],J),
	queens1(J,[H],X).

queens2([], Qs, Qs).
queens2([X|Unplaced], Placed, Qs):-
        select(Q, [X|Unplaced], NewUnplaced),
        no_attack(Q, Placed),
        queens2(NewUnplaced, [Q|Placed], Qs).
 
no_queens2(X,Y,Z):-
 	cneg(queens2(X,Y,Z)).

queens3([], Qs, Qs).
queens3([X], Placed, Qs):-
        no_attack(X, Placed),
        queens3([], [X|Placed], Qs).
 
no_queens3(X,Y,Z):-
 	cneg(queens3(X,Y,Z)).

queens4([], Qs, Qs).
queens4([X], Placed, Qs):-
        queens4([], [X|Placed], Qs).
 
no_queens4(X,Y,Z):-
 	cneg(queens4(X,Y,Z)).

queens5([X], Placed, [X|Placed]):-
        no_attack(X, Placed).
 
no_queens5(X,Y,Z):-
 	cneg(queens5(X,Y,Z)).

no_no_attack_down(X,Y,Z):- cneg(no_attack_down(X,Y,Z)).

nad(Y,Nb,Queen):-
	greater(Y,Nb),
	subst(Y,Nb,NbY),
	dist(Queen,NbY).
nad(Y,Nb,_Queen):-
	greater(Nb,Y).
nad(Y,Nb,Queen):-
	Y=Nb,
	dist(Queen,0).

no_nad(X,Y,Z):- cneg(nad(X,Y,Z)).


p(Y):-
	dist(Y,s(0)),
	add(s(0),s(0),Y).

no_p(Y):- cneg(p(Y)).

p1(X):- %% Son libres 
	dist(Y,s(0)),
	add(s(0),s(0),Y).

no_p1(Y):- cneg(p1(Y)). 

p2(X):- %% Son libres
	dist(Y,s(0)),
	add(0,0,Y).

no_p2(Y):- cneg(p2(Y)).

p3(X):- %% Son libres
	dist(Y,s(0)),
	add(0,s(0),Y).

no_p3(Y):- cneg(p3(Y)).

 
queens6([], Qs, Qs).
queens6([X], Placed, Qs):-
        select(Q, [X], NewUnplaced),
        queens6([], [Q|Placed], Qs).
 

no_queens6(X,Y,Z):-
 	cneg(queens6(X,Y,Z)).



queens7([X], Placed, [Q|Placed]):-
        select(Q, [X], []).
 
no_queens7(X,Y,Z):-
 	cneg(queens7(X,Y,Z)). %%% No funciona

queens8([X], Placed, [X|Placed]):-  %%% funciona
        select(X, [X], []).
 
no_queens8(X,Y,Z):-
 	cneg(queens8(X,Y,Z)).


queens9([X], Placed, [Q|Placed]):-
        select(Q, [X], NewUnplaced).

no_queens9(X,Y,Z):-
 	cneg(queens9(X,Y,Z)).
 
select2(X, [X|Ys], Ys).

select3(X, [Y|Ys], [Y|Zs]):-
        select3(X, Ys, Zs).

no_select2(X,Y,Z):- cneg(select2(X,Y,Z)).
no_select3(X,Y,Z):- cneg(select3(X,Y,Z)).


select4(X, [Y|Ys], [Y|Zs]):-
        select4(X, Ys, Zs).
select4(X, [X|Ys], Ys).
no_select4(X,Y,Z):- cneg(select4(X,Y,Z)).

r(e):- 3=4.
r(o):- 5=5.


no_no_attack1(X,Y,Z):- cneg(no_attack1(X,Y,Z)).



no_add(X,Y,Z):- cneg(add(X,Y,Z)).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

queens11([], Qs, Qs). 
queens11([X|Unplaced], Placed, Qs):-
        select11(Q, [X|Unplaced], NewUnplaced), 
        no_attack11(Q, Placed),
        queens11(NewUnplaced, [Q|Placed], Qs).


no_queens11(N,L,Q):-
 	cneg(queens11(N,L,Q)).

no_attack11(s(0),[]).
no_attack11(s(s(0)),[]).

select11(s(s(0)),[s(s(0)),s(0)],[s(0)]).
select11(s(0),[s(s(0)),s(0)],[s(s(0))]).

queens_list1(0, []).
queens_list1(N, [N|Ns]):-
        greater(N,0),
        subst(N,s(0),N1),
        queens_list1(N1, Ns).

no_queens_list1(N,Q):-
 	cneg(queens_list1(N,Q)).


q1(N, Qs):-
	queens_list(N, Ns),
        queens12(Ns, [], Qs).    

no_q1(N,Q):-  
 	cneg(q1(N,Q)).

queens12([], Qs, Qs).
queens12([X|Unplaced], _Placed, _Qs).

q2(N):-
	queens_list(N, Ns),
        q13(Ns).    

q13([]). 
q13([_|_]).

no_q2(N):-
  	cneg(q2(N)).




zero(0).
uno(1).

q3(N):-
	g(N,_), % Si solo tiene el primer argumento funciona bien
        zero(X).    

g(N,_):-
	uno(N).

 
 

no_q3(N):-
  	cneg(q3(N)).

q4(N):-
	zero(_),
        zero(Ns).    
g4(N):-
      	uno(N).
 
no_q4(N):-
  	cneg(q4(N)). 
  

list([]). 
list([_|_]). 

exist_list :- list(_X).
 
no_exist_list :-
  	cneg(exist_list).
  
zero_list :- list(0). 

no_zero_list :-
          	cneg(zero_list).  
 

queens13([], Qs, Qs).
queens13([X], P, Qs):-
        select(Q, [X], N), 
        queens13([], [Q|P], Qs).
  
no_queens13(X,Y,Z):- 
       	cneg(queens13(X,Y,Z)).


predf([]).
predf([X|L]):- 
	zero(Y),
	predf(L).

no_predf(N):-
  	cneg(predf(N)). 

goal1(S) :- cneg_aux((subst(s(0),s(0),N1),
                                      queens_list(N1,Ns),
                                      select(s(0),[s(0)|Ns],N),
                                      queens1(N,[s(0)],S)),[S],[N,N1,Ns]).

goal2(S) :- cneg_aux((subst(0,0,N1),
                                      queens_list(N1,Ns),
                                      select(s(0),[s(0)|Ns],N),
                                      queens1(N,[s(0)],S)),[S],[N,N1,Ns]).

goal3(S) :- cneg_aux((subst(s(0),s(0),N1),
                                      queens_list(0,Ns),
                                      select(s(0),[s(0)|Ns],N),
                                      queens1(N,[s(0)],S)),[S],[N,N1,Ns]).

% combinations([],N,0). 
% combinations([X,Y|L],N,X+Comb):-
% 	subst(N,X,N1),
% 	combinations([Y|L],N1,Comb),
% 	obtain(Comb,Res),
% 	add(X,Res,N).
% combinations([X,Y|L],N,Comb-X):-
% 	add(N,X,N1),
% 	combinations([Y|L],N1,Comb), 
% 	obtain(Comb,Res),
% 	subst(Res,X,N). 

% obtain(X+Y,N):-
% 	obtain(Y,NY),
% 	add(X,NY,N).
% obtain(X-Y,N):-
% 	obtain(Y,NY),
% 	subst(X,NY,N).
% obtain(0,0).
