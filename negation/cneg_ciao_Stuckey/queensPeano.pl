:- module(queensPeano,_,[.(cneg), .(debugger_pkg)]).
% :- module(queensPeano,_,[.(cneg)]).
 
% :- module(queensPeano, [queens/2]).
% :- use_module(dist,[dist/2]).  

  
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
        diseq(Queen,YNb, []),
	no_attack_down(Y,Nb,Queen), 
        add(Nb,s(0),Nb1),
        no_attack1(Ys, Queen, Nb1).

no_attack_down(Y,Nb,Queen):-
	greater(Y,Nb),
	subst(Y,Nb,NbY),
	diseq(Queen,NbY, []).
no_attack_down(Y,Nb,_Queen):-
	greater(Nb,Y).
no_attack_down(Y,Nb,Queen):-
	Y=Nb, 
	diseq(Queen,0, []).

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
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%queens(0,Q).
%queens(s(0),Q).
%queens(s(s(0)),Q).
%queens(s(s(s(0))),Q).
%queens(s(s(s(s(0)))),Q). 
%queens(s(s(s(s(s(0))))),Q).
 
% select1(X, Y, Z):- 
% 	Y=[X|Ys], 
% 	Z=Ys.
% select1(X, Y1, Z):-
% 	Y1=[Y|Ys], 
% 	Z=[Y|Zs],
%         select1(X, Ys, Zs).

% greater1(X1,0):-
% 	X1=s(_).
% greater1(X1,Y1):-
% 	X1=s(X),
% 	Y1=s(Y),
% 	greater1(X,Y).

% no_queens(N,Q):-
%  	cneg([], queens(N,Q)).
% no_queens1(N,L,Q):-
%  	cneg([], queens1(N,L,Q)).

% no_queens_list(N,Q):-
%  	cneg([], queens_list(N,Q)).
% no_greater(X,Y):-
%  	cneg([], greater(X,Y)).

% no_greater1(X,Y):-
%  	cneg([], greater1(X,Y)).

  
 
% p9(X):-X=s(_).

% p91(s(_)).

% no_p9(X):-
% 	cneg([], p9(X)).
% no_p91(X):-
%    	cneg([], p91(X)). 


% no_subst(N,X,N1):-
%   	cneg([], subst(N,X,N1)).


% p11(X):-
% 	greater(s(0),0),
% 	subst(s(0),s(0),Y),			   
% 	queens_list(Y,Z),
% 	select(H,[s(0)|Z],J),
% 	no_attack(H,[]),
% 	queens1(J,[H],X).
% p12(X):-
% 	greater(s(0),0),
% %	subst(s(0),s(0),Y),			   
% 	queens_list(0,Z),
% 	select(H,[s(0)|Z],J),
% 	no_attack(H,[]),
% 	queens1(J,[H],X).
% p13(X):-
% 	greater(s(0),0),		   
% 	queens_list(0,Z),
% 	select(H,[s(0)|Z],J),
% 	no_attack(H,[]),
% 	queens1(J,[H],X).
% p14(X):-
% 	greater(s(0),0),		   
% 	select(H,[s(0)],J),
% 	no_attack(H,[]),
% 	queens1(J,[H],X).

% p15(X):-
% 	greater(s(0),0),		   
% 	select(H,[s(0)],J),
% 	no_attack(H,[]),
% 	queens1(J,[H],X).

% p16(X):-	   
% 	select(H,[s(0)],J),
% 	no_attack(H,[]),
% 	queens1(J,[H],X).

% p18(X):-	   
% 	selectK(H,[s(0)],J),
% 	queens1(J,[H],X).

% p10(X):-
% 	greater(s(0),0),		   
% 	no_attack(s(0),[]),
% 	queens1([],[s(0)],X).

% no_p10(X):-
% 	cneg([], p10(X)).
% no_p11(X):-
% 	cneg([], p11(X)).
% no_p12(X):-
% 	cneg([], p12(X)).
% no_p13(X):-
% 	cneg([], p13(X)).
% no_p14(X):-
% 	cneg([], p14(X)).

% no_p15(X):- 
% 	cneg([], p15(X)). 

% no_p16(X):-
% 	cneg([], p16(X)).

% no_p17(X):-
% 	cneg([], p17(X)). 
% no_p18(X):-
% 	cneg([], p18(X)). 
% no_p19(X):-
% 	cneg([], p19(X)). 
  
% no_select(H,I,J):-
% 	cneg([], select(H,I,J)).
 
% no_no_attack(H,L):-
%        	cneg([], no_attack(H,L)).

% selectK(s(0),[s(0)],[]).

% queens1K([],[s(0)],[s(0)]).

% select19(s(0)).

% queens19([],Q,Q).
% queens19([X|U],R,e).

% p19(X):-
% 	select19(H),
% 	queens19([],[H],X).
% get(w).
 
% put([],Q,Q).
% put([_|_],_,e).

% p20(X):-
% 	get(H),
% 	put([],[H],X).
% no_p20(X):-
% 	cneg([], p20(X)).
% p21(X):-
% 	get(H),
% 	put([],H,X).
% no_p21(X):-
% 	cneg([], p21(X)).
% no_p22(X):-
% 	cneg([], p22(X)).

% p22(X):-		   
% 	queens_list(0,Z),
% 	select(H,[s(0)|Z],J),
% 	no_attack(H,[]),
% 	queens1(J,[H],X).

% no_p23(X):-
% 	cneg([], p23(X)).

% p23(J):-		   
% 	queens_list(0,Z),
% 	select(s(0),[s(0)|Z],J).
% 	%queens1(J,[s(0)],X).

% no_p24(X):-
% 	cneg([], p24(X)).

% p24(J):-		   
% 	get1(Z),
% 	put1(Z,J).

% get1([]).

% put1(Ys, Ys).
% put1(Ys, [Y|Zs]):-
%         put1(Ys, Zs).

% no_p25(X):-
% 	cneg([], p25(X)).

% p25(J):-		   
% 	get1(Z),
% 	put2([Z],J). 

% put2([Ys], Ys).
% put2([_|Ys], [Y|Zs]):-
%         put2(Ys, Zs).
% p26(J):-		    
% 	get1(Z),
% 	select(s(0),[s(0)|Z],J).

% no_p26(X):-
% 	cneg([], p26(X)).

% no_select1(X,Y,Z):-
% 	cneg([], select1(X,Y,Z)).

% no_p27(X):-
% 	cneg([], p27(X)).
 
% p27(J):-		   
% 	select(s(0),[s(0)],J). 

% no_p28(X):-
% 	cneg([], p28(X)).

% p28(J):-		   
% 	get1(Z),
% 	select(s(0),[s(0)|Z],J).

% no_p17b(X):-
% 	cneg([], p17b(X)).

% p17b(X):-	   
% 	select(H,[s(0)],J),
% 	queens17b(J,[H],X).

% queens17b([],[H],[H]).
 
% p17(X):-	   
% 	select(H,[s(0)],J),
% 	queens1(J,[H],X).

% queens2([], Qs, Qs).
% queens2([X|Unplaced], Placed, Qs):-
%         select(Q, [X|Unplaced], NewUnplaced),
%         no_attack(Q, Placed),
%         queens2(NewUnplaced, [Q|Placed], Qs).
 
% no_queens2(X,Y,Z):-
%  	cneg([], queens2(X,Y,Z)).

% queens3([], Qs, Qs).
% queens3([X], Placed, Qs):-
%         no_attack(X, Placed),
%         queens3([], [X|Placed], Qs).
 
% no_queens3(X,Y,Z):-
%  	cneg([], queens3(X,Y,Z)).

% queens4([], Qs, Qs).
% queens4([X], Placed, Qs):-
%         queens4([], [X|Placed], Qs).
 
% no_queens4(X,Y,Z):-
%  	cneg([], queens4(X,Y,Z)).

% queens5([X], Placed, [X|Placed]):-
%         no_attack(X, Placed).
 
% no_queens5(X,Y,Z):-
%  	cneg([], queens5(X,Y,Z)).

% no_no_attack_down(X,Y,Z):- cneg([], no_attack_down(X,Y,Z)).

% nad(Y,Nb,Queen):-
% 	greater(Y,Nb),
% 	subst(Y,Nb,NbY),
% 	dist(Queen,NbY).
% nad(Y,Nb,_Queen):-
% 	greater(Nb,Y).
% nad(Y,Nb,Queen):-
% 	Y=Nb,
% 	dist(Queen,0).

% no_nad(X,Y,Z):- cneg([], nad(X,Y,Z)).


% p(Y):-
% 	dist(Y,s(0)),
% 	add(s(0),s(0),Y).

% no_p(Y):- cneg([], p(Y)).

% p1(X):- %% Son libres 
% 	dist(Y,s(0)),
% 	add(s(0),s(0),Y).

% no_p1(Y):- cneg([], p1(Y)). 

% p2(X):- %% Son libres
% 	dist(Y,s(0)),
% 	add(0,0,Y).

% no_p2(Y):- cneg([], p2(Y)).

% p3(X):- %% Son libres
% 	dist(Y,s(0)),
% 	add(0,s(0),Y).

% no_p3(Y):- cneg([], p3(Y)).

 
% queens6([], Qs, Qs).
% queens6([X], Placed, Qs):-
%         select(Q, [X], NewUnplaced),
%         queens6([], [Q|Placed], Qs).
 

% no_queens6(X,Y,Z):-
%  	cneg([], queens6(X,Y,Z)).



% queens7([X], Placed, [Q|Placed]):-
%         select(Q, [X], []).
 
% no_queens7(X,Y,Z):-
%  	cneg([], queens7(X,Y,Z)). %%% No funciona

% queens8([X], Placed, [X|Placed]):-  %%% funciona
%         select(X, [X], []).
 
% no_queens8(X,Y,Z):-
%  	cneg([], queens8(X,Y,Z)).


% queens9([X], Placed, [Q|Placed]):-
%         select(Q, [X], NewUnplaced).

% no_queens9(X,Y,Z):-
%  	cneg([], queens9(X,Y,Z)).
 
% select2(X, [X|Ys], Ys).

% select3(X, [Y|Ys], [Y|Zs]):-
%         select3(X, Ys, Zs).

% no_select2(X,Y,Z):- cneg([], select2(X,Y,Z)).
% no_select3(X,Y,Z):- cneg([], select3(X,Y,Z)).


% select4(X, [Y|Ys], [Y|Zs]):-
%         select4(X, Ys, Zs).
% select4(X, [X|Ys], Ys).
% no_select4(X,Y,Z):- cneg([], select4(X,Y,Z)).

% r(e):- 3=4.
% r(o):- 5=5.


% no_no_attack1(X,Y,Z):- cneg([], no_attack1(X,Y,Z)).



% no_add(X,Y,Z):- cneg([], add(X,Y,Z)).
 

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% queens11([], Qs, Qs). 
% queens11([X|Unplaced], Placed, Qs):-
%         select11(Q, [X|Unplaced], NewUnplaced), 
%         no_attack11(Q, Placed),
%         queens11(NewUnplaced, [Q|Placed], Qs).


% no_queens11(N,L,Q):-
%  	cneg([], queens11(N,L,Q)).

% no_attack11(s(0),[]).
% no_attack11(s(s(0)),[]).

% select11(s(s(0)),[s(s(0)),s(0)],[s(0)]).
% select11(s(0),[s(s(0)),s(0)],[s(s(0))]).

% queens_list1(0, []).
% queens_list1(N, [N|Ns]):-
%         greater(N,0),
%         subst(N,s(0),N1),
%         queens_list1(N1, Ns).

% no_queens_list1(N,Q):-
%  	cneg([], queens_list1(N,Q)).


% q1(N, Qs):-
% 	queens_list(N, Ns),
%         queens12(Ns, [], Qs).    

% no_q1(N,Q):-  
%  	cneg([], q1(N,Q)).

% queens12([], Qs, Qs).
% queens12([X|Unplaced], _Placed, _Qs).

% q2(N):-
% 	queens_list(N, Ns),
%         q13(Ns).    

% q13([]). 
% q13([_|_]).

% no_q2(N):-
%   	cneg([], q2(N)).




% zero(0).
% uno(1).

% q3(N):-
% 	g(N,_), % Si solo tiene el primer argumento funciona bien
%         zero(X).    

% g(N,_):-
% 	uno(N).

 
 

% no_q3(N):-
%   	cneg([], q3(N)).

% q4(N):-
% 	zero(_),
%         zero(Ns).    
% g4(N):-
%       	uno(N).
 
% no_q4(N):-
%   	cneg([], q4(N)). 
  

% list([]). 
% list([_|_]). 

% exist_list :- list(_X).
 
% no_exist_list :-
%   	cneg([], exist_list).
  
% zero_list :- list(0). 

% no_zero_list :-
%           	cneg([], zero_list).  
 

% queens13([], Qs, Qs).
% queens13([X], P, Qs):-
%         select(Q, [X], N), 
%         queens13([], [Q|P], Qs).
  
% no_queens13(X,Y,Z):- 
%        	cneg([], queens13(X,Y,Z)).


% predf([]).
% predf([X|L]):- 
% 	zero(Y),
% 	predf(L).

% no_predf(N):-
%   	cneg([], predf(N)). 

% goal1(S) :- cneg_aux((subst(s(0),s(0),N1),
%                                       queens_list(N1,Ns),
%                                       select(s(0),[s(0)|Ns],N),
%                                       queens1(N,[s(0)],S)),[S],[N,N1,Ns]).

% goal2(S) :- cneg_aux((subst(0,0,N1),
%                                       queens_list(N1,Ns),
%                                       select(s(0),[s(0)|Ns],N),
%                                       queens1(N,[s(0)],S)),[S],[N,N1,Ns]).

% goal3(S) :- cneg_aux((subst(s(0),s(0),N1),
%                                       queens_list(0,Ns),
%                                       select(s(0),[s(0)|Ns],N),
%                                       queens1(N,[s(0)],S)),[S],[N,N1,Ns]).

% % combinations([],N,0).
% % combinations([X,Y|L],N,X+Comb):-
% % 	subst(N,X,N1),
% % 	combinations([Y|L],N1,Comb),
% % 	obtain(Comb,Res),
% % 	add(X,Res,N).
% % combinations([X,Y|L],N,Comb-X):- 
% % 	add(N,X,N1),
% % 	combinations([Y|L],N1,Comb), 
% % 	obtain(Comb,Res),
% % 	subst(Res,X,N). 

% % obtain(X+Y,N):-
% % 	obtain(Y,NY),
% % 	add(X,NY,N).
% % obtain(X-Y,N):-
% % 	obtain(Y,NY),
% % 	subst(X,NY,N).
% % obtain(0,0).
 
% p5(X,Y):-
% 	p91(X),
% 	X=s(Y).

% no_p5(X,Y):- cneg([], p5(X,Y)).

% queens10(N, Qs):- 
%         queens_list10(N, Ns),
%         queens110(Ns, [], Qs).     % To place, placed, result

% queens14(N, Qs):- 
% %        queens_list(N, Ns),
%         queens1(Ns, [], Qs).     % To place, placed, result

% queens_list10(N, [s(s(0)),s(0)]).
% queens110( [], [], Qs). 

% no_queens10(X,Y):- cneg([], queens10(X,Y)).

% no_queens14(X,Y):- cneg([], queens14(X,Y)). 


% p6(X,Y,Z,W):-
% 	X=[1|Y],
% 	Y=Z,
% 	W=s(X).

% no_p6(X,Y,Z,W):- cneg([], p6(X,Y,Z,W)).

% no_number1(X):- cneg([], number1(X)).



 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %               EXAMPLES  FOR PACKAGE CNEG                      %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mi_number(0).
% mi_number(s(_)). 

% odd(s(0)). 
% odd(s(s(X))):-
% 	odd(X).
  
% parent(a1,p). 
% parent(a2,p). 
% parent(p,h1).
% parent(p,h2).

% grandparent(X,Y):- 
% 	parent(X,Z),
% 	parent(Z,Y).

boole(0).
boole(1).

% binary_list([]).
% binary_list([Y|L]):-
% 	boole(Y),  
% 	binary_list(L).

% dist_list([]).
% dist_list([(X,Y)|L]):-
% 	dist(X,Y),
% 	dist_list(L).

% p(1,2,X,X):- 
% 	dist(X,3),
% 	dist(X,Z),
% 	q(Z).

% pp1(X):- 
% 	dist(X,3),
% 	dist(X,Z),
% 	q(Z).
 
% pp2(X):- 
% 	dist(X,3),
% 	dist(X,_Z).
% pp3(X):- 
% 	dist(X,3),
% 	dist(X,5).

% p4(X,Y):- 
% 	dist(X,3),
% 	dist(Y,5).
 
% q(Z):- dist(Z,0).
% q(Z):- r(Z,_W). 

% r(8,9). 

positive(0). 
positive(s(X)):-
	positive(X).  

% natural(X):-
% 	dist(X,0), 
% 	positive(X). 
 
% pred2(7,_). 
% pred2(9,Y):- 
% 	dist(Y,5),  
% 	pred1(_X).  
% pred1(2).

% iguales(X,X).

% r1(1,1).
% r1(2,2).

% r2(X,Y):-
% 	r(X,Y).
% r3(1,2).
% r3(2,3).

% r4(X,Y):-
% 	r3(X,Z),
% 	r3(Z,Y).

% r5(X,Y,Z):-
% 	r3(X,Y),
% 	r3(Y,Z).

% not_odd_number(X):- cneg([], odd(X)).   
% no_mi_number(X):- cneg([], mi_number(X)).
% no_boole(X):- cneg([], boole(X)).
% no_binary_list(X):-cneg([], binary_list(X)). 
% no_dist_list(X):-cneg([], dist_list(X)).
% no_p(V1,V2,VX,VY):-cneg([], p(V1,V2,VX,VY)). 
% no_q(Z):-cneg([], q(Z)).
% no_r(X,Y):-cneg([], r(X,Y)). 
% no_pp1(X):-cneg([], pp1(X)). 
% no_pp2(X):-cneg([], pp2(X)).
% no_pp3(X):-cneg([], pp3(X)).
% no_p4(X,Y):-cneg([], p4(X,Y)).
% no_positive(X):-cneg([], positive(X)).
% no_natural(X):-cneg([], natural(X)).
% no_parent(X,Y):-cneg([], parent(X,Y)). 
% no_grandparent(X,Y):-cneg([], grandparent(X,Y)). 
% no_pred2(X,Y):-cneg([], pred2(X,Y)).
% no_pred1(X):-cneg([], pred1(X)).
% no_iguales(X,Y):-cneg([], iguales(X,Y)).  
% no_r1(X,Y):-cneg([], r1(X,Y)).
% no_r2(X,Y):-cneg([], r2(X,Y)).
% no_r3(X,Y):-cneg([], r3(X,Y)).
% no_r4(X,Y):-cneg([], r4(X,Y)).
% no_r5(X,Y,Z):-cneg([], r5(X,Y,Z)).

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

append1([],L,L).
append1([X|L],L1,[X|R]):-
	append1(L,L1,R).

prefix(P,L):- 
	append1(P,_S,L).
suffix(S,L):- 
	append1(_P,S,L).

sublist([],_List).
sublist([El|Sub],List):- 
	prefix(P,List),
	suffix([El|Sub],P).

no_append1(X,Y,Z):-cneg([], append1(X,Y,Z)).
no_prefix(P,L):-cneg([], prefix(P,L)).
no_suffix(S,L):-cneg([], suffix(S,L)).
no_sublist(L1,L2):-cneg([], sublist(L1,L2)).

% no:-cneg([], ).




less(0,s(_X)).
less(s(X),s(Y)):-
	less(X,Y).

even(0).
even(s(s(X))):-
	even(X).

no_even(X):- cneg([], even(X)).

% eq(0,0).
% eq(s(X),s(Y)):-
% 	eq(X,Y).

parent1(john, mary).
parent1(john, peter).
parent1(mary, joe).
parent1(peter, susan).

ancestor(X, Y):-
 	parent1(X, Y).
ancestor(X, Y):-
	parent1(Z, Y),
	ancestor(X, Z).

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

no_grandparent2(Y,X):- cneg([], grandparent2(Y,X)).

