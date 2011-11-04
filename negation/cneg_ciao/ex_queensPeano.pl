:- module(ex_queensPeano,_,[.(cneg), .(debugger_pkg)]).
% :- module(queensPeano,_,[.(cneg)]).
 
% :- module(queensPeano, [queens/2]).
% :- use_module(dist,[dist/2]).  

:- use_module(library(write), [write/1]).  
% :- set_prolog_flag(multi_arity_warnings, off).

test(N, Columns) :- % Let's see if we have invalid results.
	cneg_rt([], queens(N, Columns)), % First constraints.
	queens(N, Columns). % Secondly values generator.

% queens(N,Columns) returns in Columns the column where we must place each of N
% queens in a Checkerboard of NxN assuming each position in the list 
% is a different row. For example :   
%        queens(s(s(s(s(0)))),[s(s(0)),s(s(s(s(0)))),s(0),s(s(s(0)))])
% means that the 4 queens are placed in positions (1,2),(2,4),(3,1) and (4,3). 

queens(N, Columns):- 
        queens_list(N, Ns),
        queens1(Ns, [], Columns).     % To be placed, placed, result

% queens_list(N, List) generates a list of N queens in List.
% The order does not matter yet.
queens_list(0, []).
queens_list(s(N), [s(N)|List]) :-
        queens_list(N, List).

% queens1(Ns,[],Columns) returns in Columns a permutation of the columns represented
% in Ns such as there will be secure position for placing queens in them.
% queens1(Unplaced, Placed, Columns) appends to Placed columns the columns of 
% Unplaced in a secure way for all the queens.
queens1([], Columns, Columns). 
queens1([X|Unplaced], Placed, Columns):-
        select_column(Column, [X|Unplaced], NewUnplaced),
        no_attack(Placed, Column, Column),
        queens1(NewUnplaced, [Column|Placed], Columns).
 
% select(X, Ys, Zs) X is an element of Ys and Zs is Ys except X
select_column(X, [X|Ys], Ys).
select_column(X, [Y|Ys], [Y|Zs]):-
        select_column(X, Ys, Zs).

% no_attack(Placed, Q, Q) checks that 
% a queen in the next row to the ones in Placed 
% doesn't attack any queen there.
no_attack([], _Col_Add, _Col_Subst).
no_attack([Y|Ys], Col_Add, 0):-
	add(Col_Add, s(0), New_Col_Add),
        disequality(New_Col_Add, Y, []),
        no_attack(Ys, New_Col_Add, 0).
no_attack([Y|Ys], Col_Add, Col_Subst):-
	add(Col_Add, s(0), New_Col_Add),
        disequality(New_Col_Add, Y, []),
	subst(Col_Subst, s(0), New_Col_Subst),
	disequality(New_Col_Subst, Y, []),
        no_attack(Ys, New_Col_Add, New_Col_Subst).

add(0,X,X). 
add(s(X),Y,s(Z)) :-
	add(X,Y,Z).

subst(X,0,X).
subst(s(X),s(Y),Z) :-
	subst(X,Y,Z).

 
%number1(0).
%number1(s(X)):-
%	number1(X).

%greater(s(X),0):-
%	number1(X).
%greater(s(X),s(Y)):-
%	greater(X,Y).
 
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
%  	cneg_tr([], queens(N,Q)).
% no_queens1(N,L,Q):-
%  	cneg_tr([], queens1(N,L,Q)).

% no_queens_list(N,Q):-
%  	cneg_tr([], queens_list(N,Q)).
% no_greater(X,Y):-
%  	cneg_tr([], greater(X,Y)).

% no_greater1(X,Y):-
%  	cneg_tr([], greater1(X,Y)).

  
 
% p9(X):-X=s(_).

% p91(s(_)).

% no_p9(X):-
% 	cneg_tr([], p9(X)).
% no_p91(X):-
%    	cneg_tr([], p91(X)). 


% no_subst(N,X,N1):-
%   	cneg_tr([], subst(N,X,N1)).


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
% 	cneg_tr([], p10(X)).
% no_p11(X):-
% 	cneg_tr([], p11(X)).
% no_p12(X):-
% 	cneg_tr([], p12(X)).
% no_p13(X):-
% 	cneg_tr([], p13(X)).
% no_p14(X):-
% 	cneg_tr([], p14(X)).

% no_p15(X):- 
% 	cneg_tr([], p15(X)). 

% no_p16(X):-
% 	cneg_tr([], p16(X)).

% no_p17(X):-
% 	cneg_tr([], p17(X)). 
% no_p18(X):-
% 	cneg_tr([], p18(X)). 
% no_p19(X):-
% 	cneg_tr([], p19(X)). 
  
% no_select(H,I,J):-
% 	cneg_tr([], select(H,I,J)).
 
% no_no_attack(H,L):-
%        	cneg_tr([], no_attack(H,L)).

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
% 	cneg_tr([], p20(X)).
% p21(X):-
% 	get(H),
% 	put([],H,X).
% no_p21(X):-
% 	cneg_tr([], p21(X)).
% no_p22(X):-
% 	cneg_tr([], p22(X)).

% p22(X):-		   
% 	queens_list(0,Z),
% 	select(H,[s(0)|Z],J),
% 	no_attack(H,[]),
% 	queens1(J,[H],X).

% no_p23(X):-
% 	cneg_tr([], p23(X)).

% p23(J):-		   
% 	queens_list(0,Z),
% 	select(s(0),[s(0)|Z],J).
% 	%queens1(J,[s(0)],X).

% no_p24(X):-
% 	cneg_tr([], p24(X)).

% p24(J):-		   
% 	get1(Z),
% 	put1(Z,J).

% get1([]).

% put1(Ys, Ys).
% put1(Ys, [Y|Zs]):-
%         put1(Ys, Zs).

% no_p25(X):-
% 	cneg_tr([], p25(X)).

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
% 	cneg_tr([], p26(X)).

% no_select1(X,Y,Z):-
% 	cneg_tr([], select1(X,Y,Z)).

% no_p27(X):-
% 	cneg_tr([], p27(X)).
 
% p27(J):-		   
% 	select(s(0),[s(0)],J). 

% no_p28(X):-
% 	cneg_tr([], p28(X)).

% p28(J):-		   
% 	get1(Z),
% 	select(s(0),[s(0)|Z],J).

% no_p17b(X):-
% 	cneg_tr([], p17b(X)).

% p17b(X):-	   
% 	select(H,[s(0)],J),
% 	queens17b(J,[H],X).

% queens17b([],[H],[H]).
 
% p17(X):-	   
% 	select(H,[s(0)],J),
% 	queens1(J,[H],X).

% queens2([], Columns, Columns).
% queens2([X|Unplaced], Placed, Columns):-
%         select(Q, [X|Unplaced], NewUnplaced),
%         no_attack(Q, Placed),
%         queens2(NewUnplaced, [Q|Placed], Columns).
 
% no_queens2(X,Y,Z):-
%  	cneg_tr([], queens2(X,Y,Z)).

% queens3([], Columns, Columns).
% queens3([X], Placed, Columns):-
%         no_attack(X, Placed),
%         queens3([], [X|Placed], Columns).
 
% no_queens3(X,Y,Z):-
%  	cneg_tr([], queens3(X,Y,Z)).

% queens4([], Columns, Columns).
% queens4([X], Placed, Columns):-
%         queens4([], [X|Placed], Columns).
 
% no_queens4(X,Y,Z):-
%  	cneg_tr([], queens4(X,Y,Z)).

% queens5([X], Placed, [X|Placed]):-
%         no_attack(X, Placed).
 
% no_queens5(X,Y,Z):-
%  	cneg_tr([], queens5(X,Y,Z)).

% no_no_attack_down(X,Y,Z):- cneg_tr([], no_attack_down(X,Y,Z)).

% nad(Y,Nb,Queen):-
% 	greater(Y,Nb),
% 	subst(Y,Nb,NbY),
% 	dist(Queen,NbY).
% nad(Y,Nb,_Queen):-
% 	greater(Nb,Y).
% nad(Y,Nb,Queen):-
% 	Y=Nb,
% 	dist(Queen,0).

% no_nad(X,Y,Z):- cneg_tr([], nad(X,Y,Z)).


% p(Y):-
% 	dist(Y,s(0)),
% 	add(s(0),s(0),Y).

% no_p(Y):- cneg_tr([], p(Y)).

% p1(X):- %% Son libres 
% 	dist(Y,s(0)),
% 	add(s(0),s(0),Y).

% no_p1(Y):- cneg_tr([], p1(Y)). 

% p2(X):- %% Son libres
% 	dist(Y,s(0)),
% 	add(0,0,Y).

% no_p2(Y):- cneg_tr([], p2(Y)).

% p3(X):- %% Son libres
% 	dist(Y,s(0)),
% 	add(0,s(0),Y).

% no_p3(Y):- cneg_tr([], p3(Y)).

 
% queens6([], Columns, Columns).
% queens6([X], Placed, Columns):-
%         select(Q, [X], NewUnplaced),
%         queens6([], [Q|Placed], Columns).
 

% no_queens6(X,Y,Z):-
%  	cneg_tr([], queens6(X,Y,Z)).



% queens7([X], Placed, [Q|Placed]):-
%         select(Q, [X], []).
 
% no_queens7(X,Y,Z):-
%  	cneg_tr([], queens7(X,Y,Z)). %%% No funciona

% queens8([X], Placed, [X|Placed]):-  %%% funciona
%         select(X, [X], []).
 
% no_queens8(X,Y,Z):-
%  	cneg_tr([], queens8(X,Y,Z)).


% queens9([X], Placed, [Q|Placed]):-
%         select(Q, [X], NewUnplaced).

% no_queens9(X,Y,Z):-
%  	cneg_tr([], queens9(X,Y,Z)).
 
% select2(X, [X|Ys], Ys).

% select3(X, [Y|Ys], [Y|Zs]):-
%         select3(X, Ys, Zs).

% no_select2(X,Y,Z):- cneg_tr([], select2(X,Y,Z)).
% no_select3(X,Y,Z):- cneg_tr([], select3(X,Y,Z)).


% select4(X, [Y|Ys], [Y|Zs]):-
%         select4(X, Ys, Zs).
% select4(X, [X|Ys], Ys).
% no_select4(X,Y,Z):- cneg_tr([], select4(X,Y,Z)).

% r(e):- 3=4.
% r(o):- 5=5.


% no_no_attack1(X,Y,Z):- cneg_tr([], no_attack1(X,Y,Z)).



% no_add(X,Y,Z):- cneg_tr([], add(X,Y,Z)).
 

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% queens11([], Columns, Columns). 
% queens11([X|Unplaced], Placed, Columns):-
%         select11(Q, [X|Unplaced], NewUnplaced), 
%         no_attack11(Q, Placed),
%         queens11(NewUnplaced, [Q|Placed], Columns).


% no_queens11(N,L,Q):-
%  	cneg_tr([], queens11(N,L,Q)).

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
%  	cneg_tr([], queens_list1(N,Q)).


% q1(N, Columns):-
% 	queens_list(N, Ns),
%         queens12(Ns, [], Columns).    

% no_q1(N,Q):-  
%  	cneg_tr([], q1(N,Q)).

% queens12([], Columns, Columns).
% queens12([X|Unplaced], _Placed, _Columns).

% q2(N):-
% 	queens_list(N, Ns),
%         q13(Ns).    

% q13([]). 
% q13([_|_]).

% no_q2(N):-
%   	cneg_tr([], q2(N)).




% zero(0).
% uno(1).

% q3(N):-
% 	g(N,_), % Si solo tiene el primer argumento funciona bien
%         zero(X).    

% g(N,_):-
% 	uno(N).

 
 

% no_q3(N):-
%   	cneg_tr([], q3(N)).

% q4(N):-
% 	zero(_),
%         zero(Ns).    
% g4(N):-
%       	uno(N).
 
% no_q4(N):-
%   	cneg_tr([], q4(N)). 
  

% list([]). 
% list([_|_]). 

% exist_list :- list(_X).
 
% no_exist_list :-
%   	cneg_tr([], exist_list).
  
% zero_list :- list(0). 

% no_zero_list :-
%           	cneg_tr([], zero_list).  
 

% queens13([], Columns, Columns).
% queens13([X], P, Columns):-
%         select(Q, [X], N), 
%         queens13([], [Q|P], Columns).
  
% no_queens13(X,Y,Z):- 
%        	cneg_tr([], queens13(X,Y,Z)).


% predf([]).
% predf([X|L]):- 
% 	zero(Y),
% 	predf(L).

% no_predf(N):-
%   	cneg_tr([], predf(N)). 

% goal1(S) :- cneg_tr_aux((subst(s(0),s(0),N1),
%                                       queens_list(N1,Ns),
%                                       select(s(0),[s(0)|Ns],N),
%                                       queens1(N,[s(0)],S)),[S],[N,N1,Ns]).

% goal2(S) :- cneg_tr_aux((subst(0,0,N1),
%                                       queens_list(N1,Ns),
%                                       select(s(0),[s(0)|Ns],N),
%                                       queens1(N,[s(0)],S)),[S],[N,N1,Ns]).

% goal3(S) :- cneg_tr_aux((subst(s(0),s(0),N1),
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

% no_p5(X,Y):- cneg_tr([], p5(X,Y)).

% queens10(N, Columns):- 
%         queens_list10(N, Ns),
%         queens110(Ns, [], Columns).     % To place, placed, result

% queens14(N, Columns):- 
% %        queens_list(N, Ns),
%         queens1(Ns, [], Columns).     % To place, placed, result

% queens_list10(N, [s(s(0)),s(0)]).
% queens110( [], [], Columns). 

% no_queens10(X,Y):- cneg_tr([], queens10(X,Y)).

% no_queens14(X,Y):- cneg_tr([], queens14(X,Y)). 


% p6(X,Y,Z,W):-
% 	X=[1|Y],
% 	Y=Z,
% 	W=s(X).

% no_p6(X,Y,Z,W):- cneg_tr([], p6(X,Y,Z,W)).

% no_number1(X):- cneg_tr([], number1(X)).



 
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

% not_odd_number(X):- cneg_tr([], odd(X)).   
% no_mi_number(X):- cneg_tr([], mi_number(X)).
% no_boole(X):- cneg_tr([], boole(X)).
% no_binary_list(X):-cneg_tr([], binary_list(X)). 
% no_dist_list(X):-cneg_tr([], dist_list(X)).
% no_p(V1,V2,VX,VY):-cneg_tr([], p(V1,V2,VX,VY)). 
% no_q(Z):-cneg_tr([], q(Z)).
% no_r(X,Y):-cneg_tr([], r(X,Y)). 
% no_pp1(X):-cneg_tr([], pp1(X)). 
% no_pp2(X):-cneg_tr([], pp2(X)).
% no_pp3(X):-cneg_tr([], pp3(X)).
% no_p4(X,Y):-cneg_tr([], p4(X,Y)).
% no_positive(X):-cneg_tr([], positive(X)).
% no_natural(X):-cneg_tr([], natural(X)).
% no_parent(X,Y):-cneg_tr([], parent(X,Y)). 
% no_grandparent(X,Y):-cneg_tr([], grandparent(X,Y)). 
% no_pred2(X,Y):-cneg_tr([], pred2(X,Y)).
% no_pred1(X):-cneg_tr([], pred1(X)).
% no_iguales(X,Y):-cneg_tr([], iguales(X,Y)).  
% no_r1(X,Y):-cneg_tr([], r1(X,Y)).
% no_r2(X,Y):-cneg_tr([], r2(X,Y)).
% no_r3(X,Y):-cneg_tr([], r3(X,Y)).
% no_r4(X,Y):-cneg_tr([], r4(X,Y)).
% no_r5(X,Y,Z):-cneg_tr([], r5(X,Y,Z)).

  
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

no_append1(X,Y,Z):-cneg_tr([], append1(X,Y,Z)).
no_prefix(P,L):-cneg_tr([], prefix(P,L)).
no_suffix(S,L):-cneg_tr([], suffix(S,L)).
no_sublist(L1,L2):-cneg_tr([], sublist(L1,L2)).

% no:-cneg_tr([], ).




less(0,s(_X)).
less(s(X),s(Y)):-
	less(X,Y).

even(0).
even(s(s(X))):-
	even(X).

no_even(X):- cneg_tr([], even(X)).

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
	parent1(X, Z),
	ancestor(Z, Y).

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

grandparent2(X,Y):- 
    parent2(X,Z),
    parent2(Z,Y).

no_grandparent2(Y,X):- cneg_tr([], grandparent2(Y,X)).

% Problematic calls being fixed.
% (diseq_uqv([X,Y],[_692,_694],[_692,_694]);eq_uqv([X,Y],[_692,_694],[]),(diseq_uqv([X,Y],[john,mary],[]),diseq_uqv([X,Y],[john,peter],[]),diseq_uqv([X,Y],[mary,joe],[]),diseq_uqv([X,Y],[peter,susan],[]))),(diseq_uqv([X,Y],[_766,_768],[_766,_768]);eq_uqv([X,Y],[_766,_768],[]),cneg_rt_Chan([_775],(parent1(_766,_775),ancestor(_775,_768)))).