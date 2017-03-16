 %% From Pascal Van Hentenryck's kalah.p
 %% Has the same number of predicates than in the "cardinality"
 %% paper:44 (legal/1 has been commented out because it is never
 %% reached, and thus the type analyzer doesn't infer any information). 
 %% Translated to Prolog and modified by Pedro Lopez. 5 - 9 - 96.
 %% Is simmilar to Shapiro's program except for the definition of some
 %% (input/output) predicates.
 %% The built-in predicate write/1 is defined as writepvh/1.

 %% Decomment for freeness & sharing analysis.

:- module(pv_kalah, [play/2], [assertions]).

:- entry play(A, B) : var * var.

 %% :- entry play(A,B) : kalahgame * var.
 %% :- regtype kalahgame/1.
 %% 
 %% kalahgame(kalah).

 %% Play framework

play(Game,Result) :-
   initialize(Game,Position,Player),
   displaygame(Position,Player),
   play3(Position,Player,Result).

play3(Position,Player,Result) :-
  gameover(Position,Player,Result),!,
  announce(Result).
play3(Position,Player,Result) :-
  choosemove(Position,Player,Move),
  move(Move,Position,Position1),
  displaygame(Position1,Player),
  nextplayer(Player,Player1),!,
  play3(Position1,Player1,Result).

 /* Choosing a move by minimax with alpha-beta cut-off  */

choosemove(Position,computer,Move) :-
   lookahead(Depth),
   alphabeta(Depth,Position,-40,40,Move,_Value),
   nl, writepvh(Move), nl.
choosemove(_Position,opponent,Move) :-
 %PVH   
 genlegal(Move).
   %% In Shapiro's: 
   %% nl, writeln(['please make move']), read(Move), legal(Move).


alphabeta(0,Position,_Alpha,_Beta,_Move,Value) :-
   value(Position,Value).
alphabeta(D,Position,Alpha,Beta,Move,Value) :-
  D > 0,
  allmoves(Position,Moves),
  Alpha1 is 0 - Beta,
  Beta1 is 0 - Alpha,
  D1 is D - 1,
  evaluateandchoose(Moves,Position,D1,Alpha1,Beta1,[],p(Move,Value)).
  %% Replaced nil by []. -PL 

%% This tries to simulate a call to findall(M,move2(Position,M),Moves),
%% but clearly is not correct.

 %% Original code.
 %% allmoves(Position,[X]) :-
 %%   move2(Position,X).
 %% allmoves(Position,[X|Xs]) :-
 %%   move2(Position,X),
 %%   allmoves(Position,Xs).

allmoves(Position,[X|Xs]) :-
  move2(Position,X),!,
  allmoves(Position,Xs).
allmoves(_Position,[]).

evaluateandchoose([Move|Moves],Position,D,Alpha,Beta,Record,BestMove) :-
  move(Move,Position,Position1),
  alphabeta(D,Position1,Alpha,Beta,_MoveX,Value),
  Value1 is 0 - Value,
  cutoff(Move,Value1,D,Alpha,Beta,Moves,Position,Record,BestMove).
evaluateandchoose([],_Position,_D,Alpha,_Beta,Move,p(Move,Alpha)).

cutoff(Move,Value,_D,_Alpha,Beta,_Moves,_Position,_Move1,p(Move,Value)) :-
  Value >= Beta.
cutoff(Move,Value,D,Alpha,Beta,Moves,Position,_Move1,BestMove) :-   
  Alpha < Value,
  Value < Beta,
  evaluateandchoose(Moves,Position,D,Value,Beta,Move,BestMove).
cutoff(_Move,Value,D,Alpha,Beta,Moves,Position,Move1,BestMove) :-   
  Value =< Alpha, 
  evaluateandchoose(Moves,Position,D,Alpha,Beta,Move1,BestMove).

move2(Board,[M|Ms]) :-
   ismember(M,[1,2,3,4,5,6]),
   stonesinhole(M,Board,N),
   extendmove(N,M,Board,Ms).
move2(board([0,0,0,0,0,0],_K,_Ys,_L),[]).


stonesinhole(M,board(Hs,_K,_Ys,_L),Stones) :-
  nthmember(M,Hs,Stones),
  Stones > 0.

extendmove(Stones,M,_Board,[]) :-
  Stones =\= (7 - M) mod 13, !.
extendmove(Stones,M,Board,Ms) :-
  Stones =:= (7 - M) mod 13,
  distributestones(Stones,M,Board,Board1),
  move2(Board1,Ms).

/*  Executing a move  */

move([N|Ns],Board,FinalBoard) :-
  stonesinhole(N,Board,Stones),
  distributestones(Stones,N,Board,Board1),
  move(Ns,Board1,FinalBoard).
move([],Board1,Board2) :-
  swap(Board1,Board2).

/*  distribute_stones(Stones,Hole,Board,Board1) :-
	Board1 is the result of distributing the number of stones,
	Stones, from Hole from the current Board.
	It consists of two stages: distributing the stones in the player's
	holes, distribute_my_holes, and distributing the stones in 
	the opponent's holes, distribute_your_holes.
*/

distributestones(Stones,Hole,Board,FinalBoard) :-
  distributemyholes(Stones,Hole,Board,Board1,Stones1),
  distributeyourholes(Stones1,Board1,FinalBoard).

distributemyholes(Stones,N,board(Hs,K,Ys,L),board(Hs1,K1,Ys,L),Stones1) :-
  Stones > 7 - N, !,
  pickupanddistribute(N,Stones,Hs,Hs1),
  K1 is K + 1,
  Stones1 is Stones + N - 7.
distributemyholes(Stones,N,board(Hs,K,Ys,L),Board,0) :-
  pickupanddistribute(N,Stones,Hs,Hs1),
  checkcapture(N,Stones,Hs1,Hs2,Ys,Ys1,Pieces),
  updatekalah(Pieces,N,Stones,K,K1),
  checkiffinished(board(Hs2,K1,Ys1,L),Board).

checkcapture(N,Stones,Hs,Hs1,Ys,Ys1,Pieces) :-
  FinishingHole is N + Stones,
  OppositeHole is 7 - FinishingHole,
  nthmember(OppositeHole,Ys,Y),
  Y > 0, !,
 %% Original
 %%   nsubstitute(OppositeHole,Hs,0,Hs1),
 %%   nsubstitute(FinishingHole,Ys,0,Ys1),
 %% In Shapiro's
  nsubstitute(OppositeHole,Ys,0,Ys1),
  nsubstitute(FinishingHole,Hs,0,Hs1),
  Pieces is Y + 1.
checkcapture(_N,_Stones,Hs,Hs,Ys,Ys,0) :- !.

checkiffinished(board(Hs,K,Ys,L),board(Hs,K,Hs,L1))   :-
 zero(Hs),
 sumlist(Ys,YsSum),
 L1 is L + YsSum.
checkiffinished(board(Hs,K,Ys,L),board(Ys,K1,Ys,L))   :-
 zero(Ys),!,
 sumlist(Hs,HsSum),
 K1 is K + HsSum.
checkiffinished(Board,Board) :- !.

updatekalah(0,Stones,N,K,K) :-
  Stones < 7 - N, !.
updatekalah(0,Stones,N,K,K1) :-
  Stones =:= 7 - N, !,
  K1 is K + 1.
updatekalah(Pieces,_Stones,_N,K,K1) :-
  Pieces > 0, !,
  K1 is K + Pieces.

distributeyourholes(0,Board,Board):- !.
distributeyourholes(Stones,board(Hs,K,Ys,L),board(Hs,K,Ys1,L)) :-
   1 =< Stones,
   Stones =< 6,
   nonzero(Hs), !,
   distribute(Stones,Ys,Ys1).
%% Original
%% distributeyourholes(Stones,board(Hs,K,Ys,L),board(Hs,K,Ys1,L)) :-
distributeyourholes(Stones,board(Hs,K,Ys,L),Board) :-
   Stones > 6, !,
   distribute(6,Ys,Ys1),
   Stones1 is Stones - 6,
   distributestones(Stones1,1,board(Hs,K,Ys1,L),Board).
distributeyourholes(Stones,board(Hs,K,Ys,L),board(Hs,K,Hs,L1)) :-
   zero(Hs), !,
   sumlist(Ys,YsSum),
   L1 is Stones + YsSum + L.

/*  Lower level stone distribution    */

pickupanddistribute(0,N,Hs,Hs1) :-
  !, distribute(N,Hs,Hs1).
pickupanddistribute(1,N,[_H|Hs],[0|Hs1]) :-
   !, distribute(N,Hs,Hs1).
pickupanddistribute(K,N,[_H|Hs],[0|Hs1]) :-
   K > 1,
   K1 is K - 1,
   pickupanddistribute(K1,N,Hs,Hs1).

 %% Original. Aclause is missing.
 %% pickupanddistribute(1,N,[H|Hs],[0|Hs1]) :-
 %%    distribute(N,Hs,Hs1).
 %% pickupanddistribute(K,N,[H|Hs],[0|Hs1]) :-
 %%    K > 1,
 %%    K1 is K - 1,
 %%    pickupanddistribute(K1,N,Hs,Hs1).

distribute(0,Hs,Hs).
distribute(N,[H|Hs],[H1|Hs1]) :-
   N > 0,!,
   N1 is N - 1,
   H1 is H + 1,
   distribute(N1,Hs,Hs1).
distribute(_N,[],[]):- !.

/*   Evaluation function	*/

value(board(_H,K,_Y,L),Value) :-
  Value is K - L.

/*  Testing for the end of the game	*/

gameover(board(0,N,0,N),_Player,draw) :-
  pieces(K),
  N =:= 6 * K, !.
gameover(board(_H,K,_Y,_L),Player,Player) :-
  pieces(N),
  K > 6 * N, !.
gameover(board(_H,_K,_Y,L),Player,Opponent) :-
  pieces(N),
  L > 6 * N,
  nextplayer(Player,Opponent).

announce(opponent).
announce(computer).
announce(draw).

 %% Shapiro's
 %%      announce(opponent) :- writeln(['You won! Congratulations.']).
 %%      announce(computer) :- writeln(['I won.']).
 %%      announce(draw) :- writeln(['The game is a draw.']).

/*  Miscellaneous game utilities	*/

nthmember(N,[_H|Hs],K) :-
  N > 1, !,
  N1 is N - 1,
  nthmember(N1,Hs,K).
nthmember(1,[H|_Hs],H).

nsubstitute(1,[_X|Xs],Y,[Y|Xs]):- !.
nsubstitute(N,[X|Xs],Y,[X|Xs1]) :-
   N > 1, !,
   N1 is N -1,
   nsubstitute(N1,Xs,Y,Xs1).

nextplayer(computer,opponent).
nextplayer(opponent,computer).

%% This is an infinite loop unless the list be ground -PL.
%% This predicate is never reached. 
 %% legal([N|Ns]) :-
 %%   0 < N,
 %%   N < 7,
 %%   legal(Ns).
 %% legal([]).

%% This is an infinite loop -PL.
genlegal([N|Ns]) :-
  ismember(N,[1,2,3,4,5,6]),
  genlegal(Ns).
genlegal([]).

swap(board(Hs,K,Ys,L),board(Ys,L,Hs,K)).

displaygame(Position,computer) :-
  show(Position).
displaygame(Position,opponent) :-
  swap(Position,Position1),
  show(Position1).

show(board(H,K,Y,L)) :-
   reverse(H,Hr),
   writestones(Hr),
   writekalahs(K,L),
   writestones(Y).

writestones(H) :-
  displayholes(H).

 %% Shapiro's
 %% write_stones(H) :- 
 %% 	nl, tab(5), display_holes(H).


displayholes([H|Hs]) :-
   writepile(H),
   displayholes(Hs).
displayholes([]).
 %%  Shapiro's
 %%  display_holes([]) :- nl.

writepile(N) :-
  N < 10,
  writepvh(N).
writepile(N) :-
  N >= 10,
  writepvh(N).

 %% Shapiro's
 %% write_pile(N) :- N < 10, write(N), tab(4).
 %% write_pile(N) :- N >= 10, write(N), tab(3).

%% Commented -PL
writepvh(_X).

writekalahs(K,L) :-
  writepvh(K),
  writepvh(L).

 %% Shapiro's
 %% write_kalahs(K,L) :- 
 %%         write(K), tab(34), write(L), nl.

zero([0,0,0,0,0,0]).

nonzero(Hs) :-
 \+ (Hs = [0,0,0,0,0,0]).


/*  Initializing	*/

lookahead(2).
%% lookahead(5).

%% Shapiro's
initialize(kalah,board([N,N,N,N,N,N],0,[N,N,N,N,N,N],0),opponent) :-
	   pieces(N).

 %% Commented out from original code. -PL
 %% initialize(kalah,board(a,0,a,0),opponent).
 %% initialize(kalah,toto(b,1,b,1),computer).
 %% initialize(kalah,board(c,2,c,2),computer).
 %% initialize(kalah,board(a,0,a,0),computer).
 %% initialize(kalah,board(c,2,c,2),opponent).

pieces(6).
%% pieces(1).

%% The following predicates are not defined in Shapiro's.

ismember(X,[X|_Y]).
ismember(X,[_F|T]) :-
   ismember(X,T).

reverse(L,K):-
   rev(L,[],K).

rev([],L,L).
rev([H|T],L,K):-
   rev(T,[H|L],K).

sumlist(Is,Sum) :-
   sumlist3(Is,0,Sum).
sumlist3([],Sum,Sum).
sumlist3([_I|Is],Temp,Sum) :-
  Temp1 is Temp + 1,
  sumlist3(Is,Temp1,Sum).
