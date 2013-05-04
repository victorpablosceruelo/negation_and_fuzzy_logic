:- module(illumination, [seq/2,par_nondet/2,data/1],[]).

:- use_package(andprolog_nd).
:- use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

r([[0,1,1,0,0,0,1,0],[0,0,0,0,1,0,1,0],[0,0,0,0,0,0,0,1],[0,0,0,0,1,0,0,0],
   [0,1,0,0,0,1,0,0],[0,0,0,1,1,0,1,0],[0,0,0,1,0,0,0,1],[0,0,1,0,0,1,0,0]]).
d(3).

data([R,D]) :- r(R), d(D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq([R,D],X) :- find_ind(R,D,X).
par_nondet([R,D],X) :- find_par(R,D,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_xxx(R,D,L): searchs a bit belonging to        %%
%% different columns of the board R such as the       %%
%% line difference between bits of consecutive colums %%
%% is greater than D.                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%sequential independent search
m_ind(L) :- 
	get_board(bin,3,8,8,R), 
	display(R), nl, 
	find_ind(R,3,L).

%sequential dependent search
m_dep(L) :- 
	get_board(bin,3,8,8,R), 
	display(R), nl, 
	find_dep(R,3,L).

%parallel backtracking search
m_par(L) :- 
	get_board(bin,3,8,8,R), 
	display(R), nl, 
	%this is what we have to measure
	find_par(R,3,L).


find_dep(Board,D,R) :-
	D2 is D * -1,
	each_colum_det(Board,R,D,D2).

each_colum_det([],[],_,_) :- !.
each_colum_det([C|RC],[Ind|IndR],D,PrevInd) :- 
	get_pos(C,1,PrevInd,D,Ind),
	each_colum_det(RC,IndR,D,Ind).

get_pos([],_,_PrevInd,_D,_Ind) :- fail.
get_pos([N|_RL],Pos,PrevInd,D,Ind) :-
 	pause,
	N == 1,
	Diff is Pos - PrevInd,
	Diff >= D, 
	Ind = Pos.

get_pos([N|_RL],Pos,PrevInd,D,Ind) :-
	N == 1,
	Diff is PrevInd - Pos,
	Diff >= D, 
	Ind = Pos.

get_pos([_|LR],Pos,PrevInd,D,Ind) :-
	Pos1 is Pos + 1,
	get_pos(LR,Pos1,PrevInd,D,Ind).


find_par(Board,D,R) :-
	throw_colum_ndet_par(Board,R,D).
%% check_dist(R,D).

throw_colum_ndet_par([C],[R],_) :- 
	!, get_colum(C,1,R).

throw_colum_ndet_par([C|RC],[R1,R2|RR],D) :- 
	get_colum(C,1,R1) & throw_colum_ndet_par(RC,[R2|RR],D),
	check_value(R1,R2,D).

find_ind(Board,D,R) :-
	throw_colum_ndet(Board,R,D).
%% check_dist(R,D).

throw_colum_ndet([C],[R],_) :- 
	!,get_colum(C,1,R).

throw_colum_ndet([C|RC],[R1,R2|RR],D) :- 
	get_colum(C,1,R1),
	throw_colum_ndet(RC,[R2|RR],D), 
	check_value(R1,R2,D).

get_colum([],_,_) :- !, fail.
get_colum([C|_],Ind,R) :- 
 	pause,
	C == 1,
	Ind = R.
get_colum([_|RC],Ind,R) :- 
	Ind1 is Ind + 1,
	get_colum(RC,Ind1,R).

%% check_dist([_],_) :- !.
%% check_dist([R1,R2|RR],D) :-
%% check_value(R1,R2,D) '&'
%% check_dist([R2|RR],D).

check_value(R1,R2,D) :-
	R1 > R2,
	R1 - R2 >= D.

check_value(R1,R2,D) :-
	R2 - R1 >= D.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Generates in R a board of Type numbers, with probability of (0-1)
%equal to P, C colums and L lines.
get_board(Type,P,C,L,R) :- 
	iterate_col(C,Type,P,L,R).


iterate_col(0,_,_,_,[]) :- !.
iterate_col(C,Type,P,L,[R|Rs]) :-
	C1 is C - 1,
	get_line(L,Type,P,R),
	iterate_col(C1,Type,P,L,Rs).

get_line(0,_,_,[]) :- !.
get_line(L,Type,P,[R|Rs]) :-
	L1 is L - 1,
	get_number(Type,P,R),
	get_line(L1,Type,P,Rs).

get_number(bin,P,R) :-
	random(1,10,N),
	N =< P,
	!, R = 1.

get_number(bin,_P,0).

get_number(dec,_,R) :-
	random(1,10,R).

pause :- fib(12,_).

fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :-
        N > 1,
        N1 is N - 1,
        N2 is N - 2,
        fib(N1, F1),
	fib(N2, F2),
        F is F1 + F2.
