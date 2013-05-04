:- module(_, [main/2], [ciaopp(examples(resources(minizinc)))]).
%:- module(_, [main/2,problem/4], [ciaopp(examples(resources(minizinc)))]).

:- entry main/2 : int * var.

/* MZN
int: S;
int: N = S * S;

set of int: PuzzleRange = 1..N;
set of int: SubSquareRange = 1..S;

array[1..N,1..N] of var PuzzleRange: puzzle;
*/

/* CIAO data
problem(a,3,
array(array(A1,B1,C1,D1,E1,F1,G1,H1,I1),
      array(A2,B2,C2,D2,E2,F2,G2,H2,I2),
      array(A3,B3,C3,D3,E3,F3,G3,H3,I3),
      array(A4,B4,C4,D4,E4,F4,G4,H4,I4),
      array(A5,B5,C5,D5,E5,F5,G5,H5,I5),
      array(A6,B6,C6,D6,E6,F6,G6,H6,I6),
      array(A7,B7,C7,D7,E7,F7,G7,H7,I7),
      array(A8,B8,C8,D8,E8,F8,G8,H8,I8),
      array(A9,B9,C9,D9,E9,F9,G9,H9,I9) ),

(     B1=8, D1=3, F1=9, G1=6,
      E2=2, G2=9, H2=7,
      B3=7, F3=8, G3=3, H3=2, I3=4,
      A4=6, C4=9, H4=8,
      D5=8, G5=5, I5=2,
      C6=8, D6=6, F6=4, G6=7, I6=9,
      E7=3,
      A8=7, B8=9, F8=6,
      A9=3, E9=8, F9=7, H9=4
)).
solution(a,
 a(array(a(array(2,8,4,3,7,9,6,1,5)),
       a(array(1,6,3,4,2,5,9,7,8)),
       a(array(9,7,5,1,6,8,3,2,4)),
       a(array(6,3,9,7,5,2,4,8,1)),
       a(array(4,1,7,8,9,3,5,6,2)),
       a(array(5,2,8,6,1,4,7,3,9)),
       a(array(8,4,6,5,3,1,2,9,7)),
       a(array(7,9,1,2,4,6,8,5,3)),
       a(array(3,5,2,9,8,7,1,4,6)))) )
).
*/


main(S, P):- 
     N is S*S,
     % array[1..N,1..N] of var PuzzleRange: puzzle;
     Size is N-1 +1,
     build_array(P,Size),
     init_i(Size,N,N,P),
     %first constraint
     Index1 is N-1+1,  
     forall1_i(Index1,N,P),
     %second constraint
     Index2 is S-1+1,  
     forall2_a(Index2,S,P).

init_i(0,_DU,_Uj,_A) :- !.
init_i(Ind,DU,Uj,A):-
    % Ind > 0,
    element_array(Ind,A,Ai),
    Size is Uj-1+1,
    build_array(Ai,Size),
    init_ij(Size,DU,Ai),
    NInd is Ind - 1,
    init_i(NInd,DU,Uj,A).

init_ij(0,_DU,_A) :- !.
init_ij(Ind,DU, A):-
    % Ind > 0,
    element_dint(Ind,A,Ai),
    % Ai in DL..DU, where DL is known to be constant 1
    in(Ai,1,DU),
    NInd is Ind - 1,
    init_ij(NInd,DU,A).

% All different in rows and all different in columns.
%
% constraint 
% 	forall (i, j in PuzzleRange, k in 1..(j - 1)) (
% 		puzzle[i,j] != puzzle[i,k]
% 	/\	puzzle[j,i] != puzzle[k,i]
% 	);


forall1_i(0, _, _ ) :- !.
forall1_i(Ind,U,P) :-
        % Ind > 0,
        Index_i is U - Ind + 1, 
        %
        Ind_j is U - 1 + 1,
        forall1_ij(Ind_j, U, Index_i, P),
        %
	NInd is Ind-1,
        forall1_i(NInd, U, P).

forall1_ij(0, _, _, _) :- !.
forall1_ij(Ind, U, Index_i, P) :-
        % Ind > 0,
        Index_j is U - Ind + 1,
        %
        Uk is Index_j - 1,
        Ind_k is Uk - 1 + 1, 
        forall1_ijk(Ind_k,Uk,Index_i, Index_j,P),
        %
        NInd is Ind - 1, 
        forall1_ij(NInd, U, Index_i, P).

forall1_ijk(0, _,_, _, _) :- !.
forall1_ijk(Ind, Uk, Index_i, Index_j, P) :-
        % Ind > 0,
        Index_k is Uk - Ind + 1,
        I is Index_i - 1 + 1,
        J is Index_j - 1 + 1,
        K is Index_k - 1 + 1,
        % puzzle[i,j] != puzzle[i,k]
        element_array(I,P,Pi), 
        element_dint(J,Pi,Pij), 
        element_dint(K,Pi,Pik),  
	neq(Pij,Pik),
	% puzzle[j,i] != puzzle[k,i]
        element_array(J,P,Pj), 
        element_dint(I,Pj,Pji), 
        element_array(K,P,Pk), 
        element_dint(I,Pk,Pki), 
	neq(Pji,Pki),
        NInd is Ind - 1, 
        forall1_ijk(NInd, Uk, Index_i, Index_j, P).

/*
% All different in sub-squares:
% for the set of global coordinates (abscissa,ordinate) of the sub-squares 
% take a set of local coordinates of each sub-square,
% and for each coordinate, given a second set of local sub-square coordinates,
% if the coordinates differ, the value in the puzzle is different
%
constraint
	forall (
		a, o, a1, o1, a2, o2 in SubSquareRange
		where	a1 != a2
		/\	o1 != o2
	) (
		puzzle[(a - 1) * S + a1, (o - 1) * S + o1] !=
			puzzle[(a - 1) * S + a2, (o - 1) * S + o2]
	);

*/

forall2_a(0, _, _) :- !.
forall2_a(Ind,S,P) :-
        % Ind > 0,
        Index_a is S - Ind + 1, 
        %
        Ind_o is S - 1 + 1,
        forall2_ao(Ind_o, S, Index_a, P),
        %
	NInd is Ind-1,
        forall2_a(NInd, S, P).

forall2_ao(0, _, _, _) :- !.
forall2_ao(Ind, S, Index_a, P) :-
        % Ind > 0,
        Index_o is S - Ind + 1,
        %
        Ind_a1 is S -1 + 1,
        forall2_aoa1(Ind_a1, S, Index_a, Index_o, P),
        %
        NInd is Ind - 1, 
        forall2_ao(NInd, S, Index_a, P).

forall2_aoa1(0, _, _, _, _) :- !.
forall2_aoa1(Ind, S, Index_a, Index_o, P) :-
        % Ind > 0,
        Index_a1 is S - Ind + 1,
        %
        Ind_o1 is S -1 + 1,
        forall2_aoa1o1(Ind_o1, S, Index_a, Index_o, Index_a1, P),
        %
        NInd is Ind - 1, 
        forall2_aoa1(NInd, S, Index_a, Index_o, P).

forall2_aoa1o1(0, _, _, _, _, _) :- !.
forall2_aoa1o1(Ind, S, Index_a, Index_o, Index_a1, P) :-
        % Ind > 0,
        Index_o1 is S - Ind + 1,
        %
        Ind_a2 is S -1 + 1,
        forall2_aoa1o1a2(Ind_a2, S, Index_a, Index_o, Index_a1, Index_o1, P),
        %
        NInd is Ind - 1, 
        forall2_aoa1o1(NInd, S, Index_a, Index_o, Index_a1, P).


% % Uncomment this trusts to find the exact bounds (EMM):
% :- trust pred forall2_aoa1o1a2(Ind,_1,_2,_3,_4,_5,_6)
%          : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6) )
%         => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,int(_4)), size(lb,_5,int(_5)), size(lb,_6,size(_6)) )
%          + ( cost(lb,alldiff,0), cost(lb,constraints,(int(_1) - 1)*(int(Ind)-1)), cost(lb,numvars,0) ).

% :- trust pred forall2_aoa1o1a2(Ind,_1,_2,_3,_4,_5,_6)
%          : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6) )
%         => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), array(_6), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,int(_4)), size(ub,_5,int(_5)), size(ub,_6,size(_6)) )
%          + ( cost(ub,alldiff,0), cost(ub,constraints,(int(_1) - 1)*(int(Ind)-1)), cost(ub,numvars,0) ).

% :- trust pred forall2_aoa1o1a2o2(Ind,_1,_2,_3,_4,_5,_6,_7)
%          : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7) )
%         => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7), size(lb,Ind,int(Ind)), size(lb,_1,int(_1)), size(lb,_2,int(_2)), size(lb,_3,int(_3)), size(lb,_4,int(_4)), size(lb,_5,int(_5)), size(lb,_6,int(_6)), size(lb,_7,size(_7)) )
%          + ( cost(lb,alldiff,0), cost(lb,constraints,int(Ind)-1), cost(lb,numvars,0) ).

% :- trust pred forall2_aoa1o1a2o2(Ind,_1,_2,_3,_4,_5,_6,_7)
%          : ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7) )
%         => ( int(Ind), int(_1), int(_2), int(_3), int(_4), int(_5), int(_6), array(_7), size(ub,Ind,int(Ind)), size(ub,_1,int(_1)), size(ub,_2,int(_2)), size(ub,_3,int(_3)), size(ub,_4,int(_4)), size(ub,_5,int(_5)), size(ub,_6,int(_6)), size(ub,_7,size(_7)) )
%          + ( cost(ub,alldiff,0), cost(ub,constraints,int(Ind)-1), cost(ub,numvars,0) ).

forall2_aoa1o1a2(0, _, _, _,  _, _, _) :- !.
forall2_aoa1o1a2(Ind, S, Index_a, Index_o, Index_a1, Index_o1, P) :-
        % Ind > 0,
        Index_a2 is S - Ind + 1,
        %
	( Index_a2 =\= Index_a1 ->
        Ind_o2 is S -1 + 1,
        forall2_aoa1o1a2o2(Ind_o2, S, Index_a, Index_o, Index_a1, Index_o1, Index_a2, P)
	; true ),
        %
        NInd is Ind - 1, 
        forall2_aoa1o1a2(NInd, S, Index_a, Index_o, Index_a1, Index_o1, P).

forall2_aoa1o1a2o2(0, _, _, _, _,  _, _, _) :- !.
forall2_aoa1o1a2o2(Ind, S, Index_a, Index_o, Index_a1, Index_o1, Index_a2, P) :-
        % Ind > 0,
        Index_o2 is S - Ind + 1,
	( Index_o2 =\= Index_o1 ->
	%puzzle[(a - 1) * S + a1, (o - 1) * S + o1] !=
	%	puzzle[(a - 1) * S + a2, (o - 1) * S + o2]
	Indexi1 is (Index_a-1)*S + Index_a1,
	Indexj1 is (Index_o-1)*S + Index_o1,
	Indexi2 is (Index_a-1)*S + Index_a2,
	Indexj2 is (Index_o-1)*S + Index_o2,
	I1 is Indexi1-1+1,
	J1 is Indexj1-1+1,
	I2 is Indexi2-1+1,
	J2 is Indexj2-1+1,
        element2d_dint(I1,J1,P,P1ij),
        element2d_dint(I2,J2,P,P2ij),
	neq_dint_dint(P1ij,P2ij)
	; true ),
        %
        NInd is Ind - 1, 
        forall2_aoa1o1a2o2(NInd, S, Index_a, Index_o, Index_a1, Index_o1, Index_a2, P).

