:- module(_, [main/5], [ciaopp(examples(resources(minizinc)))]).

/* MZN 
INPUT DATA 
size = 2;
d = [| 2,5,
     | 3,4 |];

SOLUTION
s[1..2, 1..2] = 
  [ 0 2
    2 7 ]
----------------
INPUT DATA 
size = 4;
d = [| 2,5,1,3
     | 3,4,2,1
     | 4,3,5,6
     | 1,6,2,1 |];

SOLUTION
s[1..4, 1..4] = 
  [ 0 2 7 8
    6 10 15 21
    2 7 10 15
    9 14 20 22 ]
*/

/* CIAO
INPUT DATA 
main(2, End, 14, a(array(a(array(2,5)),a(array(3,4)))), S).
main(4, End, 49, a(array(a(array(2,5,1,3)),
	                a(array(3,4,2,1)),
		        a(array(4,3,5,6)),
		        a(array(1,6,2,1)))), S).
*/

/* MZN

int: size;                                  % size of problem
array [1..size,1..size] of int: d;          % task durations
int: total = sum(i,j in 1..size) (d[i,j]);  % total duration
array [1..size,1..size] of var 0..total: s; % start times
var 0..total: end;                          % total end time

*/

:- entry main/5 : int * dint * int * array * var.

main(Size, End, Total, D, S):- 
     % S is the array of start times.
     % array [1..size,1..size] of var 0..total: s;
     Ss is Size - 1 + 1,
     build_array(S, Ss),
     init_s_i(Ss, Total, Size, S),
     % constraint
     Ind_i is Size - 1 + 1,
     forall1_i(Ind_i, Size, Size, S, D, End).

init_s_i(0, _DU, _Uj, _S):-!.
init_s_i(Ind_i, DU, Uj, S):-
%    Ind_i > 0,
    element_array(Ind_i, S, Si),
    Siz_j is Uj - 1 + 1,
    build_array(Si, Siz_j),
    init_s_ij(Siz_j, DU, Si),
    NInd_i is Ind_i - 1,
    init_s_i(NInd_i, DU, Uj, S).

init_s_ij(0, _DU, _Si):-!.
init_s_ij(Ind_j, DU, Si):-
%    Ind_j > 0,
    element_dint(Ind_j, Si, Sij),
    % Sij in DL..DU, where DL is known to be constant 0
    in(Sij, 0, DU),
    NInd_j is Ind_j - 1,
    init_s_ij(NInd_j, DU, Si).

/* MZN
predicate no_overlap(var int:s1, int:d1, var int:s2, int:d2) =
    s1 + d1 <= s2 \/ s2 + d2 <= s1;

*/

no_overlap(S1, D1, S2, D2) :-
        plus_dint_int_var(S1,D1,A),
        reif_leq_dint_dint_var(A, S2, B1),
	plus_dint_int_var(S2, D2, B),
	reif_leq_dint_dint_var(B, S1, B2),
	or_dint_dint_int(B1, B2, 1).

/* MZN
constraint
    forall(i in 1..size) (
        forall(j in 1..size-1) (s[i,j] + d[i,j] <= s[i,j+1]) /\
        s[i,size] + d[i,size] <= end /\
        forall(j,k in 1..size where j < k) (
            no_overlap(s[j,i], d[j,i], s[k,i], d[k,i])
        )
    );

solve minimize end;

*/

:- entry forall1_i/6 : int * int * int * array * array * dint.

:- trust pred forall1_i(Ind_i,_1,_2,_3,_4,_5)
         : ( int(Ind_i), int(_1), int(_2), term(_3), array(_4), dint(_5) )
        => ( int(Ind_i), int(_1), int(_2), term(_3), array(_4), dint(_5) ).

forall1_i(0, _, _, _, _, _):-!.
forall1_i(Ind_i, Ui, Size, S, D, End) :-
        % Ind_i > 0,
        Index_i is Ui - Ind_i + 1, 
        %
        Uj is Size - 1,
        Ind_j is Uj - 1 + 1,
        % First conjunct
        forall1_j(Ind_j, Uj, Index_i, Size, S, D, End),
        % Second conjunct
        I is Index_i - 1 + 1,
        element2d_dint(I, Size, S, S_i_size), 
        element2d_int(I, Size, D, D_i_size), 
        plus_dint_int_var(S_i_size, D_i_size, B),
        leq(B, End), 
        % Third conjunct
        Ind2_j is Size - 1 + 1, 
        forall2_j(Ind2_j, Size, Index_i, Size, S, D),
        % Uncomment this line to find the exact bounds (EMM):
	% forall2_j_(Size, Index_i, S, D),
        %
	NInd_i is Ind_i - 1,
        forall1_i(NInd_i, Ui, Size, S, D, End).

% % Uncomment this trusts to find the exact bounds (EMM):
% :- trust pred forall2_j_(Size,Index_i,S,D)
%          : ( int(Size), int(Index_i), array(S), array(D) )
%         => ( int(Size), int(Index_i), array(S), array(D), size(lb,Size,int(Size)), size(lb,Index_i,int(Index_i)), size(lb,S,size(S)), size(lb,D,size(D)) )
%          + ( cost(lb,alldiff,0), cost(lb,constraints,5*(int(Size)*(int(Size)-1)/2)), cost(lb,numvars,4*(int(Size)*(int(Size)-1)/2)) ).

% :- trust pred forall2_j_(Size,Index_i,S,D)
%          : ( int(Size), int(Index_i), array(S), array(D) )
%         => ( int(Size), int(Index_i), array(S), array(D), size(ub,Size,int(Size)), size(ub,Index_i,int(Index_i)), size(ub,S,size(S)), size(ub,D,size(D)) )
%          + ( cost(ub,alldiff,0), cost(ub,constraints,5*(int(Size)*(int(Size)-1)/2)), cost(ub,numvars,4*(int(Size)*(int(Size)-1)/2)) ).

% forall2_j_(Size, Index_i, S, D) :-
%         Ind2_j is Size - 1 + 1, 
% 	forall2_j(Ind2_j, Size, Index_i, Size, S, D).

forall1_j(0, _, _, _, _, _, _) :- !.
forall1_j(Ind_j, Uj, Index_i, Size, S, D, End) :-
%        Ind_j > 0,
        Index_j is Uj - Ind_j + 1,
        %
        I is Index_i - 1 + 1,
        J is Index_j - 1 + 1,
        element2d_dint(I, J, S, Sij),
        element2d_int(I, J, D, Dij),
        J1 is J + 1,
        element2d_dint(I, J1, S, Sij1),
        plus_dint_int_var(Sij, Dij, A),
        leq(A, Sij1),
        %
        NInd_j is Ind_j - 1,
        forall1_j(NInd_j, Uj, Index_i, Size, S, D, End).

forall2_j(0, _, _, _, _, _) :- !.
forall2_j(Ind_j, Uj, Index_i, Size, S, D) :-
%       Ind_j > 0,
        Index_j is Uj - Ind_j + 1,
        %
        Ind_k is Size - 1 + 1,
        forall2_jk(Ind_k, Size, Index_i, Index_j, S, D),
        %
	NInd_j is Ind_j - 1,
        forall2_j(NInd_j, Uj, Index_i, Size, S, D).

forall2_jk(0, _, _, _, _, _) :- !.
forall2_jk(Ind_k, Uk, Index_i, Index_j, S, D):-
%       Ind_k > 0,
        Index_k is Uk - Ind_k + 1,
        %
        where_forall2_jk(Index_i, Index_j, Index_k, S, D),
        %
	NInd_k is Ind_k - 1, 
        forall2_jk(NInd_k, Uk, Index_i, Index_j, S, D).

where_forall2_jk(Index_i, Index_j, Index_k, S, D) :- 
        Index_k < Index_j,
        !,
        element2d_dint(Index_j, Index_i, S, Sji), 
        element2d_int(Index_j, Index_i, D,  Dji), 
        element2d_dint(Index_k, Index_i, S,  Ski), 
        element2d_int(Index_k, Index_i, D,  Dki), 
        no_overlap(Sji, Dji, Ski, Dki).
where_forall2_jk(_Index_i, _Index_j, _Index_k, _S, _D).        
