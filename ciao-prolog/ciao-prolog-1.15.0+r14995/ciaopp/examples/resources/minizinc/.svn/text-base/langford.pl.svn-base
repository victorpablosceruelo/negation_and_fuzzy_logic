%-----------------------------------------------------------------------------%
% Langford's Problem  (CSPlib problem 24)
%
% June 2006; Sebastian Brand
%
% Instance L(k,n):
% Arrange k sets of numbers 1 to n so that each appearance of the number m is m
% numbers on from the last.  For example, the L(3,9) problem is to arrange 3
% sets of the numbers 1 to 9 so that the first two 1's and the second two 1's
% appear one number apart, the first two 2's and the second two 2's appear two
% numbers apart, etc.
%-----------------------------------------------------------------------------%
% MiniZinc version
% Peter Stuckey September 30

/* MZN POSSIBLE DATA FILE

n = 9;
k = 3;

------------------------------------------------------------------
SOLUTION:

L(3, 9):

1s at 1, 3, 5
2s at 7, 10, 13
3s at 19, 23, 27
4s at 16, 21, 26
5s at 8, 14, 20
6s at 4, 11, 18
7s at 9, 17, 25
8s at 6, 15, 24
9s at 2, 12, 22

*/

:- module(_, [main/4], [ciaopp(examples(resources(minizinc)))]).

:- entry main/4 : int * int * var * var.

main(N, K, Pos, Num):-
     % Primal model.
     % Init Pos array.
     Size_Pos is (N * K) - 1 + 1,
     DU_Pos is N * K,
     build_array(Pos, Size_Pos),
     init_pos_i(Size_Pos, 1, DU_Pos, Pos),
     % First constraint.
     Ind1_i is N - 1 + 1, 
     forall1_i(Ind1_i, N, K, Pos),
     % Second constraint.
     all_different_(Pos, Size_Pos),
     % Dual model (partial).
     % Init Num array.
     Size_Num is (N * K) - 1 + 1,
     DU_Num is N * K,
     build_array(Num, Size_Num),
     init_num_i(Size_Num, 1, DU_Num, Num),
     % Second constraint.
     all_different_(Num, Size_Num),
     % Channelling between primal model and dual model.
     % Third constraint.
     Ind2_i is N - 1 + 1, 
     forall2_i(Ind2_i, N, N, K, Num).

/* MZN 

%-----------------------------------------------------------------------------%
% Instance
%-----------------------------------------------------------------------------%

int: n;                            % numbers 1..n
int: k;                            % sets 1..k

%-----------------------------------------------------------------------------%
% Input
%-----------------------------------------------------------------------------%

set of int: numbers = 1..n;             % numbers
set of int: sets    = 1..k;             % sets of numbers
set of int: num_set = 1..n*k;

set of int: positions = 1..n*k;         % positions of (number, set) pairs
*/ 


/*
%-----------------------------------------------------------------------------%
% Primal model
%-----------------------------------------------------------------------------%

array[num_set] of var positions: Pos;   % Pos[ns]: position of (number, set)
                                        % pair in the sought sequence
constraint
        forall(i in 1..n, j in 1..k-1) (
            Pos[k*(i-1) + j+1] - Pos[k*(i-1) + j] = i + 1
        );

constraint
        all_different(Pos);
*/

init_pos_i(0, _DL, _DU, _A) :- !.
init_pos_i(Ind, DL, DU, A):-
    % Ind > 0,
    element_dint(Ind, A, Ai),
    % Ai in DL..DU, 
    in(Ai, DL, DU),
    NInd is Ind - 1,
    init_pos_i(NInd, DL, DU, A).


forall1_i(0, _, _, _) :- !.
forall1_i(Ind_i, Ui, K, Pos):-
        % Ind_i > 0,
        Index_i is Ui - Ind_i + 1, 
        %
        Uj is K - 1,
        Ind_j is Uj - 1 + 1,
        forall1_ij(Ind_j, Uj, Index_i, K, Pos),
        %
	NInd_i is Ind_i - 1,
        forall1_i(NInd_i, Ui, K, Pos).

forall1_ij(0, _, _, _, _) :- !.
forall1_ij(Ind_j, Uj, Index_i, K, Pos) :-
        % Ind_j > 0,
        Index_j is Uj - Ind_j + 1,
        %
        A is K * (Index_i - 1) + Index_j + 1,
        B is K * (Index_i - 1) + Index_j, 
        Ia is A - 1 + 1,
        Ib is B - 1 + 1,
        element(Ia, Pos, Pos_a), 
        element(Ib, Pos, Pos_b),
        minus_dint_dint_var(Pos_a, Pos_b, D),
        C is Index_i + 1,
        eq(D, C),
        %
        NInd_j is Ind_j - 1, 
        forall1_ij(NInd_j, Uj, Index_i, K, Pos).

/*
%-----------------------------------------------------------------------------%
% Dual model (partial)
%-----------------------------------------------------------------------------%

array[positions] of var num_set: Num;   % Num[p]: (number, set) pair at
                                        % position p in the sought sequence
constraint
        all_different(Num);

*/

init_num_i(0, _DL, _DU, _A) :- !.
init_num_i(Ind, DL, DU, A):-
    % Ind > 0,
    element_dint(Ind, A, Ai),
    % Ai in DL..DU, 
    in(Ai, DL, DU),
    NInd is Ind - 1,
    init_num_i(NInd, DL, DU, A).

/*
%-----------------------------------------------------------------------------%
% Channelling between primal model and dual model
%-----------------------------------------------------------------------------%

constraint
        forall(i in numbers, j in sets, p in positions) (
                (Pos[k*(i-1) + j] = p) <-> (Num[p] = k*(i-1) + j)
        );

%-----------------------------------------------------------------------------%
*/

forall2_i(0, _, _, _, _) :- !.
forall2_i(Ind_i, Ui, N, K, Num):-
        % Ind_i > 0,
        Index_i is Ui - Ind_i + 1, 
        %
        Ind_j is K - 1 + 1,
        forall2_ij(Ind_j, K, Index_i, N, K, Num),
        %
	NInd_i is Ind_i - 1,
        forall2_i(NInd_i, Ui, N, K, Num).

forall2_ij(0, _, _, _, _, _) :- !.
forall2_ij(Ind_j, Uj, Index_i, N, K, Num) :-
        % Ind_j > 0,
        Index_j is Uj - Ind_j + 1,
        %
        Up is N * K,
        Ind_p is Up - 1 + 1,
        forall2_ijp(Ind_p, Up, Index_i, Index_j, K, Num),
        %
        NInd_j is Ind_j - 1, 
        forall2_ij(NInd_j, Uj, Index_i, N, K, Num).

forall2_ijp(0, _, _, _, _, _) :- !.
forall2_ijp(Ind_p, Up, Index_i, Index_j, K, Num) :-
        % Ind_p > 0,
        Index_p is Up - Ind_p + 1,
        % Pos[k*(i-1) + j] = p
        A is K * (Index_i - 1) + Index_j, 
        Ia is A - 1 + 1,
        element(Ia, Num, Num_a), 
        reif_eq_dint_int_var(Num_a, Index_p, B1),
        % Num[p] = k*(i-1) + j
        Ip is Index_p - 1 + 1, 
        element(Ip, Num, Num_p),
        reif_eq_dint_int_var(Num_p, A, B2),
        equiv(B1, B2, 1),
        %
        NInd_p is Ind_p - 1, 
        forall2_ijp(NInd_p, Up, Index_i, Index_j, K, Num).

/*
	% Without specifying a sensible search order this problem takes
	% forever to solve.
	%
solve	:: int_search(Pos, "first_fail", "indomain_split", "complete")
	satisfy;

output  [ "L(", show(k), ", ", show(n), "):\n"] ++
	[ if j = 1 then "\n" ++ show(i) ++ "s at " else ", " endif ++
	  show(Pos[k*(i-1) + j])
	| i in 1..n, j in 1..k
	];

*/
