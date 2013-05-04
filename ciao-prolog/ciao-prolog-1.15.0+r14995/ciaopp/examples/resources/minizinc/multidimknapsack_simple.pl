%------------------------------------------------------------------------------%
% The classical 0/1 multidimensional knapsack problem.
% There is a knapsack with m different Capacity constraints.
% There are n items with Profits and Weights.
% The Goals is to maximise the total Profit of the Items in the 
% Knapsack while  not violating any of the Capacity constraints.

/* MZN POSSIBLE DATA FILE

n = 5;
m = 3;

Profits    = [5,6,3,7,4];

Capacities = [5,10,15];

Weights    = [| 2, 3, 2 
              | 1, 4, 4 
              | 1, 2, 5 
              | 3, 2, 3 
              | 1, 3, 5 |];

--------------------------

SOLUTION:

Multidimensional Knapsack Problem:
Total Profit: 17
Chosen Items: 1:0 2:1 3:0 4:1 5:1 

*/

/* CIAO

main(5,3,array(5,6,3,7,4),array(5,10,15),array(array(2, 3, 2),
	                                       array(1, 4, 4),
					       array(1, 2, 5),
					       array(3, 2, 3),
					       array(1, 3, 5)), X, Sum).
*/

:- module(_, [main/7], [ciaopp(examples(resources(minizinc)))]).

:- entry main/7 : int * int * array * array * array * var * var.

/* MZN 

int: n;
int: m;

array[1..n] of int: Profits;
array[1..n,1..m] of int: Weights;
array[1..m] of int: Capacities;

array[1..n] of var 0..1: x;

*/

main(N, M, Profits, Capacities, Weights, X, Sum) :-
     % array[1..n] of var 0..1: x;
     Size is N - 1 + 1,
     build_array(X, Size),
     init_i(Size, 0, 1, X),
     % constraint
     Ind_i is M - 1 + 1,  
     forall_i(Ind_i, M, N, M, Profits, Capacities, Weights, X),
     % function to maximize
     sum_i(N, N, Profits, X, Sum).

init_i(0, _DL, _DU, _A) :- !.
init_i(Ind, DL, DU, A):-
    % Ind > 0,
    element_dint(Ind, A, Ai),
    % Ai in DL..DU, 
    in(Ai, DL, DU),
    NInd is Ind - 1,
    init_i(NInd, DL, DU, A).

/* MZN 

constraint
    forall(i in 1..m) (
        sum([Weights[j,i] * x[j] | j in 1..n])  <=  Capacities[i]
    );

*/

forall_i(0, _, _, _, _, _, _, _) :- !.
forall_i(Ind_i, Ui, N, M, Profits, Capacities, Weights, X):-
        % Ind_i > 0,
        Index_i is Ui - Ind_i + 1, 
        %
        Ind_j is N - 1 + 1,
        sum_j(Ind_j, N, Index_i, Weights, X, Sum),
        I is Index_i - 1 + 1,
        element_int(I, Capacities, Ci), 
        leq_dint_int(Sum, Ci),
        %
	NInd_i is Ind_i - 1,
        forall_i(NInd_i, Ui, N, M, Profits, Capacities, Weights, X).

sum_j(0, _, _, _, _, '..'(0,0)) :- !.
sum_j(Ind, U, Index_i, Weights, X, Sum):- 
      % Ind > 0,
      Index_j is U - Ind + 1, 
      %
      J is Index_j - 1 + 1,
      I is Index_i - 1 + 1,
      element2d_int(J, I, Weights, Wji),
      element_int(J, X, Xj),
      mult_int(Wji, Xj, P),
      NInd is Ind - 1,
      sum_j(NInd, U, Index_i, Weights, X, NSum),
      plus_int_dint_var(P, NSum, Sum).

/* MZN

solve maximize
    sum([x[j] * Profits[j] | j in 1..n]);

*/

sum_i(0, _, _, _, 0) :- !.
sum_i(Ind, U, Profits, X, Sum):- 
      % Ind > 0,
      Index is U - Ind + 1, 
      %
      J is Index - 1 + 1,
      element_int(J, Profits, Pj),
      element_int(J, X, Xj),
      mult_int(Pj, Xj, P),
      NInd is Ind - 1,
      sum_i(NInd, U, Profits, X, NSum),
      plus_int(P, NSum, Sum).
