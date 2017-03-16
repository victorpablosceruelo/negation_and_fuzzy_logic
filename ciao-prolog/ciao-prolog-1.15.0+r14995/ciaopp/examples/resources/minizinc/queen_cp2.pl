:- module(_, [main/2], [ciaopp(examples(resources(minizinc)))]).

:- entry main/2 : int * var.
 
/* MZN
array [1..n] of var 1..n: q;

constraint
    forall (i in 1..n, j in i+1..n) (
        noattack(i, j, q[i], q[j])
    );
*/

main(N, Q) :- 
     Size is N-1+1, 
     build_array(Q,Size), 
     init_q(Size,N,Q),
     Index1 is N-1+1,  
     forall_i(Index1,N,Q).

init_q(0,_DU,_A) :- !.
init_q(Ind,DU,A) :-
    % Ind > 0,
    element_dint(Ind,A,Ai),
    % Ai in DL..DU, where DL is known to be constant 1
    in(Ai,1,DU),
    NInd is Ind - 1,
    init_q(NInd,DU,A).

forall_i(0, _, _) :- !.
forall_i(Ind, Uij, A) :-
        % Ind > 0,
        Index_i is Uij - Ind + 1, 
        %
        Ind_j is Uij - (Index_i + 1) + 1,
        forall_ij(Ind_j, Uij, Index_i, A),
        %
	NInd is Ind-1,
        forall_i(NInd, Uij, A).

forall_ij(0, _, _, _) :- !.
forall_ij(Ind_j, Uij, Index_i, A) :-
        % Ind_j > 0,
        Index_j is Uij - Ind_j + 1,
        I is Index_i - 1 + 1,
        % arg(I,A,A_i), 
        element(I,A,A_i), 
        J is Index_j - 1 + 1,
        % arg(J,A,A_j), 
        element(J,A,A_j),  
	% display(I:J),nl,
	no_attack(Index_i, Index_j, A_i, A_j),
        NInd_j is Ind_j - 1, 
        forall_ij(NInd_j, Uij, Index_i, A).

/* MZN
predicate 
    noattack(int: i, int: j, var int: qi, var int: qj) =
        qi     != qj     /\
        qi + i != qj + j /\
        qi - i != qj - j
*/

no_attack(I, J, QI, QJ) :-
        neq(QI, QJ),
        plus_dint_int_var(QI,I,A),
        plus_dint_int_var(QJ,J,B),
        neq(A, B),
        minus_dint_int_var(QI,I,C),
        minus_dint_int_var(QJ,J,D),
        neq(C, D).
