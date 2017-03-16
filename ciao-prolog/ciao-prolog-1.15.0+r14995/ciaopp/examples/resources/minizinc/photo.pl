%-----------------------------------------------------------------------------
% Placing people on a photo
%
% A group of people wants to take a group photo. Each person can give
% preferences next to whom he or she wants to be placed on the
% photo. The problem to be solved is to find a placement that
% satisfies as many preferences as possible.


/* MZN POSSIBLE DATA FILE

n_names = 9;
n_prefs = 17;
prefs = array2d(1..n_prefs, 0..1,
        [| 0,2 | 0,4 | 0,7 | 1,4 | 1,8 | 2,3 | 2,4 | 3,0 | 3,4 |
           4,5 | 4,0 | 5,0 | 5,8 | 6,2 | 6,7 | 7,8 | 7,6 |]);

------------------------------------------------------------------
SOLUTION:

photo: [0, 1, 4, 3, 2, 8, 5, 6, 7](score 8)

main(9, 17,
	a(array(
		 a(array(0,2)),a(array(0,4)),a(array(0,7)),a(array(1,4)),a(array(1,8)),
		 a(array(2,3)),a(array(2,4)),a(array(3,0)),a(array(3,4)),a(array(4,5)),
		 a(array(4,0)),a(array(5,0)),a(array(5,8)),a(array(6,2)),a(array(6,7)),
		 a(array(7,8)),a(array(7,6))
	     )),
	     Ful,Pos,Sat).

*/

:- module(_, [main/6], [ciaopp(examples(resources(minizinc)))]).

:- entry main/6 : int * int * array * var * var * var.

main(N_names, N_prefs, Prefs, Ful, Pos, Sat):-
     U_Pos is N_names - 1,
     Size_Pos is U_Pos - 0 + 1,
     DU_Pos is N_names - 1,
     build_array(Pos, Size_Pos),
     init_pos_i(Size_Pos, 0, DU_Pos, Pos),
     Size_Ful is N_prefs - 1 + 1, 
     build_array(Ful, Size_Ful),
     init_ful_i(Size_Ful, 0, 1, Ful),
     % First constraint.
     Ind_i is N_prefs - 1 + 1, 
     forall_i(Ind_i, N_prefs, N_names, N_prefs, Prefs, Pos, Ful),
     % Second constraint
     Ind_sum is N_prefs - 1 + 1,
     sum_i(Ind_sum, N_prefs, Ful, Sum),
     eq_int(Sum, Sat),
     % Third constraint
     all_different_(Pos, Size_Pos),
     % Fourth constraint
     I0 is 0 - 1 + 1,
     element(I0, Pos, Pos_0),
     I1 is 1 - 1 + 1,
     element(I1, Pos, Pos_1),
     less(Pos_0, Pos_1).

sum_i(0, _, _, 0) :- !.
sum_i(Ind, U, Ful, Sum):- 
      % Ind > 0,
      Index_i is U - Ind + 1, 
      %
      I is Index_i - 1 + 1,
      element_int(I, Ful, Ful_i),
      NInd is Ind - 1,
      sum_i(NInd, U, Ful, NSum),
      plus_int(Ful_i, NSum, Sum).

/*

% Specification

int: n_names;
int: n_prefs;
array[1..n_prefs, 0..1] of int: prefs;

*/


/*
% Model

array[0..n_names-1] of var 0..n_names-1: pos;
var 0..n_names-1: sat;

array[1..n_prefs] of var bool: ful;
*/

init_pos_i(0, _DL, _DU, _A) :- !.
init_pos_i(Ind_i, DL, DU, A):-
    % Ind_i > 0,
    element_dint(Ind_i, A, Ai),
    % Ai in DL..DU, 
    in(Ai, DL, DU),
    NInd_i is Ind_i - 1,
    init_pos_i(NInd_i, DL, DU, A).

init_ful_i(0, _DL, _DU, _A) :- !.
init_ful_i(Ind, DL, DU, A):-
    % Ind > 0,
    element_dint(Ind, A, Ai),
    % Ai in DL..DU, 
    in(Ai, DL, DU),
    NInd is Ind - 1,
    init_ful_i(NInd, DL, DU, A).

/*
constraint
  forall (i in 1..n_prefs) (
    let {
      int: pa = prefs[i,0],
      int: pb = prefs[i,1]
    } in
    ful[i] = (pos[pb]-pos[pa] == 1 xor pos[pa]-pos[pb] == 1)
  );
*/


forall_i(0, _, _, _, _, _, _) :- !.
forall_i(Ind_i, Ui, N_names, N_prefs, Prefs, Pos, Ful):-
        % Ind_i > 0,
        Index_i is Ui - Ind_i + 1, 
        %
        I is Index_i - 1 + 1,
        J0 is 0 - 0 + 1,
        element2d_int(I, J0, Prefs, Prefs_i_0), 
        J1 is 1 - 0 + 1,
        element2d_int(I, J1, Prefs, Prefs_i_1), 
        type_decl_int(Pa),
        Pa = Prefs_i_0,
        type_decl_int(Pb),
        Pb = Prefs_i_1,
        element(Pa, Pos, Pos_a),
        element(Pb, Pos, Pos_b),
        minus_dint_dint_var(Pos_b, Pos_a, Dab),
        reif_eq_dint_int_var(Dab, 1, Rab),
        minus_dint_dint_var(Pos_a, Pos_b, Dba),
        reif_eq_dint_int_var(Dba, 1, Rba),
        xor_dint_dint_var(Rab, Rba, Xor),
        element_dint(I, Ful, Ful_i),
        eq_dint(Ful_i, Xor),
        %
	NInd_i is Ind_i - 1,
        forall_i(NInd_i, Ui, N_names, N_prefs, Prefs, Pos, Ful).

/*
constraint
  sum (i in 1..n_prefs) (bool2int(ful[i])) = sat;

constraint
  all_different(pos);

% Break some symmetry
constraint
  pos[0] < pos[1];

/*
solve :: int_search(pos, "first_fail", "indomain", "complete") 
    maximize sat;

output [
  "photo: ",
  show(pos),
  "(score ", show(sat), ")\n"
];
*/
