:- module(_, [main/4], [ciaopp(examples(resources(minizinc)))]).

:- entry main/4 : int * array * array * var.

main(N, Profits, X, Sum) :-
   sum_i(N, Profits, X, Sum).

sum_i(0, _, _, 0).
sum_i(J, Profits, X, Sum):- 
      J > 0,
      element_int(J, Profits, Pj),
      element_dint(J, X, Xj),
      leq(Xj, Pj),
      % mult_int(Pj, Xj, _P),
      mult_int_dint_var(Pj, Xj, _P),
      NJ is J - 1,
      sum_i(NJ, Profits, X, NSum),
      Sum is NSum + 1.
