:- module(cost_approx, [approximation/1], []).

:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).

approximation(X):- current_pp_flag(cost_approximation, X).

