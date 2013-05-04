:- load_resource_module(res_giunif(res_giunif_res)).

:- head_cost(ub, giunif, f_giunif).
:- head_cost(lb, giunif, f_giunif).

:- literal_cost(ub, giunif, f_giunif).
:- literal_cost(lb, giunif, f_giunif).

:- trust_default + cost(ub, giunif, 0).
:- trust_default + cost(lb, giunif, 0).
