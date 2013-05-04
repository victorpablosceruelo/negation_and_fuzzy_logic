:- load_resource_module(res_viunif(res_viunif_res)).

:- head_cost(ub, viunif, f_viunif).
:- head_cost(lb, viunif, f_viunif).

:- literal_cost(ub, viunif, 0).
:- literal_cost(lb, viunif, 0).

:- trust_default + cost(ub, viunif, 0).
:- trust_default + cost(lb, viunif, 0).
