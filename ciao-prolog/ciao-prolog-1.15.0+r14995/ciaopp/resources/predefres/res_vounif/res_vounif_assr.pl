:- load_resource_module(res_vounif(res_vounif_res)).

:- head_cost(ub, vounif, f_vounif).
:- head_cost(lb, vounif, f_vounif).

:- literal_cost(ub, vounif, 0).
:- literal_cost(lb, vounif, 0).

:- trust_default + cost(ub, vounif, 0).
:- trust_default + cost(lb, vounif, 0).
