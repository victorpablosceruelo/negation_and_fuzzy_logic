:- load_resource_module(res_wamcount(res_wamcount_res)).

:- head_cost(ub, wamcount, f_wamcount).
:- head_cost(lb, wamcount, f_wamcount).

:- literal_cost(ub, wamcount, f_wamcount).
:- literal_cost(lb, wamcount, f_wamcount).

:- trust_default + cost(ub, wamcount, 0).
:- trust_default + cost(lb, wamcount, 0).
