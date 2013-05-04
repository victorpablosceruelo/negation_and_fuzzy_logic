:- load_resource_module(res_arith(res_arith_res)).

:- include(res_arith(res_arith_assr_auto)).
:- include(res_arith(res_arith_auto)).

:- head_cost(ub, arith, 0).
:- head_cost(lb, arith, 0).

:- literal_cost(ub, arith, f_arith).
:- literal_cost(lb, arith, f_arith).

:- trust_default + cost(ub, arith, 0).
:- trust_default + cost(lb, arith, 0).
