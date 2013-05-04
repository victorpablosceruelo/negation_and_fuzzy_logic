:- load_resource_module(res_exectime_me(res_exectime_me_res)).

:- head_cost(ub, exectime_me, f_exectime_me).
:- head_cost(lb, exectime_me, f_exectime_me).

:- literal_cost(ub, exectime_me, f_exectime_me).
:- literal_cost(lb, exectime_me, f_exectime_me).

:- trust_default + cost(ub, exectime_me, 0).
:- trust_default + cost(lb, exectime_me, 0).
