:- package(exectimehl).

:- use_package(ciaopp(tests(resources))).

:- include(res_arith(res_arith_assr)).
:- include(res_exectime_hlm(auto(res_exectime_hlm_63_assr))).
:- include(resources(saved_results(platform))).
:- resource exectime_model4.
:- compound_resource(exectime_model4, [arith, exectime_hlm_63]).
:- trust_default + cost(ub, exectime_model4, 0).
:- trust_default + cost(lb, exectime_model4, 0).
