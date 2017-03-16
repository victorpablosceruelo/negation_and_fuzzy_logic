:- package(sta_ticks).

% NOTE: we have selected the model 53 by default, modify this file if
% you like to use other module.

:- use_package(predefres(res_ticks)).
:- include(res_arith(res_arith_assr)).
:- include(res_exectime_hlm(auto(res_exectime_hlm_53_assr))).
%:- include(resources(saved_results(platform))).
:- compound_resource(ticks, [arith, exectime_hlm_53]).
:- trust_default + cost(ub, ticks, 0).
:- trust_default + cost(lb, ticks, 0).
