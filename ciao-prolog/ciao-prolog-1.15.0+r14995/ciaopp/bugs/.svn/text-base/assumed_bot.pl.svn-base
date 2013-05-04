% When checking assertions in this program with :
%
% ?- use_module(ciaopp(ciaopp)).
% set_pp_flag(verbose_ctchecks,on).
% set_pp_flag(ass_not_stat_eval,warning).
% set_pp_flag(pred_ctchecks, new_all_succ).
% ?- module(assumed_bot), analyze(eterms), acheck.
% 
% the following warning pops up:
%
% {WARNING (typeslib): Type 'basic_props:list'(_18184) not defined, assumed bot}
%
% Hint: uncomment the entry, everything is fine again.


:- module(_,[p/1],[assertions, regtypes]).

%:- entry p(A): list(A).

:- calls p(A) : list(A,num).
p(_).
