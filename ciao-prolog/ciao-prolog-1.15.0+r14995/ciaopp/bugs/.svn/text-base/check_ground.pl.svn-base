
% [[a]] is not recognized as a subtype of list(gnd)
% as demonstrated by this program. The assertions get status
% as indicated in the comments. Only eterms analysis was
% used, as the following:

% ?- use_module(ciaopp(ciaopp)).
% ?- set_pp_flag(verbose_ctchecks,on).
% ?- set_pp_flag(ass_not_stat_eval,warning).
% ?- set_pp_flag(pred_ctchecks, new_all_succ).

% ?- module(check_ground), analyze(eterms), acheck.


:- module(_,[p/0],[assertions, regtypes]).

p :- 
	r([[a]]).

:- check calls r(A) : list(A, list(gnd)). % becomes checked
:- check calls r(A) : list(A, gnd).       % becomes false
:- check calls r(A) : gnd(A).             % becomes checked



r(_).
