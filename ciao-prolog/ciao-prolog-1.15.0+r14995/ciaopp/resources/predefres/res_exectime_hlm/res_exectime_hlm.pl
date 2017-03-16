:- package(res_exectime_hlm).

:- include(res_exectime_hlm(res_exectime_hlm_indep_auto)).
:- include(res_exectime_hlm(res_exectime_hlm_dep_auto)).

:- doc(bug, "This package causes a Memory allocation failure (due
	to it uses too many resources -- EMM.").
