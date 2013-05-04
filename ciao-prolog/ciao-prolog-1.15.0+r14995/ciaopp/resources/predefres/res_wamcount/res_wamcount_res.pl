:- module(_, [f_wamcount/2], [assertions]).

:- use_module(sta_exectime(sta_exectime_res)).

f_wamcount(LitInfo, Cost) :-
	exectime(cn, LitInfo, Cost).
