:- module(notime_stats_tr,[no_time_stats/2],[]).

:- use_package(assertions).

no_time_stats((H:-B),(H:-B1)):-
	no_time_stats_(B,B1).
%% no_time_stats((:- use_module(spec(time_stats),_)),[]).
%% no_time_stats((:- use_module(spec(time_stats))),[]).

no_time_stats_(G,G):-
	var(G), !.
no_time_stats_((A,B),(A0,B0)):- !,
	no_time_stats_(A,A0),
	no_time_stats_(B,B0).
no_time_stats_((A;B),(A0;B0)):- !,
	no_time_stats_(A,A0),
	no_time_stats_(B,B0).
no_time_stats_((A->B),(A0->B0)):- !,
	no_time_stats_(A,A0),
	no_time_stats_(B,B0).
no_time_stats_(increment_time(_,_),true):- !.
no_time_stats_(G,G).
