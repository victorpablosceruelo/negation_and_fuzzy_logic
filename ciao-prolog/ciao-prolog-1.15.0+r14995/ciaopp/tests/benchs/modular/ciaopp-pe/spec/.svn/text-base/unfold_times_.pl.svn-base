:- module(unfold_times_,
	[init_unfold_times/0,
	 ask_unfold_times/1,
	 increment_unfold_time/1,
	 increment_transformation_time/1,
	 global_time_ellapsed/3
	 ],
	 []).

:- use_package(assertions).

:- doc(title,"Accurate Timing of Unfolding").

:- doc(author, "Germ@'{a}n Puebla").

:- doc(module," This module contains a series of predicates for 
	measuring the time actually taken by local control (unfolding).").

:- use_module('..'(preprocess_flags_)).

:- data unfold_time/1.

:- data transform_time/1.

init_unfold_times:-
	retractall_fact(unfold_time(_)),
	retractall_fact(transform_time(_)).

ask_unfold_times(Info):-
	current_pp_flag(local_control,LC),
	LC == off,!,
	Info = [].
ask_unfold_times([(transform,T),(unfold,U)]):-
	current_fact(unfold_time(U)),
	current_fact(transform_time(T)).


increment_unfold_time(T_u):-
	unfold_time(Time),!,
	NTime is Time + T_u,
	retractall_fact(unfold_time(_)),
	asserta_fact(unfold_time(NTime)).
increment_unfold_time(T_u):-
	asserta_fact(unfold_time(T_u)).

increment_transformation_time(T_T):-
	transform_time(Time),!,
	NTime is Time + T_T,
	retractall_fact(transform_time(_)),
	asserta_fact(transform_time(NTime)).
increment_transformation_time(T_T):-
	asserta_fact(transform_time(T_T)).


global_time_ellapsed(GT_After,GT_Before,TE):-
	TE is round(1000*(GT_After - GT_Before))/1000.

:- doc(version_maintenance,dir('../version')).  

:- doc(version(1*0+790,2004/10/30,09:32*31+'UTC'), "Added module
@tt{unfold_times}. This module contains a series of predicates for
measuring the time actually taken by local control (unfolding).
(German Puebla)").

