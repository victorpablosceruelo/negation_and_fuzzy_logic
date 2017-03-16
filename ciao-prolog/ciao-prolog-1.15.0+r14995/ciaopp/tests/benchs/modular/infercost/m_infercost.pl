
:- module(m_infercost,
	[ do_dependency_analysis/7,
	  do_determinacy_analysis/4,
	  do_relation_analysis/9,
	  do_size_analysis/7,
	  do_solution_analysis/9,
	  do_time_analysis/9
	],
	[]).

%% :- use_module(database,[ loaded/1 ]).
%% 
%% :- initialization(asserta_fact(loaded(infercost))).

:- use_module(size).
:- use_module(dependency).
:- use_module(determinacy).
:- use_module(solution).
:- use_module(time).

do_time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time) :-
	time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Time).
do_solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol) :-
	solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,Sol).
do_determinacy_analysis(Comp,BT,ST,Adg) :-
	determinacy_analysis(Comp,BT,ST,Adg).
do_relation_analysis(CompL,BT,ST,Comp,SizeL,AdgL,GvL,LdgL,Sol) :-
	relation_analysis(CompL,BT,ST,Comp,SizeL,AdgL,GvL,LdgL,Sol).
do_size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size) :-
	size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size).
do_dependency_analysis(CompL,BT,ST,AdgL,LdgL,GvL,Error) :-
	dependency_analysis(CompL,BT,ST,AdgL,LdgL,GvL,Error).

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

