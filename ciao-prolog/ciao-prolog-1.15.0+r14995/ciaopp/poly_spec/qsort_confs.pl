:- module( _, [ qsort_confs/2 ], [assertions, isomodes] ).

:- entry qsort_confs(As,Bs)
 	: ( ground(As), var(Bs) ).
% 	: ( list(As,num), var(Bs) ).

:- doc(title,"Quicksort of Configurations").

:- doc(author, "Germ@'{a}n Puebla").
:- doc(author, "Claudio Ochoa").

:- doc(module,"This module implements a quicksort over
	configurations").

:- push_prolog_flag(multi_arity_warnings,off).

:- pred qsort_confs(+As,-Bs) # "Performs a quicksort of a list
	@var{As} of configurations and returns the result in
	@var{Bs}. Each configuration is a tuple @tt{e(S,H,Val)}, where
	@tt{S} is a list of atoms to be visited, @tt{H} is the list of
	already visited atoms, and @tt{Val} is the fitness of the
	current configuration".

qsort_confs(As,Bs):- 
	qsort_confs(As,Bs,[]).

qsort_confs([e(TV,V,Value)|L],R,R2) :-
	partition(L,Value,L1,L2),
        qsort_confs(L2,R1,R2),
	qsort_confs(L1,R,[e(TV,V,Value)|R1]).
qsort_confs([],R,R).

:- pop_prolog_flag(multi_arity_warnings).

partition([],_,[],[]).
partition([e(TV,V,Value)|R],C,[e(TV,V,Value)|Left1],Right):- 
	Value < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	partition(R,C,Left,Right1).
/*
merge([],L,L):-!.
merge(L,[],L):-!.
merge([e(TV1,V1,Value1)|R1],[e(TV2,V2,Value2)|R2],Merge):- 
	Value1 >= Value2, !,
	Merge = [e(TV1,V1,Value1)|More_Merged],
	merge(R1,[e(TV2,V2,Value2)|R2],More_Merged).
merge(Sols1,[Sol2|R2],Merge):- 
	Merge = [Sol2|More_Merged],
	merge(Sols1,R2,More_Merged).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

