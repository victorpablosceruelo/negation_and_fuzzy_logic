:- module(basic_trans_res,
	[ get_literal_pred/4, 
	  get_pred_literal/2,  
	  replace_literal/4, 
	  mark_parallel_threads_0/2 
	],
	[andprolog,assertions]). 

:- doc(author,"Pedro L@'{o}pez").  
:- doc(author,"Jorge Navas (adapted for resource analysis)").  

:- use_module(resources(size_res(clause_res)), [ith_body_literal/3]). 

:- reexport(resources(gran_res(gran_trans_res)),
	[ seq/2,            
	  clause_translate_to_internal/2, 
	  translate_to_internal/2,  
	  flat_seq_body/2
	]).


get_literal_pred(LitNum,Body,Literal,Pred/A):-
	ith_body_literal(LitNum, Body, Literal),
	functor(Literal,Pred,A).

% Write definition!!!
% ith_body_literal(LitNum, Body, Literal)


get_pred_literal(Lit, F/A):-
	functor(Lit, F, A).

 %% replace_literal(+Body,+Lit,+ByLit,-RepBody)
 %% RepBody is Body with Lit replace by ByLit.

replace_literal((G1,G2),Lit,ByLit,(RG1,RG2)):-
	!,
	replace_literal(G1,Lit,ByLit,RG1),
	replace_literal(G2,Lit,ByLit,RG2).
replace_literal((G1;G2),Lit,ByLit,(RG1;RG2)):-
	!,
	replace_literal(G1,Lit,ByLit,RG1),
	replace_literal(G2,Lit,ByLit,RG2).
replace_literal((G1->G2),Lit,ByLit,(RG1->RG2)):-
	!,
	replace_literal(G1,Lit,ByLit,RG1),
	replace_literal(G2,Lit,ByLit,RG2).
replace_literal((G1=>G2),Lit,ByLit,(RG1=>RG2)):-
	!,
	replace_literal(G1,Lit,ByLit,RG1),
	replace_literal(G2,Lit,ByLit,RG2).
replace_literal(Body,Lit,ByLit,NBody):-
	list(Body),!,
	replace_literal_list(Body,Lit,ByLit,NBody).
replace_literal(Body,Lit,ByLit,ByLit):-
	Body == Lit, !.
replace_literal(Body,_Lit,_ByLit,Body).


replace_literal_list([G],Lit,ByLit,NG):-
	G = resource(Resource,B),
	replace_literal(B,Lit,ByLit,NB),
	NG = (
		resource(Resource) ->
		NB
	     ).
replace_literal_list([G|Gs],Lit,ByLit,NG):-
	G = resource(Resource,B),
	replace_literal(B,Lit,ByLit,NB),
	NG = (
		resource(Resource) ->
		NB
	     ;
		NGs
             ),
	replace_literal_list(Gs,Lit,ByLit,NGs).

mark_parallel_threads_0((GA,GB), (GA0,GB0)):-!, 
	mark_parallel_threads_0(GA, GA0),
	mark_parallel_threads_0(GB, GB0).
mark_parallel_threads_0((GA;GB), (GA0;GB0)):-!,
	mark_parallel_threads_0(GA, GA0),
	mark_parallel_threads_0(GB, GB0).
mark_parallel_threads_0((GA & GB), G):-!,
	mark_parallel_threads_1(GB, [GA], G).
mark_parallel_threads_0('andprolog_rt:&'(GA,GB), G):-!,
	mark_parallel_threads_1(GB, [GA], G).
mark_parallel_threads_0(G, G):-!.


mark_parallel_threads_1((GA & GB), L, G):-!,
	mark_parallel_threads_1(GB, [GA|L], G).
mark_parallel_threads_1('andprolog_rt:&'(GA,GB), L, G):-!,
	mark_parallel_threads_1(GB, [GA|L], G).
mark_parallel_threads_1(G, L, [G|L]):-!.

 %% mark_parallel_threads_1((GA,GB), L, [(GA,GB)|L]):-!.
 %% mark_parallel_threads_1((GA;GB), L, [(GA;GB)|L]):-!.

 %% mark_parallel_threads_1((GA;GB), L, (L;GB0)):-!,
 %%     mark_parallel_threads_0(GB, GB0).
 %% 
 %% mark_parallel_threads_0((GA & GB), G):-!,
 %%     mark_parallel_threads_1(GB, [GA], G).
 %% 
 %% mark_parallel_threads_1(GB, [GA], G).
 %% 
 %% mark_parallel_threads( (GA & (GB,GC)), Plist, ([(Plist & GA & GB)],GC1)):-!,
 %%     mark_parallel_threads(GC, [], GC1).
 %% 
 %% mark_parallel_threads( (GA & (GB;GC)), ([(GA & GB)];GC1)):-!,
 %%    mark_parallel_threads(GC, GC1).
 %%  %% mark_parallel_threads( (GA & (GB -> GC)), ([(GA & GB)] -> GC1)):-!,
 %%  %%    mark_parallel_threads(GC, GC1).
 %%  %% mark_parallel_threads( (GA & (GB => GC)), ([(GA & GB)] => GC1)):-!,
 %%  %%   mark_parallel_threads(GC, GC1).
 %% mark_parallel_threads( (GA & (GB & GC)), [(GA & GB)],GC1)):-!,
 %%     mark_parallel_threads(GC, GC1).
 %% 
 %% mark_parallel_threads(Goal, GoalA):-
 %%    functor(Goal,F,A),
 %%    functor(GoalA,F,A),
 %%    mark_parallel_threads_arg(A, Goal, GoalA).
 %% 
 %% mark_parallel_threads_arg(0, _Goal, _GoalA):-!.
 %% mark_parallel_threads_arg(A, Goal, GoalA):-
 %%     arg(A,Goal,Arg1),
 %%     mark_parallel_threads(Arg1, ArgA),
 %%     arg(A,GoalA,ArgA),
 %%     A1 is A - 1,
 %%     mark_parallel_threads_arg(A1, Goal, GoalA).
 %% 

