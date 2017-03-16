:- module(tr_tr, [expand_goal/3, goal_trans/3], []).

expand_goal(A,B,Mod) :-
	display(goal_from_expand_goal(A,B,Mod)), nl,
	fail.

goal_trans(A,B,Mod) :-
	display(goal_from_goal_trans(A,B,Mod)), nl,
	fail.

