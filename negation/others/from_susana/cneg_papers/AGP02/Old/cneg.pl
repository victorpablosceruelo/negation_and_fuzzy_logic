
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Program  CNEG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% To be able to call Pred from neg
:- meta_predicate
        cneg(goal,_). 

%:- data stored_clause/2.

% cneg(Goal) makes the constructive negation of the goal Pred
cneg(Goal,Subgoal):-
	stored_clause(Goal,Body),
	member(Subgoal,Body).

:- load_compilation_module(library('cneg/cneg_tr')).
:- add_sentence_trans(flat_pred/3).
