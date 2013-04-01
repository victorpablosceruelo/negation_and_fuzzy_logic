%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Package CNEGT (complete Constructive NEGation with  %
%  Typed arguments) for negating a predicate whose     %
%  arguments can be typed                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cnegt_tr,[same_pred/3],[]).
:- use_module(library(aggregates),[findall/3, findall/4]).

% dynamic predicates to store clauses that are going
% to be expanded. They are used to expand them in a
% continous way
:- data pre_clause/2.
:- data pre_pred/1.

% free_list(Num,VarList). VarList is a list with a 
% number of 'Num' free variables
free_list(0,[]):-
        !.
free_list(N,[_|L]):-
	N1 is N-1,
	free_list(N1,L).

% change_name(OriginalName,TransfName). TransfName is
% the same as OriginalName but concatenated to the 
% sufix '_check_types'
change_name(NamePred,Name_inst) :-
	atom_concat(NamePred,'_check_types',Name_inst).

% same_pred/3 is a predicate that lets the code as
% the original; it means that does a neutral expansion
same_pred(end_of_file,end_of_file,_).

% % walk_list(List,Result) returns in Result the list of 
% % continuos clauses that is going to include in the 
% % expanded program for the aparition of each predicate
% % in the list 'List'
% walk_list([],[end_of_file]).
% walk_list([Pred|T],Cls):-
% 	findall(CL,(retract_fact(pre_clause(Pred,CL))),Cls,RT),
%         walk_list(T,RT).
