%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cnegf_tr.pl
%%                                 Juan Manuel Martinez Barrena
%%                                                  version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(cnegf_tr,[add_cnegf/3],[assertions]).

:- use_module(.(finite_facts)).



% The predicate Pred is finite. In finite_facts.pl it is found:
%      finite(P,A) 
% where P is the Pred's name and A its arity.
condicionPredicado(Pred) :- 
	functor(Pred,P,A),
	finite_facts:finite(P,A).



% add_cnegf(Term,TermList,Module) sustitutes Term in the program
%   Module if it is a neg -> cnegf whenever the condition
%   condicionPredicado succeeds
add_cnegf(end_of_file, end_of_file, _):- 
	!.
add_cnegf(neg(Pred), cnegf(Pred), _):-
	condicionPredicado(Pred), !.

