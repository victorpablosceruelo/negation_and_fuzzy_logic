%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cnegf_tr.pl
%%                                 Juan Manuel Martinez Barrena
%%                                                  version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(cnegf_tr,[add_cnegf/3],[assertions]).

:- use_module(.(finite_facts)).

%hay_finite(P,A) :- 
%	finite_facts:finite(P,A).

condicionPredicado(Pred) :- 
	functor(Pred,P,A),
	finite_facts:finite(P,A).
%	hay_finite(P,A).


% add_cnegf(Term,TermList,Module) sustitutes Term in the program
% Module if it is a neg -> cnegf
add_cnegf(end_of_file, end_of_file, _):- 
	!.
add_cnegf(neg(Pred), cnegf(Pred), _):-
	condicionPredicado(Pred), !.

