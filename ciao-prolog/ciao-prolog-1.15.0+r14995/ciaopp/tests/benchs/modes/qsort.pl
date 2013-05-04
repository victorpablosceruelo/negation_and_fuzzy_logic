:- module( qsort, [qsort/2], [assertions] ).

:- entry qsort(As,Bs)
 	: ( ground(As), var(Bs) ).
% 	: ( list(As,num), var(Bs) ).

:- push_prolog_flag(multi_arity_warnings,off).

qsort(As,Bs):- qsort(As,Bs,[]).

qsort([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsort(L2,R1,R2),
	qsort(L1,R,[X|R1]).
qsort([],R,R).

:- pop_prolog_flag(multi_arity_warnings).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):- E < C, !,
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	partition(R,C,Left,Right1).

%% -----------------------------------------------------------------------------
%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

