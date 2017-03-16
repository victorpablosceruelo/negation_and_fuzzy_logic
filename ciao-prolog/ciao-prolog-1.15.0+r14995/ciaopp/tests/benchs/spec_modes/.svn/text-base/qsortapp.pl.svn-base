
:- module( qsortapp, [qsort/2], [assertions] ).


:- entry qsort([1,7,2,8,0,9],L).

qsort([],[]).
qsort([X|L],R) :-
	partition(L,X,L1,L2),
	qsort(L2,R2),
        qsort(L1,R1), 
        append(R1,[X|R2],R).

partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):- E =< C, 
 	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):- E > C,
	partition(R,C,Left,Right1).

append([],X,X).
append([H|X],Y,[H|Z]):- append(X,Y,Z).


%% -----------------------------------------------------------------------------
%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

