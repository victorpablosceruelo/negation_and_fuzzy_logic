%[qsort([1,2,3,4,5,6,7,8,9,10,71,72,73,74,75,76,77,78,79,80,11,12,13,14,15,16,17,18,19,20,21,22,23],L)].

qsort(As,Bs):- qsort_dl(As,Bs,[]).

qsort_dl([],R,R).
qsort_dl([X|L],R,R2) :-
	partition(L,X,L1,L2),
        qsort_dl(L2,R1,R2),
	qsort_dl(L1,R,[X|R1]).


partition([],_,[],[]).
partition([E|R],C,[E|Left1],Right):- 
	E =< C, 
	partition(R,C,Left1,Right).
partition([E|R],C,Left,[E|Right1]):-
	E > C, 
	partition(R,C,Left,Right1).


