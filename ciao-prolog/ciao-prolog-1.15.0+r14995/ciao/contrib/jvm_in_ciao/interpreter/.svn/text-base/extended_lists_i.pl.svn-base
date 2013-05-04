
replace([],_,_,[]).
replace([Researched|Old],Researched,Replacement,[Replacement|New]):-
	replace(Old,Researched,Replacement,New).
replace([X|Old],Researched,Replacement,[X|New]):-
	\+(X=Researched),
	replace(Old,Researched,Replacement,New).

init([],_,0).
init([Value|L],Value,I):-
	I > 0,
	I1 is I-1,
	init(L,Value,I1).


