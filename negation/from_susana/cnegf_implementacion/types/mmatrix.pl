:- module(mmatrix, [mmultiply/3], [assertions,regtypes]).

:- entry mmultiply(X,Y,Z): (var(Z), ground(X), ground(Y), 
	                   nlilist(X), nlilist(Y) ).

:- regtype nlilist/1.

nlilist([]).
nlilist([X|Xs]):- nlist(X), nlilist(Xs).

:- regtype nlist/1.

nlist([]).
nlist([X|Xs]):- num(X), nlist(Xs).

% nlist([X|Xs]):- int(X), nlist(Xs).


 
 %% :- typedef  nlilist ::= [] ; [nlist|nlilist].
 %% 
 %% :- typedef  nlist ::= [] ; [int|nlist].

mmultiply([],_,[]).
mmultiply([V0|Rest], V1, [Result|Others]):-  
	mmultiply(Rest, V1, Others),
	multiply(V1,V0,Result).

multiply([],_,[]).
multiply([V0|Rest], V1, [Result|Others]):-  
	multiply(Rest, V1, Others),
	vmul(V0,V1,Result).

vmul([],[],0).
vmul([H1|T1], [H2|T2], Result):- 
	vmul(T1,T2, Newresult), 
	Product is H1*H2,
	Result is Product+Newresult.


%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

