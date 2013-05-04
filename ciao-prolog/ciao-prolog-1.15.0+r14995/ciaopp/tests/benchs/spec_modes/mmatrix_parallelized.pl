:- module(_mmatrix,[mmultiply/3],[]).

:- use_package(andprolog).

mmultiply([],_1,[]).

mmultiply([V0|Rest],V1,[Result|Others]) :-
        (
          ground([V1]),
          indep([[V0,Rest],[V0,Others],[Rest,Result],[Result,Others]]) ->
             multiply(V1,V0,Result) &
             mmultiply(Rest,V1,Others)
        ;
             multiply(V1,V0,Result),
             mmultiply(Rest,V1,Others)
        ).

multiply([],_1,[]).

multiply([V0|Rest],V1,[Result|Others]) :-
        (
          ground([V1]),
          indep([[V0,Rest],[V0,Others],[Rest,Result],[Result,Others]]) ->
             vmul(V0,V1,Result) &
             multiply(Rest,V1,Others)
        ;
             vmul(V0,V1,Result),
             multiply(Rest,V1,Others)
        ).

vmul([],[],0).

vmul([H1|T1],[H2|T2],Result) :-
        Product is H1*H2,
        vmul(T1,T2,Newresult),
        Result is Product+Newresult.

