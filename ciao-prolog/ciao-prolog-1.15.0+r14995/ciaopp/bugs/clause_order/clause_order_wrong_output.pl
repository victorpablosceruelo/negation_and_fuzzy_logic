:- module( _clause_order, [p/1], [assertions] ).


:- data q/1.  %bug-report: should not be here

:- true pred p(A).

p(A) :-
        r(a,A),
        q(A) .

:- true pred r(A,B).

r(A,A).

%% %% :- check pred q(_1)
%% %%    : ground(_1).

%bug-report: sometime ago we knew how to preserve variabe names:
:- check calls q(_4172)
         : ground(_4172).

:- true pred q(A).

q(a).
q(b).
