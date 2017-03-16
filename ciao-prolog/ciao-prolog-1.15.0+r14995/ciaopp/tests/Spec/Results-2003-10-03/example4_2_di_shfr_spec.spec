:- module( _example4_2, [p/1], [assertions,nativeprops,regtypes,rtchecks] ).


p(X) :-
        q(X),
        r(X).

q(X) :-
        term_basic:(X=a).
q(X) :-
        q(X).

r(X) :-
        term_basic:(X=a).
r(X) :-
        term_basic:(X=b).



