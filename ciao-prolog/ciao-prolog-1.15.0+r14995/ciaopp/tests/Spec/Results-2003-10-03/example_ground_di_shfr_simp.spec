:- module( _example_ground, [p/1], [assertions,nativeprops,regtypes,rtchecks] ).


p(Y) :-
        term_basic:(X=f(a,g(b))),
        term_basic:(Y=3).



