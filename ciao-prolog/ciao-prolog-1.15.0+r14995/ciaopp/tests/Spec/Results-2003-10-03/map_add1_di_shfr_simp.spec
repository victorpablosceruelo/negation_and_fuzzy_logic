:- module( _map_add1, [map_add1/2], [assertions,nativeprops,regtypes,rtchecks] ).


map_add1([],_1).
map_add1([X|Xs],Y) :-
        term_typing:nonvar(Y),
        !,
        arithmetic:(X is Y+1),
        map_add1(Xs,Y).
map_add1([X|Xs],Y) :-
        arithmetic:(Y is X-1),
        map_add1(Xs,Y).



