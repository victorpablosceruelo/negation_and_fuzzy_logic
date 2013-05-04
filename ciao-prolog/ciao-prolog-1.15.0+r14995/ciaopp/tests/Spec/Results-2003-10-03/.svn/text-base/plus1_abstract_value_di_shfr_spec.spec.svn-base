:- module( _plus1_abstract_value, [p/2], [assertions,nativeprops,regtypes,rtchecks] ).


p(Value,Res) :-
        arithmetic:(Tmp is Value+3),
        plus1(Tmp,Res).

plus1(X,Y) :-
        arithmetic:(Y is X+1).



