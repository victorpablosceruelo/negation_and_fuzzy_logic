:- module( _plus1_multiple_too_many_hand, [p/2], [assertions,nativeprops,regtypes,rtchecks] ).


p(Value,Res) :-
        plus1_1(Tmp,Value),
        plus1_2(Tmp,Tmp1),
        plus1_3(Tmp1,Tmp2),
        plus1_4(Tmp2,Tmp3),
        plus1_5(Tmp3,Res).

plus1_1(X,Y) :-
        term_typing:var(X),
        arithmetic:(X is Y-1).

plus1_2(X,Y) :-
        arithmetic:(Y is X+1).

plus1_3(X,Y) :-
        arithmetic:(Y is X+1).

plus1_4(X,Y) :-
        arithmetic:(Y is X+1).

plus1_5(X,Y) :-
        arithmetic:(Y is X+1).



