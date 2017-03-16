:- module(magic3, [do/0]).

:- use_package(gecode).

do:-
    Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9],
    Vars in 1..9,
    all_different(Vars),
    X1+X2+X3 .=. 15,
    X4+X5+X6 .=. 15,
    X7+X8+X9 .=. 15,
    X1+X4+X7 .=. 15,
    X2+X5+X8 .=. 15,
    X3+X6+X9 .=. 15,
    X1+X5+X9 .=. 15,
    X3+X5+X7 .=. 15,
    labeling(Vars).
