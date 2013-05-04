:- [].

:- export(main/0).

:- use_module(engine(internals)).

main :- '$predicate_property'('system:pause'(_), Entry, Bits),
        message(['entry=',Entry,', bits=',Bits]),
        pause(1),
        '$abolish'('system:pause'(_)),
        main.

pause(X) :- display(X).
