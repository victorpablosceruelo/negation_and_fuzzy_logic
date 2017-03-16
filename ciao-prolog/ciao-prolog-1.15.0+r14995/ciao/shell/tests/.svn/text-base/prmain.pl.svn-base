:- use_package([]).

:- use_module(prau).

main :- prolog_flag(argv, [Arg]),
        main(Arg).

main(t) :-
        (a(X), display(X), nl, fail ; true).

main(a) :-
        display(a), nl,
        (a(X), display(X), nl, fail ; true),
        display(.), nl,
        (a(X), display(X), nl, fail ; true),
        b((h:-b), H, B),
        display(H), nl.

main(b) :-
        display(b), nl,
        b(a, H, B),
        display(if(H,B)), nl,
        b((a:-b), H1, B1),
        display(if(H1,B1)), nl.
