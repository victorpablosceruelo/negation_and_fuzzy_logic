:- module(test_andorra, [main/0], [assertions]).

:- use_module(library(write)).

:- use_module(crypt_and_user).
:- use_module(mqu_and_user).

:- test main.

main:-
        write('Solving SEND+MORE=MONEY using an Andorra-based strategy'),
        nl,
        crypt_and_user:test,
        nl,
        write('Solving 8 queens using Andorra'),
        mqu_and_user:queens(s(s(s(s(s(s(s(s(0))))))))),
        nl,
	!.
