:- use_package([]).

:- import(messages, [simple_message/1]).
:- use_module(library(compiler), [use_module/3]).

message(M) :-
        ( current_module(messages) -> messages:simple_message(M)
        ; this_module(Mod),
          use_module(engine(messages), all, Mod),
          messages:simple_message(M)
        ).

main :- message('This is the first message.'),
        message('This is the 2nd message.'),
        fail.
main :- message('This is the message no 3.').
