:- module(to_debug, [main/0], [assertions, debug]).

main :-
        test([1,2,3]).

test(X) :-
        display(X),
        spy(foo/1),
        foo(X),
        notrace,
        foo(X),
%       notrace,
        nl.

foo([]).
foo([X|T]) :-
        bar(X),
        foo(T).

bar(X) :-
        display(X).
