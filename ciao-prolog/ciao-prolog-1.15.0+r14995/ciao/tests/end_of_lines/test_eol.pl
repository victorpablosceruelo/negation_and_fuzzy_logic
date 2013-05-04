:- module(test_eol, [main/0], [assertions]).

:- use_module(library(read)).

:- test main.

main :-
        open('end_of_lines/file.txt', read, S),
        ( check_lines(S) ->
            close(S)
        ;
            close(S), fail
        ).

check_lines(S) :-
        read(S, T),
        line_count(S, N),
        check_line(T, N),
        ( T = end_of_file ->
            true
        ; 
            check_lines(S)
        ).

check_line(a, 0).
check_line(b, 1).
check_line(c, 2).
check_line(d, 3).
check_line(end_of_file, 4).
