:- use_package([]).

:- use_module(library(sockets)).

main :- connect_to_socket(clip, 80, S), get_code(S, C), put_code(C), close(S).
