:- module(load_dynlibs, [main/0, check_interface/0], []).


:- use_module(library(random)).
:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(format)).

:- use_module(check_links).  %% Uses sockets as well
:- use_module(byte_lists).
:- use_module(foreign_init).
:- use_module(math).
:- use_module(objects).
:- use_module(strings_and_atoms).
:- use_module(exceptions_example).

main:-
       check_random,
       check_pillow,
       check_interface.


check_random:-
        write('Checking series of random numbers... '),
        srandom(2),
        nrandom(1000, Rand1),
        srandom(2),
        nrandom(1000, Rand2),
        (
            Rand1 = Rand2 ->
            write('Ok!'), 
            nl
        ;
            write('**** Error, they differ!!!'),
            nl
        ).

nrandom(0, []).
nrandom(N, [R|Rs]):-
        random(R),
        N1 is N - 1,
        nrandom(N1, Rs).


url('http://clip.dia.fi.upm.es').

check_pillow:-
        url(U),
        check_links:main([U]).

list_len(100).

check_interface:-
%        check_list_construction,
	check_init,
 	check_math,
 	check_addresses,
%	check_strings,
	check_exceptions.

check_list_construction:-
        list_len(N),
        obtain_list(N, Len, Lst),
        length(Lst, Llen),
        (
            Len = N , Len = Llen ->
            write('OK constructing lists in C'), 
            nl,
            show_list(N, Lst)
        ;
            write('size mismatch when constructing lists in C'), 
            nl
        ).


check_init:- print_time.

angle(0.455563).

check_math:-
        write('Checking some math predicates...'),
        angle(R),
        sin(R, S),
        cos(R, C),
        Radius is sqrt(S*S + C*C),
        (
            Radius == 1.0 ->
            write('Ok with math'),
            nl
        ;
            nl,
            write('Problem with math: radius = '),
            write(Radius),
            write(' instead of 1.0'),
            nl
        ).

check_addresses:-
        write('Checking passing pointeres to and from C'),
        nl,
        so(0),
        so(2),
        so(3).

so(N):-
        object(N, O),
        show_object(O).


check_strings:-
        a_string(S),
        format("A static C string: ~s~n", [S]),
        lookup_string(3, S1),
        format("A dynamic C string: ~s~n", [S1]),
        lookup_atom(3, A1),
        format("A dynamic C string passed as atom: ~w~n", [A1]),
        write('Printing from C: '), nl,
        show_string(S1),
        show_atom(A1).


check_exceptions:-
        write('Checking exceptions...'), nl,
	safe_codes_to_number("thisisnotanumber", _N).
