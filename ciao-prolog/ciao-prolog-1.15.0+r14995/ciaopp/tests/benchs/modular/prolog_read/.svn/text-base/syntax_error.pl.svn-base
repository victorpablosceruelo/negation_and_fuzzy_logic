:- module(syntax_error,[ syntax_error/1, syntax_error/2 ], []).

:- use_module(library(lists), [length/2]).
:- use_module(library(old_database), [recorda/3, recorded/3]).
:- use_module(library(ttyout), [ttynl/0, ttyput/1]).

syntax_error(Message,List) :-
        ttyout:ttynl,
        io_basic:display(**),
        my_display_list(Message),
        lists:length(List,Length),
        old_database:recorda(syntax_error,length(Length),_M),
        !,
        basiccontrol:fail.

my_display_list([Head|Tail]) :-
        ttyout:ttyput(32),
        display_token(Head),
        !,
        my_display_list(Tail).
my_display_list([]) :-
        ttyout:ttynl.

:- push_prolog_flag(multi_arity_warnings,off).

syntax_error(List) :-
        old_database:recorded(syntax_error,length(AfterError),Ref),
        data_facts:erase(Ref),
        lists:length(List,Length),
        arithmetic:(BeforeError is Length-AfterError),
        display_list(List,BeforeError),
        !.

:- pop_prolog_flag(multi_arity_warnings).

display_list(X,0) :-
        io_basic:nl,
        io_basic:display('<<here>> '),
        !,
        io_basic:nl,
        display_list(X,99999).
display_list([Head|Tail],BeforeError) :-
        display_token(Head),
        ttyout:ttyput(32),
        arithmetic:(Left is BeforeError-1),
        !,
        display_list(Tail,Left).
display_list([],_N) :-
        ttyout:ttynl.

display_token(atom(X)) :-
        !,
        io_basic:display(X).
display_token(var(_V,X)) :-
        !,
        io_basic:display(X).
display_token(integer(X)) :-
        !,
        io_basic:display(X).
display_token(string(X)) :-
        !,
        io_basic:display(X).
display_token(X) :-
        io_basic:display(X).




