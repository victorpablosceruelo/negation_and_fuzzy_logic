:- module(quick_test, [], []).

% This does a quick tests (nondeterminism, local functors, ...)

:- export(attach/1).
attach(Container) :-
        simple_run_widget("Quick Samples", run).attach(Container).
  
:- export(run/0).
run :-
        console.clear,
        log("**A quick test (writting to the console)**"),
	multiarity_test,
        display_test,
        nondet_test.

multiarity_test :-
        log("Multi-arity predicates can be defined"),
        foo,
        foo(1),
        foo(1,2).
foo :- console.display(i_am_foo), console.nl.
foo(X) :- console.display(i_am_foo(X)), console.nl.
foo(X,Y) :- console.display(i_am_foo(X,Y)), console.nl.

% TODO: like ':- local' in XSB: 
:- local and/0.
:- local some_local/1.
  
display_test :-
        M = 10 + 100,
        console.display([usermod,terms,implicitly,defined]), console.nl,
        console.display([and,some_local(terms)]), console.nl,
        console.display([~mynumber,M,A,B,A]), console.nl,
        console.display(~map_type([~mynumber,M,A,B,A])), console.nl.

mynumber := 123.
  
map_type([]) := [].
map_type([X|Xs]) := [~get_type(X)| ~map_type(Xs)].

get_type(X) := Y :-
	( '$kind_of'(X, var) -> Y = "var"
	; '$kind_of'(X, t_num) -> Y = "t_num"
	; '$kind_of'(X, t_string) -> Y = "t_string"
	; Y = "other"
	).

member(X, [Y|Xs]) :- X = Y ; member(X, Xs).

append([], B, B).
append([X|Y], B, [X|Z]) :- append(Y, B, Z).

nondet_test :-
        ( append(A, B, [1,2,3,4,5,6,7,8]),
          ( member(p(N,L), [p("first", A), p("second", B)]),
	    console.display("("),
            console.display(N),
	    console.display(") "),
	    console.display(L),
	    console.display(" ")
          ; console.nl
	  ),
          fail
        ; true
        ).

log(Msg) :-
        console.display(Msg), console.nl.

