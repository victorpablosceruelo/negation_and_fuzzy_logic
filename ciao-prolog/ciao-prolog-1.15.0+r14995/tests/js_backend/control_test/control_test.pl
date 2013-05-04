:- module(control_test, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Test for control").

% TODO: Missing exceptions

:- use_module(engine(io_basic)).

:- export(main/0).
main :-
        nondet_test.

nondet_test :-
        ( append(A, B, [1,2,3,4,5,6,7,8]),
          ( member(p(N,L), [p("first", A), p("second", B)]),
	    display("("),
            display(N),
	    display(") "),
	    display(L),
	    display(" ")
          ; nl
	  ),
          fail
        ; true
        ).

member(X, [Y|Xs]) :- X = Y ; member(X, Xs).

append([], B, B).
append([X|Y], B, [X|Z]) :- append(Y, B, Z).
