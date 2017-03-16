:- module(hiord_test, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Test for hiord").

:- use_module(engine(io_basic)).

:- export(main/0).
main :-
	ho_test.partial_application_test.

% TODO: defining a nested module with the same name makes the compiler loop, fix
:- module ho_test {
    % Testing higher order calls
    %
    % This code tests predicate abstractions and emulates partial
    % applications by defining functions.
    % 
    % TODO: functional abstractions are not yet ready

    :- export(partial_application_test/0).
    partial_application_test :-
        log("**Testing partial application**"),
        partial_method_test,
        partial_static_test,
        partial_method_test_2,
        partial_static_test_2.

    partial_method_test :-
        log("  (Method - apply on call)"),
        B = ~secret_book(3.1416),
        P1 = B.the_secret0, P1(Z1),
        display(got_the_secret(Z1)), nl,
        P2 = B.something_plus_the_secret0(10), P2(Z2),
        display(something_plus_the_secret(Z2)), nl.

    partial_static_test :-
        log("  (Static - apply on call)"),
        P1 = static_secret0, P1(Z1),
        display(got_static_secret(Z1)), nl,
        P2 = something_plus_static_secret0(10), P2(Z2),
        display(something_plus_static_secret0(Z2)), nl.

    partial_method_test_2 :-
        log("  (Method - apply on expression)"),
        B = ~secret_book(3.1416),
        P1 = B.the_secret0, G1 = P1(Z1), G1,
        display(got_the_secret_2(Z1)), nl,
        P2 = B.something_plus_the_secret0(10), G2 = P2(Z2), G2,
        display(something_plus_the_secret_2(Z2)), nl.

    partial_static_test_2 :-
        log("  (Static - apply on expression)"),
        P1 = static_secret0, G1 = P1(Z1), G1,
        display(got_static_secret(Z1)), nl,
        P2 = something_plus_static_secret0(10), G2 = P2(Z2), G2,
        display(something_plus_static_secret0(Z2)), nl.

    :- export(run/0).
    run :-
  	partial_application_test.
  
    log(Msg) :-
        display(Msg), nl.
}.

% A class to store a secret
:- class secret_book {
    :- export(secret/1).
    :- attr secret.

    :- export(cons__/1).
    cons__(Secret) :- ~secret = Secret.

    % :- export(the_secret0/1). % TODO: not working
    :- partial(the_secret/1, the_secret0/0). % TODO: 'partial' declaration should not be necessary
    the_secret(X) :- X = ~secret.

    % :- export(something_plus_the_secret0/2). % TODO: not working
    :- partial(something_plus_the_secret/2, something_plus_the_secret0/1). % TODO: 'partial' declaration should not be necessary
    something_plus_the_secret(A, X) :- X is A + ~secret.
}.

% The same methods, but static
secret__ := 2.7183.
% TODO: compiled in a different way...
:- export(static_secret0/0).
:- partial(static_secret/1, static_secret0/0). % TODO: 'partial' declaration should not be necessary
static_secret(X) :- X = ~secret__.
% TODO: compiled in a different way...
:- export(something_plus_static_secret0/1). % (fix arity)
% TODO: WRONG! use real partial applications
:- partial(something_plus_static_secret/2, something_plus_static_secret0/1). % TODO: 'partial' declaration should not be necessary
something_plus_static_secret(A, X) :- X is A + ~secret__.
