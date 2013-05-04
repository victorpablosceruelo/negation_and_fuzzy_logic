:- module(interface_test, [], [compiler(complang)]).

:- doc(title, "Testing interfaces").
:- doc(author, "Jose F. Morales").

:- doc(module, "This is a test program for classes implementing
   interfaces. Interfaces allow controlled dynamic dispatching. That
   is, once we check that a term implements the interface, we can
   safely propagate that information to avoid dynamic tests.").

% TODO: Automatize (add PROPS file)
% $ ciaotool comp --dynexec interface_test interface_test 
% $ ./interface_test
% A = point<1,1>
% B = point<2,2>
% C = circle<0,0,10>
% point<1,1>
% point<2,2>
% circle<0,0,10>
% $ ciaodump --disasm interface_test

:- doc(bug, "(See TODO comments in the code)").

:- public main/0.
main :-
	% Create three objects.
	A = ~point.new(1,1),
	B = ~point.new(2,2),
	C = ~circle.new(0,0,10),
	% Show the objects through their show/0 method
	% (it is statically checked that the method is defined)
	display('A = '), A.show, nl,
	display('B = '), B.show, nl,
	display('C = '), C.show, nl,
	% Show the objects through the 'printable' interface
	% (it is statically checked that the method is defined)
	show_all([A,B,C]).

% TODO: :- meta_predicate show_all(list(printable)) ?
show_all(Xs) :-
	% TODO: avoid trust(_), interface checking is not being performed
	maplist((''(X) :- trust(X instance_of printable), X.show, nl), Xs).

:- interface printable {
    :- '$statemodel'(single). % TODO: This should not be necessary
    :- multifile(show/0). % TODO: Use different syntax
}.

:- class point {
    :- extends printable. % TODO: No error is detected if this line is missing
    :- attr x :: any.
    :- attr y :: any.

    :- constructor new_/2.
    new_(X,Y) :- ~x = X, ~y = Y.

    % TODO: missing 'public', but no error reported
    show :-
        display('point<'), display(~x), display(','), display(~y), display('>'). 
}.

:- class circle {
    :- extends printable.
    :- attr x :: any.
    :- attr y :: any.
    :- attr r :: any.

    :- constructor new_/3.
    new_(X,Y,R) :- ~x = X, ~y = Y, ~r = R.

    % TODO: missing 'public', but no error reported
    show :-
        display('circle<'), display(~x), display(','), display(~y), display(','), display(~r), display('>'). 
}.
