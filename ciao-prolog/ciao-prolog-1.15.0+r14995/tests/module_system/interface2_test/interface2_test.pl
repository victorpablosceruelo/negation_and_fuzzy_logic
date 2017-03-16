:- module(interface2_test, [], [compiler(complang)]).

:- doc(title, "Testing interfaces (2)").
:- doc(author, "Jose F. Morales").

:- doc(module, "Like @tt{testing_interfaces}, but split in several
   files").

% TODO: Automatize (add PROPS file)
% $ ciaotool comp --dynexec interface2_test interface2_test 
% $ ./interface2_test
% A = point<1,1>
% B = point<2,2>
% C = circle<0,0,10>
% point<1,1>
% point<2,2>
% circle<0,0,10>
% $ ciaodump --disasm interface2_test

:- doc(bug, "(See TODO comments in the code)").

:- use_module(+(printable_)).

:- use_module(+(point_)).
:- '$trust_statemodel'(point, single).
:- use_module(+(circle_)).
:- '$trust_statemodel'(circle, single).

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

