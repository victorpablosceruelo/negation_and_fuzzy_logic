% variable shunting + setarg = bug

% A1
%
%
%

:- module(_, _, []).

:- use_module(engine(internals), ['$setarg'/4]).
:- use_module(library(prolog_sys), [garbage_collect/0]).

main :-
	display('All tests should succeed'), nl,
	p(false, false),
	p(true, false),
	p(false, true),
	p(true, true).

p(UseSetarg, DoGC) :-
	display(testing(UseSetarg, DoGC)), nl,
	A = f(_,_),
	B = f(_,_),
	c(I), % that creates a choicepoint
	B = A, % that trails
	( I = 1 ->
	    ( UseSetarg = true ->
	        '$setarg'(1, A, 5, on) %% A1
	    ;
		arg(1, A, 5) %% A2
	    ), 
	    % if shunting does not take into account setart, it will
	    % incorrectly make the first argument of B have the value 5 
            % even on failure
	    ( DoGC = true ->
	        garbage_collect %% B1
	    ;
		true %% B2
	    )
	;
	    true
	),
	display('  '), display(a(A)), display(' '),
	display(b(B)), nl,
	( I = 1 ->
	    display('  '), display(failing), nl,
	    fail
	; arg(1, B, X),
	  ( var(X) ->
	      display('The test succeed'), nl
	  ; display('The test failed'), nl
	  ),
	  true
	).

c(1).
c(2).
