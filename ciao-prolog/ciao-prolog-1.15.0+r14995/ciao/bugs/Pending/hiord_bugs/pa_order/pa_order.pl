:- module(pa_order, [main/0], [hiord]).

% Bug: Ciao defines a partial application order that is
% counter-intuitive, incompatible with other systems, and which lacks
% some good properties that one usually expect from partial
% applications / point-free style.
%
% (Jose F. Morales)

% 'GOOD' PROPERTIES THAT THE CURRENT APPLICATION ORDER LACKS
%
% Applying 2 arguments in two steps is equivalent to applying 2
% arguments in one step.
%
%   % SWI  
%   ?- X = append, Y = call(X, [1]), call(Y, [2], Z).
%   X = append,
%   Y = call(append, [1]),
%   Z = [1, 2].
%
%   % Ciao
%   ?- use_package(hiord).
%   % ?- X = append, Y = call(X, [1]), call(Y, [2], Z). % not supported
%   % try this, which seems equivalent
%   ?- X = append, Y = append([1]), call(Y, [2], Z).
%
%   X = append,
%   Y = append([1]),
%   Z = [2,1] ?    % DIFFERENT RESULT!!
%
% As a corollary, we are incompatible with other systems (see
% yap_libs/...).

% Some other examples (with some internal tracing messages)
% ===========================================================================
%
% Ciao 1.15.0-13694: Fri Aug 19 18:14:48 CEST 2011
% ?- use_package(hiord).
% {Using package /Users/jfran/Documents/svn/ciao/ciao/lib/hiord.pl
% Note: module hiord_rt already in executable, just made visible
% }
%
% yes
% ?- X = append, X([1],[2],Y).
% normal_unify_head_args(pred_expansion(append,3,user(/Users/jfran/Documents/svn/ciao/ciao/library/toplevel/toplevel__scope),-,compile,PA(append,(_1219,_1220,_1221),lists:append(_1219,_1220,_1221))))
%
% X = append,
% Y = [1,2] ? 
%
% yes
% ?- X = append([1]), X([2],Y).
% normal_unify_head_args(pred_expansion(append([1]),2,user(/Users/jfran/Documents/svn/ciao/ciao/library/toplevel/toplevel__scope),-,compile,PA(append([1]),(_1222,_1223),lists:append(_1222,[1],_1223))))
%
% X = append([1]),
% Y = [2,1] ? 
%
% yes
% ?- X = append([1],[2]), X(Y).
% normal_unify_head_args(pred_expansion(append([1],[2]),1,user(/Users/jfran/Documents/svn/ciao/ciao/library/toplevel/toplevel__scope),-,compile,PA(append([1],[2]),(_1219),lists:append(_1219,[1],[2]))))
%
% no
%
% ===========================================================================
%
% % library(swi_hooks) compiled into pce_swi_hooks 0.00 sec, 3,688 bytes
% Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 5.8.3)
% Copyright (c) 1990-2009 University of Amsterdam.
% SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
% and you are welcome to redistribute it under certain conditions.
% Please visit http://www.swi-prolog.org for details.
%
% For help, use ?- help(Topic). or ?- apropos(Word).
%
% ?- X = append, call(X,[1],[2],Y).
% X = append,
% Y = [1, 2].
%
% ?- X = append([1]), call(X,[2],Y).
% X = append([1]),
% Y = [1, 2].
%
% ?- X = append([1],[2]), call(X,Y).
% X = append([1], [2]),
% Y = [1, 2].

:- use_module(library(lists)).

main :-
	display('Partial Application test'), nl,
	display('(all tests must give the same results)'), nl,
	test1,
	test2,
	test2b,
	test3,
	test4,
	test5,
	test6.

test1 :-
	display('Test 1: 0+3'), nl,
	X = append,
	X([1], [2], Z), !,
	display(pa_test1(Z)), nl.
test1 :-
	display('(failed)'), nl.

test2 :-
	display('Test 2: 0+1+2'), nl,
	X = append,
	Y = X([1]),
	Y([2], Z), !,
	display(pa_test2(Z)), nl.
test2 :-
	display('(failed)'), nl.

test2b :-
	display('Test 2b: 1+2'), nl,
	Y = append([1]),
	Y([2], Z), !,
	display(pa_test2b(Z)), nl.
test2b :-
	display('(failed)'), nl.

test3 :-
	display('Test 3: 2+1'), nl,
	Y = append([1],[2]),
	Y(Z), !,
	display(pa_test3(Z)), nl.
test3 :-
	display('(failed)'), nl.

test4 :-
	display('Test 4: 3+0'), nl,
	Y = append([1],[2],Z),
	Y, !,
	display(pa_test4(Z)), nl.
test4 :-
	display('(failed)'), nl.

test5 :-
	display('Test 5: 0+1+1+1'), nl,
	X = append,
	Y = X([1]),
	Z = Y([2]),
	Z(W), !,
	display(pa_test5(W)), nl.
test5 :-
	display('(failed)'), nl.

test6 :-
	display('Test 6: 0+1+1+1+0'), nl,
	X = append,
	Y = X([1]),
	Z = Y([2]),
	W = Z(A),
	W, !,
	display(pa_test5(A)), nl.
test6 :-
	display('(failed)'), nl.
