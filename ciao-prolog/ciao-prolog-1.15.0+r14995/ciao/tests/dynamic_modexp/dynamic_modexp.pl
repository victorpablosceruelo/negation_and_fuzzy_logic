% This module tests the interaction of the module system and the
% dynamic predicates.
%
% Note: The test may be incomplete, please improve with not included
% cases that may be interesting.
%
% Author: Jose F. Morales
% Date: Fri Jul 17 08:25:57 CEST 2009

:- module(dynamic_modexp, [main/0], []).

:- use_module(library(dynamic)).

:- dynamic foo/1.

main :-
    % Bug: data_fact using 'true' as body instead of 'basiccontrol:true'
    % Status: solved in r11198
    assertz_fact(foo(1)),
    assertz(foo(2)),
    display('You should see x(1) and x(2)'), nl,
    ( retract(foo(X)),
      display(x(X)), nl,
      fail
    ; true
    ),
    display('Done'), nl.

