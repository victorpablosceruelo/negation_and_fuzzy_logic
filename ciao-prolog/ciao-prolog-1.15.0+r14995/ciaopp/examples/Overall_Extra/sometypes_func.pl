:- module(_,_,[assertions,fsyntax,regtypes]).

:- regtype color/1. 

color := red ; green ; blue.

:- regtype natlist/1. 

natlist := [] ; [~nat|~natlist].

:- regtype nat/1. 

nat := 0 ; s(~nat).
