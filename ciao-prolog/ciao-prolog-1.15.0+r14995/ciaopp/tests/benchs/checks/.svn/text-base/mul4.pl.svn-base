:- module(mul4, [switch/2], [assertions, regtypes]). 

% the assertion becomes checked with multi_success=on
% and pred_ctchecks set to new_succ or new_all_succ

:- success switch(A,B):t_off(A) => t_on(B).
:- success switch(A,B):t_on(A) => t_off(B).


switch(on,off).
switch(off,on).



:- regtype t_on/1.

t_on(on).

:- regtype t_off/1.

t_off(off).
