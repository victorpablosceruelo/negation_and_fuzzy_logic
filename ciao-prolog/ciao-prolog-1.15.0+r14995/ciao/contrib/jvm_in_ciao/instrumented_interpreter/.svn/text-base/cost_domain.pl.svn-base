:- trust comp alpha_initial_state/1 + (eval,sideff(free)).
alpha_initial_state(ASi) :- ASi is 0.

:- trust comp alpha_step(_,_,AS1,_) : ground(AS1) + (eval,sideff(free)).
% The second argument provides an optional context. Here it is not used
alpha_step(_,_,AS1,AS2) :- AS2 is AS1 + 1.

alpha_add(A,B,C) :- C is A + B.
