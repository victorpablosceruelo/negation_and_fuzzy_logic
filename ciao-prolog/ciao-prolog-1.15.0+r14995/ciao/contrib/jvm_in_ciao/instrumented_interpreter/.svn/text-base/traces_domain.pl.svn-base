:- trust comp alpha_initial_state/1 + (eval,sideff(free)).
alpha_initial_state(ASi) :- ASi = [].

:- trust comp alpha_step(_,_,AS1,_) : ground(AS1) + (eval,sideff(free)).
% The second argument provides an optional context. Here, it takes the trace step
alpha_step(_,TraceStep,AS1,[TraceStep|AS1]).

%alpha_add(A,B,C) :- append_g(A,B,C).
alpha_add(A,B,[B|A]).
