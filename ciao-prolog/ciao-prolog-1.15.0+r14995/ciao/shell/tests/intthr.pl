:- use_package([]).

p(X) :- intercept(q(X), E, warn(E)).

q(f(X)) :- throw(u(X)), display(continuing), nl.

warn(u(X)) :- inform_user(['undefined ',X]).
