:- use_package([]).

p(A,B) :- X is gcd(A,B), display(X).
