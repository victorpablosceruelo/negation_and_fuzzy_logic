:- use_package([dcg]).

main :- a(T,[a,b],[]), display(T), nl.
a(X) --> [a,X].
