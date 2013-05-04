:- use_package([]).
:- use_module(engine(attributes)).

%% fails
p0(X, A):- get_attribute(X, A).

%% Attach succeeds
p1(X, A):- attach_attribute(X, prime), get_attribute(X, A).

%% Changes in the variable identity
p2(X):- display(X), nl, attach_attribute(X, prime), display(X), nl.

%% Attach and detach
p3(X):- attach_attribute(X, prime), detach_attribute(X).

%% Attach and detach, with variable changes
p4(X):- 
        display(X), nl,
        attach_attribute(X, prime),
        display(X), nl,
        detach_attribute(X),
        display(X), nl.

%% Updating a variable's attribute
p5(X, A1, A2):-
        attach_attribute(X, prime), get_attribute(X, A1),
        update_attribute(X, promo), get_attribute(X, A2),
        detach_attribute(X).
