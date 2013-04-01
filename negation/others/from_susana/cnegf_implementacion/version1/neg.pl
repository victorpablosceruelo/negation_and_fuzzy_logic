:- module(neg, [neg/1,cnegf/1],[]).

% To be able to call a goal from neg
:- meta_predicate
        neg(goal).
:- meta_predicate
        cnegf(goal).

% neg(Pred) return the negation of calling goal Pred

% Negation of negation is to call the positive goal
neg(Meta):- \+ Meta.

cnegf(Meta):- \+ Meta.
