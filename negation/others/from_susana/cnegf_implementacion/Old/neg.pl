:- module(neg, [neg/1],[]).

% To be able to call a goal from neg
:- meta_predicate
        neg(goal).

% neg(Pred) return the negation of calling goal Pred

% Negation of negation is to call the positive goal
neg(Meta):- \+ Meta.
