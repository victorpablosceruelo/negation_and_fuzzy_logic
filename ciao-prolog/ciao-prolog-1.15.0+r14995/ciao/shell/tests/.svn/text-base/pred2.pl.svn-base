
descending(Arg1,Arg2,Arg3) :-
        =(Pred, ((X, Y):- X > Y)), Pred(Arg1, Arg2), Pred(Arg2, Arg3).

:- meta_predicate(is_pred2(pred(2),pred(2))).

is_pred2(X,X).


:- meta_predicate transitive_closure(pred(2), ?, ?).

transitive_closure(Pred, X, Y) :- Pred(X,Y).
transitive_closure(Pred, X, Y) :- Pred(X,Z), transitive_closure(Pred, Z, Y).

l(a,b).
l(b,c).
l(b,e).
l(x,y).

p(Pred) :- is_pred2(((X,Y) :- X>Y), Pred).
