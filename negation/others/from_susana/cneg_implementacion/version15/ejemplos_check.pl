?- check_is_true(formula(X,[]),[]).

yes
?- check_is_true(formula(X,[[X/[s(0),s(s(0)),s(0)],
                             X/[s(s(0)),s(0),s(0)],
                             X/[s(s(0)),s(0),s(fA(Y))],
                             X/[s(0),s(s(0)),s(0)]]]),[]).

yes

Esto es lo mismo que:
[[ X/[1,2,1],
   X/[2,1,1],
   X/[2,1,s(fA(Y))],
   X/[1,2,1]]]

Que tiene que fallar.
?- appears_fA(X/s(fA(Y))).

yes
?- check_is_true(formula(X,[[X/s(fA(Y))]]),[]).

no
?- check_is_true(formula(X,[[X/[s(0),s(s(0)),s(0)],X/[s(s(0)),s(0),s(0)],X/[s(s(0)),s(0),s(fA(Y))],X/[s(0),s(s(0)),s(0)]]]),[]).

no
