/*
   Look in clause_order_wrong_output.pl for the bug-report comments

?- module(clause_order), analyze(pd), transform(arg_filtering).

  - original order of directives is not preserved
  - assertions normalized from source assertions do not preserve variable names

Besides, 

?- module(clause_order), transform(arg_filtering).
{Loading current module from .../bugs/clause_order/clause_order.pl
{loaded in 1250.81 msec.}
}
{Transforming .../bugs/clause_order/clause_order.pl

no
?- 

simply fails.
*/

:- module(_,[p/1],[assertions]).

p(X):- r(a,X), q(X).

r(X,X).

:- data q/1.
:- pred q/1 : ground.

q(a).
q(b).
