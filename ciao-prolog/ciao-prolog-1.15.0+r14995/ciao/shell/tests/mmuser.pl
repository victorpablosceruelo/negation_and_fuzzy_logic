:- use_package([]).

:- use_module(multimeta).

:- multifile mm/1.
:- data mm/1.
:- meta_predicate mm(includemodule).

:- multifile mm/2.
:- data mm/2.

af :- asserta_fact(mm(a)).

pmm :- mm(X,Y), display(mm(X,Y)), fail.
