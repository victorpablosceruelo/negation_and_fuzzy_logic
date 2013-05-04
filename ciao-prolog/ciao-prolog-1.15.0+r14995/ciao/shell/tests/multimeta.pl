:- module(multimeta, [p/2], []).

:- multifile mm/1.
:- data mm/1.
:- meta_predicate mm(includemodule).

mm(p/2).
mm(r/3).

p(a,b).
