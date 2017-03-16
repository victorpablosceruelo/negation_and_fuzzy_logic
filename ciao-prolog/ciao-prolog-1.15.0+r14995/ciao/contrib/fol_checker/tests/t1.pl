% Example 1 (propositional Horn clauses)
:- module(t1, [p/0,q/0], [fol_checker]).

p :- q ; r.
r.
q.

% TODO: not implemented, but this is trivial
:- prove p.
