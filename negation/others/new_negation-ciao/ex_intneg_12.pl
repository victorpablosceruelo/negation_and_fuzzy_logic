
:- module('ex_intneg_12', _).

a :- c, intneg(b).
b :- intneg(a).
c.

p :- q, intneg(r).
q :- r, intneg(s).
q :- p.
r :- q.
