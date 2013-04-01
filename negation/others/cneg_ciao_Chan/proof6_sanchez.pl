
:- module(proof6_sanchez,_,[.(cneg)]).

:- use_module(dist). 

% Example from Bossi, Fabris & Meo
p(f(V),Y):- q(V).
p(X,g(W)):- cneg(r(W)).
q(a).
r(b):- r(b).
r(c):- r(c).
r(c).

% cneg(p(f(a),Y) success
% cneg(p(a,c)) success
% cneg(r(c)) fails
% cneg(r(b)) loops infinitely
% cneg(r(X)) X/b,X/c; and then loops
% cneg(p(X,Y)) four answers (the last 2 are repeated infinitely)
% cneg(q(Z)) returns Z/a
% cneg(p(g(Z),f(Z))),q(Z) retuns Z=a
% p(X,g(Y)), q(Y) two answers X=f(a),Y=a and Y=a
% cneg(p(X,Y)),r((Y) returns 2 answers (infinitely) X/f(_),Y=c and X=f(_A), Y=c,_A/a
