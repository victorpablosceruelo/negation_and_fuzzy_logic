
:- module(m,[m/1,p/1,r/1],[assertions,regtypes]).

:- use_module(n).

:- entry m(X) : var(X).

:- success m(X) => ground(X).
:- calls m(X) : ground(X).

m(X):- n(X).
m(X):- mm(X).

:- success mm(X) => ground(X).

mm(_).

:- pred n(X) : ground(X) => ground(X).
:- calls n(X) : ground(X).

n(X):- check(ground(X)).
n(X):- nnn(X).

:- entry p(X) : t(X).

:- success p(X) => num(X).
:- calls p(X) : num(X).

p(b):- pp.
p(X):- q(g(X)).
p(X):- q(X).

pp.

:- pred q(X) : num(X) => num(X).
:- calls q(X) : num(X).
:- success q(X) => arithexpression(X).

q(_).
q(b):- q(_).

:- entry r(X) : t(X).

r(a):- s(3).
r(X):- ss(X,Y), s(Y).
r(X):- sss(X), sss(3).
r(X):- ss(X,Y), sss(Y).
r(X):- nn(X).
r(X):- ss(X,Y), nn(Y).
r(X):- ss(X,Y), X<Y.
r(X):- check(num(X)).
r(X):- ss(X,Y), check(num(Y)).

:- calls s(X) : num(X).

s(3).

ss(a,b).
ss(f(X),g(Y)):- ss(X,Y).

:- success sss(X) => num(X).

sss(_).

:- regtype t/1.

t(a).
t(f(X)):- t(X).

:- true pred check(X) + native(check(X)).
:- impl_defined(check/1).
