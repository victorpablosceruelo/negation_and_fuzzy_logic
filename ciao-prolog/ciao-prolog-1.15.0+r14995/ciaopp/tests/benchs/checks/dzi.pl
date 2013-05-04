:- module(dzi,  [p/1], [assertions, regtypes]). 

%% The example (adopted to ciaopp syntax) from Lunjin Lu: On Dart-Zobel 
%% Algorithm for Testing Regular Type Inclusion, ACM SIGPLAN Notices, 
%% vol 36(9), 2001 pp.81-85
%%
%% The example illustrates incorrectness of the original Dart-Zobel 
%% type inclusion algorithm. But Pedro's implementation is correct 
%% and works fine!

%% Problem:
%% alpha is not a subtype of beta, (e.g. g(h(h(a,b),a)) belongs to alpha
%% but not to beta) but D-Z algorithm shows that it is.

:-entry p(A): alpha(A).

:-check calls p(A): beta(A).

p(_).

%p(g(h(h(a,b),a))).


:-regtype alpha/1.
:-regtype omega/1.
:-regtype theta/1.
:-regtype beta/1.
:-regtype sigma/1.
:-regtype theta_or_sigma/1.

alpha(g(X)) :- omega(X). 

beta(g(X)) :- theta_or_sigma(X). 

theta_or_sigma(a).
theta_or_sigma(b).
theta_or_sigma(h(X,a)) :- theta(X).
thata_or_sigma(h(X,b)) :- sigma(X).


theta(a).
theta(h(X,a)) :- theta(X).

sigma(b).
sigma(h(X,b)) :- sigma(X).

omega(a).
omega(b).
omega(h(X,a)) :- omega(X).
omega(h(X,b)) :- omega(X).
	
