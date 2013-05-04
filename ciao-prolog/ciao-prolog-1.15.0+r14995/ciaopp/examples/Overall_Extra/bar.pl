:- use_package(fsyntax).

main(X) :-
%% 	q(a,T1),
%% 	X = T1.
	X = ^(q(a)).

:- fun_eval q/1.

q(a) := b.

:- set_prolog_flag(multi_arity_warnings,off).

p(X) :- display(p(X)),nl.
p(X,Y) :- display(p(X,Y)),nl.
p(X,Y,Z) :- display(p(X,Y,Z)),nl.
p(X,Y,Z,W) :- display(p(X,Y,Z,W)),nl.
p(X,Y,Z,W,K) :- display(p(X,Y,Z,W,K)),nl.
p(X,Y,Z,W,K,L) :- display(p(X,Y,Z,W,K,L)),nl.

%% ?- use_package(hiord).
%% {Including /home/clip/lib/ciao/ciao-1.9/lib/hiord.pl
%% Note: module hiord_rt already in executable, just made visible
%% }
%% 
%% yes
%% ?- call(p,1,2,3,4,5,6).
%% p(1,2,3,4,5,6)
%% 
%% yes
%% ?- call(p(1),2,3,4,5,6).
%% p(2,1,3,4,5,6)
%% 
%% yes
%% ?- call(p(1,2),3,4,5,6).
%% p(3,1,2,4,5,6)
%% 
%% yes
%% ?- call(p(1,2,3),4,5,6).
%% p(4,1,2,3,5,6)
%% 
%% yes
%% ?- call(p(1,2,3,4),5,6).
%% p(5,1,2,3,4,6)
%% 
%% yes
%% ?- call(p(1,2,3,4,5),6).
%% p(6,1,2,3,4,5)
%% 
%% yes
%% ?- call(p(1,2,3,4,5,6)).
%% p(1,2,3,4,5,6)
%% 
%% yes
%% ?- 

%% q(a,b).

%% 
%% listab([]).
%% listab([a|Y]) :- listab(Y).
%% listab([b|Y]) :- listb(Y).
%% 
%% listb([]).
%% listb([b|Y]) :- listb(Y).
