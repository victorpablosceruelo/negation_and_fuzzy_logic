
:- module(proof5_sanchez,_,[.(cneg)]).

:- use_module(dist). 

member1(X,[X|Ys]).
member1(X,[Y|Ys]):- member1(X,Ys).

% insert without repetitions
insert(X,Xs,[X|Xs]):- cneg(member1(X,Xs)).
insert(X,Xs,Xs):- member1(X,Xs).

% paths of a graph
next(a,b).
next(a,c).
next(b,c).
next(b,d).

path(X,X).
path(X,Y):- dist(X,Y), next(X,Z),path(Z,Y).

% save X if it is not conected to d
save(X):- cneg(path(X,d)).

% Collect all nodes next to a node of a graph
collectNext(X,L):- collectNextsAc(X,[],L).
collectNextsAc(X,Ac,L):- 
	next(X,Y), 
	cneg(member1(Y,Ac)), 
	collectNextsAc(X,[Y|Ac],L).
collectNextsAc(X,Ac,Ac):- cneg((next(X,Y),cneg(member1(Y,Ac)))).

% even and odd by using sum
sum(0,X,X).
sum(s(X),Y,s(Z)):- sum(X,Y,Z).

even(X):- sum(Y,Y,X).

%odd(X):- cneg(sum(Y,Y,X)). Bad quantification
%odd(X):- cneg(Exist X.sum(Y,Y,X)).
odd(X):- cneg(even(X)). % Also no numbers

% Example of Drabent
r:- cneg(p(X)),cneg(q(X)).
p(X):- p(X).
p(a).
q(a):-q(a).
q(X):- cneg(s(X)).
s(a).
w(f(X)).

% cneg(r) success % En el mio se pierden en un bucle, como Prolog
% r fails

% Example of Bartak 
p(a,f(Z)):- t(Z).
p(f(Z),b):- t(Z).
t(c).

% cneg(p(X,Y)) produces 5 answers

parent(ramira,susana).
parent(ezequiel,susana).
parent(damian,ramira).

ancestor(X,Y):- parent(X,Y).
ancestor(X,Y):- parent(X,Z), ancestor(Z,Y).

null(0).
sucessor(s(X)):- null(X).

number1(0).
number1(s(X)):- number1(X).
 
even1(X):- number1(X),sum(Y,Y,X).

odd1(X):- number1(X),cneg(even1(X)).
