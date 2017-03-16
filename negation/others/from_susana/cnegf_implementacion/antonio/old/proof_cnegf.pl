:- module(proof_cnegf,_,[.(cnegf)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    EXAMPLES  FOR CNEGF                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- module(proof_cnegf, _,[.(cnegf)]).
%:- use_module(library(lists),[member/2]).
:- use_module(library(lists),[append/3]).
:- use_module(library(system)).
:- use_module(dist,[dist/2,eq/2]).
%:- use_module(neg,[neg/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Ejemplos para Negación Constructiva Cnegf.pl           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dist_list([]).
dist_list([(X,Y)|L]):-
	dist(X,Y),
	dist_list(L).
no_dist_list(X):- cnegf(dist_list(X)).


positive(0).
positive(s(X)):- positive(X).
no_positive(X):-cnegf(positive(X)).

boole(1).
boole(0).
no_boole(X):-cnegf(boole(X)).

number1(0).
number1(s(X)):-number1(X).

greater(s(X),0):-number1(X).
greater(s(X),s(Y)):- greater(X,Y).
no_greater(X,Y):-cnegf(greater(X,Y)).



cierto.
no_cierto:-cnegf(cierto).




t1(X,Y):- X=_Z,Y=_T.
no_t1(X,Y):-cnegf(t1(X,Y)).

t2(X,Y):- X=s(0),Y=s(1).
no_t2(X,Y):-cnegf(t2(X,Y)).

t3r(X,Y,_Z):- X=[Y,Z1],dist(Z1,1),dist(Y,2).
no_t3r(X,Y,Z):-cnegf(t3r(X,Y,Z)).

t3(X,Y,Z):- X=[Y,Z],dist(Z,1),dist(Y,2).
no_t3(X,Y,Z):-cnegf(t3(X,Y,Z)).

t31(X,Y,Z):- X=[Y,_Q],Z=_Q,Y=2.
no_t31(X,Y,Z):-cnegf(t31(X,Y,Z)).

t32(X,Y,Z):- X=[Y,_Q],Z=_Q,Y=_T.
no_t32(X,Y,Z):-cnegf(t32(X,Y,Z)).

t33(X,Y,Z,W,F,G):- X=[Y,Z],Y=[W,W],Z=2,dist(W,[F,G]),F=0,G=1.
no_t33(X,Y,Z,W,F,G):- cnegf(t33(X,Y,Z,W,F,G)).

t4(X,Z):-
	X=s(Y),
	Y=Z.
no_t4(X,Z):-cnegf(t4(X,Z)).

distinto(X,_Y,_Z):- X=0.
distinto(_X,Y,_Z):- dist(Y,2).
distinto(_X,_Y,Z):- dist(Z,3).
no_distinto(X,Y,Z):-cnegf(distinto(X,Y,Z)).	

no_es_digito(X):-
	member(Z,[0,1,2,3]),
	dist(X,Z).
es_digito(X):-cnegf(no_es_digito(X)).

concurso(X,_Y,_Z):-
	dist(X,1).
concurso(_X,Y,_Z):-
	dist(Y,2).
concurso(_X,_Y,3).
no_concurso(X,Y,Z):-cnegf(concurso(X,Y,Z)).

parent(adelaida,heidi).
parent(abuelito,adelaida).
parent(adelaida,pablo).
no_parent(X,Y):- cnegf(parent(X,Y)).

ancestor(X,Y):- 
	parent(X,Z),parent(Z,Y).
no_ancestor(X,Y):- cnegf(ancestor(X,Y)).

p(X,_Y,3,4):-
	dist(X,1),
        dist(X,2).
p(1,2,Z,W):-
	dist(Z,3),
	dist(W,4).

no_p(X,Y,Z,R):- cnegf(p(X,Y,Z,R)).

p1(X):- 
	dist(q(4,1),q(X,1)),
	dist(X,3).
no_p1(X):- cnegf(p1(X)).

p2(X,Y):- 
	dist(q(X,Y),q(Y,2)),  
	dist(X,3).    
no_p2(X,Y):- cnegf(p2(X,Y)).

p22(X,Y):- 
	dist(X,3),    
	dist(q(X,Y),q(1,2)).  
no_p22(X,Y):- cnegf(p22(X,Y)).

p3(X,Y,Z):-
	dist(Z,6),
	dist(q(X,Y),q(Z,5)).
no_p3(X,Y,Z):- cnegf(p3(X,Y,Z)).







