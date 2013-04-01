:- module(proof_cnegf, _,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    EXAMPLES  FOR CNEGF                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- module(proof_cnegf, _,[.(cnegf)]).
%:- use_module(library(lists),[member/2]).
:- use_module(library(lists),[append/3]).
:- use_module(dist,[dist/2,eq/2]).
%:- use_module(cneg,[cneg/1]).
:- use_module(cnegf,[cnegf/1]).
%:- use_module(neg,[neg/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Ejemplos para Negación Constructiva Cnegf.pl           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




t1(X,Y):- X=_Z,Y=_T.
no_t1(X,Y):-cnegf(t1(X,Y)).

t2(X,Y):- X=s(0),Y=s(1).
no_t2(X,Y):-cnegf(t2(X,Y)).

t3(X,Y,_Z):- X=[Y,Z1],dist(Z1,1),dist(Y,2).
no_t3(X,Y,Z):-cnegf(t3(X,Y,Z)).
% 16-11-2002 Ya FUNCIONA
% 16-10-2002 Revisar el detach attribute porque la Y debería mantener su atributo en la primera
% solucion

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

distinto(X):- dist(X,2).
no_distinto(X):-cnegf(distinto(X)).	

distinto1(X):- dist(X,2),X=1.
no_distinto1(X):-cnegf(distinto1(X)).	

distinto11(X,_Y,_Z):- X=0.
distinto11(_X,Y,_Z):- dist(Y,2).
distinto11(_X,_Y,Z):- dist(Z,3).
no_distinto11(X,Y,Z):-cnegf(distinto11(X,Y,Z)).	
% 16-11-2002 YA FUNCIONA
% 16-10-2002 repite soluciones por un tubo. Además da algunas que no debería. Es porque hay 
% variables libres que no sirven para nada pero las pilla como solucion.

distinto2(X,Y):-dist(Y,2),X=1.
no_distinto2(X,Y):- cnegf(distinto2(X,Y)).

no_es_digito(X):-
	member(Z,[0,1,2,3]),
	dist(X,Z).
es_digito(X):-cnegf(no_es_digito(X)).

% AHORA YA FUNCIONA 27-09-2002
%FALLA por que sale 
%?-   es_digito(X).

%X = 0 ? .

%X = 3 ? .

%X = 2 ? .

%X = 1 ? .
%
% y deberian salir como si fueran conjunciones no disyunciones. Es culpa del asiganar igualdades
% El problema es que el setof creo nuevas variables con un solo atributo. Y todas esas hay que
% igualarlas a X para que tenga los atributos de todas.

%%%%%%%%%%%%%%%%%%%%%%%%%% Tienen que ser conjunciones de los resultados y en
%%%%%%%%%%%%%%%%%%%%%%%%%% este caso como son igualdades incompatibles tiene
%%%%%%%%%%%%%%%%%%%%%%%%%% que dar fail

no_es_concurso(X,_Y):-
	dist(X,1).
no_es_concurso(_X,Y):-
	dist(Y,2).
es_concurso(X,Y):- cnegf(no_es_concurso(X,Y)).
% 16-11-2002 YA funciona
% 16-10-2002 Mismo fallo de soluciones repetidas, mejor dichoe erroneas.

concurso(X,_Y,_Z):-
	dist(X,1).
concurso(_X,Y,_Z):-
	dist(Y,2).
concurso(_X,_Y,3).
no_concurso(X,Y,Z):-cnegf(concurso(X,Y,Z)).
% 16-11-2002 YA funciona
% 16-10-2002 Mismo fallo de soluciones repetidas, mejor dichoe erroneas.

cero(0).
no_cero(X):-cnegf(cero(X)).

parent(adelaida,heidi).
parent(abuelito,adelaida).
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
%p(X,Y,Z,8):-
%	dist(Z,6),
%	dist(q(X,Y),q(Z,5)).
no_p(X,Y,Z,R):- cnegf(p(X,Y,Z,R)).
% 16-11-2002 Ya funciona.
% 16-10-2002 Mirar lo de los repetidos.
% YA FUNCIONA PERFECTO 27-09-2002 la solucion X=2 se corresponde con (X=2 y X/1) ya que
%                                 el X/1 es un atributo que se anula con la igualdad a 2
% NO FUNCIONA BIEN DEL TODO. Se ha ce un lio con las soluciones aunque las da relativamente bien


p1(X):- 
	dist(q(4,1),q(X,1)),
	dist(X,3).
no_p1(X):- cnegf(p1(X)).
% 16-11-2002 Ya funciona

p2(X,Y):- 
	dist(q(X,Y),q(Y,2)),  % caso mas sencillo en el que se ve el problema
	dist(X,3).    % No actualiza bien el atributo.Lo hace solo en la X
no_p2(X,Y):- cnegf(p2(X,Y)).
% 16-11-2002 Ya funciona
% 27-09-2002 revisar creo que no actualiza los atributas quietados al hacer el backtracking.
% Parece que tambien da la solucion un poco mal.

p22(X,Y):- 
	dist(X,3),    % No actualiza bien el atributo.Lo hace solo en la X
	dist(q(X,Y),q(1,2)).  % caso mas sencillo en el que se ve el problema
no_p22(X,Y):- cnegf(p22(X,Y)).
% 16-11-2002 Ya funciona
% 16-10-2002 Funciona.
% 27-09-2002 revisar creo que no actualiza los atributas quietados al hacer el backtracking.
% revisar solucion

p3(X,Y,Z):-
	dist(Z,6),
	dist(q(X,Y),q(Z,5)).
no_p3(X,Y,Z):- cnegf(p3(X,Y,Z)).
% 16-11-2002 La solucion Y=5 debería ser Y=5 y X=Z
% 16-10-2002 Funciona.
% 27-09-2002 revisar creo que no actualiza los atributas quietados al hacer el backtracking.
% revisar solucion

libres(1):- libres1(1,_Y).

no_libres(X):- cnegf(libres(X)).


libres1(_X,2).

positivos(s(_X)).
no_positivos(X):- cnegf(positivos(X)).
