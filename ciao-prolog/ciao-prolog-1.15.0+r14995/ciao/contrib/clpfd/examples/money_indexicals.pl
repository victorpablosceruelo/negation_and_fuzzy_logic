:- module(money_indexicals, _).

:- use_module(library(clpfd(clpfd_rt))).
:- use_module(library(clpfd(clpfd_idx))).

:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format)).

mm([S,E,N,D,M,O,R,Y]) :-
	statistics(runtime,_),
	domain([S,E,N,D,M,O,R,Y], 0, 9),
	't<b'(0,S), 't<b'(0,M),
	all_different([S,E,N,D,M,O,R,Y]),
	psum(S,E,N,D,M,O,R,Y),
	labeling([], [S,E,N,D,M,O,R,Y]),
	statistics(runtime,[_, Time]),
	format("Used ~d milliseconds~n", [Time]).

% psum(S,E,N,D,M,O,R,Y) :-
% 	      1000*S + 100*E + 10*N + D
%  +            1000*M + 100*O + 10*R + E
%  #= 10000*M + 1000*O + 100*N + 10*E + Y.

psum(S,E,N,D,M,O,R,Y) :-
	'a=b*t'(A1,S,1000),
	'a=b*t'(A2,E,100),
	'a=b*t'(A3,N,10),
	'a+b=c'(A3,D,B1),
	'a+b=c'(B1,A2,B2),
	'a+b=c'(B2,A1,C1),

 	'a=b*t'(M1,M,1000),
 	'a=b*t'(M2,O,100),
 	'a=b*t'(M3,R,10),
 	'a+b=c'(M3,E,N1),
 	'a+b=c'(N1,M2,N2),
 	'a+b=c'(N2,M1,D1),
 	'a+b=c'(C1,D1,R1),

 	'a=b*t'(Q0,M,10000),
 	'a=b*t'(Q1,O,1000),
 	'a=b*t'(Q2,N,100),
 	'a=b*t'(Q3,E,10),
 	'a+b=c'(Q3,Y,U1),
 	'a+b=c'(U1,Q2,U2),
 	'a+b=c'(U2,Q1,U3),
 	'a+b=c'(U3,Q0,R2),

 	'a=b'(R1,R2). 

%% Example query.
%% mm([S,E,N,D,M,O,R,Y], [])
