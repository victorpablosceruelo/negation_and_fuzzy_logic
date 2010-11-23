:- use_package(rfuzzy).
:- use_package(clpr).
:- use_module(engine(hiord_rt)).
:- use_module(library(aggregates)).
%----------------------------------------------------------
% CRISP FUNCTIONS:
%-----------------------------------------------------------
equalto(X,X).
greaterthan(X,Y):- X .>. Y.
lessthan(X,Y):- X .<. Y.
fraction(X):- X .>=. 0 , X .=<. 1 .

get(A,X,Y):- attributes(L), nth(N,L,A),getNth(N,X,Y).
getNth(1,X,X):- house(X,_,_,_,_,_,_).
getNth(2,X,Y):- house(X,Y,_,_,_,_,_).
getNth(3,X,Y):- house(X,_,Y,_,_,_,_).
getNth(4,X,Y):- house(X,_,_,Y,_,_,_).
getNth(5,X,Y):- house(X,_,_,_,Y,_,_).
getNth(6,X,Y):- house(X,_,_,_,_,Y,_).
getNth(7,X,Y):- house(X,_,_,_,_,_,Y).

%% arg(1,house(lfs2168,'apartment',114,5,630000,2,5700),A).

% QUALIFIERS------------------------------------------------
%-----------------------------------------------------------
:- set_prop very_func/1 => fraction/1.
:- default(very_func/1,0.5).
very_func :# ([(0,0),(0.5,0),(0.8,0.8),(1,1)]).

:- set_prop little_func/1 => fraction/1.
:- default(little_func/1,0.5).
little_func :# ([(0,1),(0.1,1),(0.4,0),(1,0)]).

:- set_prop not_func/1 => fraction/1.
:- default(not_func/1,0.5).
not_func :# ([(0,1),(1,0)]).

:- set_prop id_func/1 => fraction/1.
:- default(id_func/1,0.5).
id_func :# ([(0,0),(1,1)]).


%QUALIFIERS AS METAPREDICATES---------------------------------------------
%-------------------------------------------------------------------------
%when a single qualification is needed.....like VERY EXPENSIVE
%Example: very(expensive,X,Y).
very(Pred,X,Z):- call(Pred,X,Y),very_func(Y,Z).
little(Pred,X,Z):- call(Pred,X,Y),little_func(Y,Z).
not(Pred,X,Z):- call(Pred,X,Y),not_func(Y,Z).
id(Pred,X,Z):- call(Pred,X,Z). 
%---------------------------------------------------------------------------
%when 2 level qualification is needed...like NOT VERY EXPENSIVE
%Example: solve(not,very,expensive,X,Y).
solve(Q1,Q2,Pred,X,A):- call(Pred,X,Y),q(Q2,Y,Z),q(Q1,Z,A).
q(not,X,Y):- not_func(X,Y).
q(little,X,Y):- little_func(X,Y).
q(very,X,Y):- very_func(X,Y).
q(id,X,Y):- id_func(X,Y).
%---------------------------------------------------------------------------
%---------------------------------------------------------------------------