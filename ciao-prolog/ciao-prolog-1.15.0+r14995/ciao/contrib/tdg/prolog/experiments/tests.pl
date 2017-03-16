:- module(tests,_).

foo(in(X),out(Y)) :- X > 0,Y = pos.
foo(in(X),out(Y)) :- X = 0,Y = zero.

%% app([],L,L).
%% app([X|R],L,[X|RL]) :- app(R,L,RL).
%% 
%% nrev([],[]).
%% nrev([X|R],L) :- nrev(R,Rr),app(Rr,[X],L).
%% 
%% rev(L,Lr) :- rev_(L,[],Lr).
%% rev_([],Ac,Ac).
%% rev_([X|R],Ac,Lr) :- rev_(R,[X|Ac],Lr).
%% 
memb(out(X),in(L)) :- L = [X|_].
memb(out(X),in(L)) :- L = [_|R],memb(out(X),in(R)).
%% 
%% memb(in(X),in(L)) :- L = [X|_].
%% memb(in(X),in(L)) :- L = [_|R],memb(in(X),in(R)).

%% 
gat(in(L),in(N),out(X)) :- memb(out(X),in(L)),N < X.

