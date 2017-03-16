 % I think (according to the cardinality paper - Pascal V. Hentenryck)
 % that this could be Queens benchmark of table 2.

:- module(pv_queen, [queens/2], [assertions]).

:- entry queens(A, B) : num * var.

%% These 2 predicates have been added by -PL.

queens(N, Qs):- range(1, N, Ns), perm(Ns, Qs), safe(Qs).

range(N, N, [N]):-!.
range(M, N, [M|Ns]) :- M1 is  M+1, range(M1, N, Ns).

% range(M, N, [M|Ns]) :- M < N, M1 is  M+1, range(M1, N, Ns).

 % Original 
 %% queens(X,Y) :-
 %%    perm(X,Y),
 %%    safe(Y).

perm([],[]).
perm([X|Y],[V|Res]) :-
  delete(V,[X|Y],Rest),  % Warning! note the use of [X|Y]. 
  perm(Rest,Res).

delete(X,[X|Y],Y).
delete(X,[F|T],[F|R]) :-
  delete(X,T,R).

safe([]).
safe([X|Y]) :-
  noattack(X,Y,1),
  safe(Y).

noattack(_X,[],_N).
noattack(X,[F|T],N) :-
   X =\= F,
   X =\= F + N,
   F =\= X + N,
   N1 is N + 1,
   noattack(X,T,N1).
