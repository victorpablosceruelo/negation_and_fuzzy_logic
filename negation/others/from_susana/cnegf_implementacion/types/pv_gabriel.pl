%% Note: the code needs revision.

:- module(pv_gabriel, [main/2], [assertions]).

% :- use_module(library(lists), [list/2, list1/2]).

:- entry main(A,B): (var(A), var(B)).

main(V1,V2) :-
   %% Commented -PL init(100,10,4,V1,Symbols),
   init5(2,1,1,V1,Symbols), 
   randomize(Symbols,RSymbols,21),
   investigate(RSymbols,V2).


init5(N,M,Npats,Ipats,Result) :- init(N,M,M,Npats,Ipats,Result).

init(0,_,_,_,_,_).
init(N,I,M,Npats,Ipats,[Symb|Rest]) :- 
   fill(I,[],L),
   get_pats3(Npats,Ipats,Ppats),
   J is M - I,
   fill(J,[pattern(Ppats)|L],Symb),
   N1 is N - 1,
        test(I,I1,M),
   init(N1,I1,M,Npats,Ipats,Rest).

test(I,I1,M) :-
  I = 0,
  I1 is M.
test(I,I1,_M) :-
   I1 is I - 1.

fill(0,L,L).
fill(N,L,[dummy([])|Rest]) :- N1 is N - 1, fill(N1,L,Rest).


randomize([],[],_).
randomize(In,[X|Out],Rand) :-
   % pv_length(In,Lin),
   pv_length2(In,_Lin),
   Rand1 is Rand * 17,
   N is Rand1,
   split(N,In,X,In1),
   randomize(In1,Out,Rand1).

split(0,[X|Xs],X,Xs).
split(N,[X|Xs],RemovedElt,[X|Ys]) :-
   N1 is N - 1,
   split(N1,Xs,RemovedElt,Ys).


investigate([],_).
investigate([U|Units],Patterns) :-
   property(U,pattern,Data),
   p_investigate(Data,Patterns),
   investigate(Units,Patterns).


get_pats3(Npats,Ipats,Result) :- get_pats(Npats,Ipats,Result,Ipats).

get_pats(0,_,[],_).
get_pats(N,[X|Xs],[X|Ys],Ipats) :-
   N1 is N - 1,
   get_pats(N1,Xs,Ys,Ipats).
get_pats(N,[],Ys,Ipats) :-
   get_pats(N,Ipats,Ys,Ipats).

property([Prop|_RProps],P,Val) :-
   pv_functor(Prop,P,_),
   pv_arg(1,Prop,Val).
property([_|RProps],P,Val) :-
   property(RProps,P,Val).

% Warning! -PL

 pv_functor(f(a,b),f,[a,b]).
 pv_functor([a|X],f,[a,X]).
 pv_functor(start(X),star,[X]).
 pv_functor(pattern(X),pattern,[X]).

% Warning! -PL

 pv_arg(1,f(X,_Y),X).
 pv_arg(1,[X|_Y],X).
 pv_arg(1,g(X,_Y,_Z),X).
 pv_arg(1,pattern(X),X).

p_investigate([],_).
p_investigate([D|Data],Patterns) :-
   p_match(Patterns,D),
   p_investigate(Data,Patterns).

p_match([],_).
p_match([P|_Patterns],D) :-
   match(D,P). 
      % Commented -PL   a = b.
p_match([_P|Patterns],D) :-
   p_match(Patterns, D).

match([],[]).
match([X|PRest],[Y|SRest]) :-
   X = Y,
   match(PRest,SRest).
match(List,[Y|Rest]) :- 
   Y = star(X),
   concat(X,SRest,List),
   match(SRest,Rest).
match([X|PRest],[Y|SRest]) :-
   pv_atom(X),
   X = Y,
   match(PRest,SRest).
match([X|PRest],[Y|SRest]) :-
        match(X,Y),
   match(PRest,SRest).
% Warning! -PL
 pv_atom(a).
 pv_atom(b).

concat([],L,L).
concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).

pv_length2(List,L) :- pv_length(List,0,L).

pv_length([],L,L).
pv_length([_X|Xs],N,L) :- N1 is N + 1, pv_length(Xs,N1,L).


%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

