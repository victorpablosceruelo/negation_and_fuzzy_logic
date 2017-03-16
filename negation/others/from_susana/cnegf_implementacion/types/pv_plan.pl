:- module(pv_plan, [testplan/2], [assertions]).

% :- use_module(library(lists), [list/2, list1/2]).

:- entry testplan(A,B): (var(A), var(B)).

testplan(Name,Plan) :-
  initialstate(Name,I),
  finalstate(Name,F),
  transform3(I,F,Plan).

transform3(State1,State2,Plan) :-
   transform(State1,State2,[State1],Plan).

transform(State,State,_Visited,[]).
transform(State1,State2,Visited,[Action|Actions]) :-
   chooseaction(Action,State1,State2),
   update(Action,State1,State),
   notmember(State,Visited),
   transform(State,State2,[State|Visited],Actions).

chooseaction(Action,State1,State2):-
  suggest(Action,State2), 
  legalaction(Action,State1).
chooseaction(Action,State1,_State2):-
  legalaction(Action,State1).

suggest(toplace(X,_Y,Z),State) :-
   ismember(on(X,Z),State), 
   place(Z).
suggest(toblock(X,_Y,Z),State) :-
   ismember(on(X,Z),State),
   pv_block(Z).

legalaction(toplace(Block,Y,Place),State) :-
   on(Block,Y,State),
   clear(Block,State),
   place(Place),
   clear(Place,State).

legalaction(toblock(Block1,Y,Block2),State) :-
   on(Block1,Y,State),
   clear(Block1,State),
   pv_block(Block2),
   \+ Block1 = Block2,
   clear(Block2,State).

clear(X,State) :-
   pv_block(A),
   notmember(on(A,X),State).
on(X,Y,State) :-
   ismember(on(X,Y),State).

update(toblock(X,Y,Z),State,State1) :-
   substitute(on(X,Y),on(X,Z),State,State1).
update(toplace(X,Y,Z),State,State1) :-
   substitute(on(X,Y),on(X,Z),State,State1).

ismember(X,[X|_Y]).
ismember(X,[_F|T]) :-
   ismember(X,T).

notmember(_X,[]).
notmember(X,[F|T]) :-
   \+ X = F,
   notmember(X,T).

substitute(_X,_Y,[],[]).
substitute(X,Y,[X|T],[Y|Ts]) :-
  substitute(X,Y,T,Ts).
substitute(X,Y,[F|T],[F|Ts]) :-
  \+ X = F,
  substitute(X,Y,T,Ts).


initialstate(test,[on(a,b),on(b,p),on(c,r)]).
finalstate(test,[on(a,b),on(b,c),on(c,r)]).

pv_block(a).
pv_block(b).
pv_block(c).

place(p).
place(q).
place(r).






%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

