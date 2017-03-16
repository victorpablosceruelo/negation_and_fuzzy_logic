%:- module(proof_looking,_,[show_trans, .(cneg)]). % para verlo expandido
:- module(proof_looking,_,[.(cneg)]).

:- use_module(dist). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

restaurant(a).
restaurant(b).

expensive(a).
 
chose1(X):- restaurant(X), cneg(expensive(X)).

chose2(X):- cneg(expensive(X)), restaurant(X).

chose3(X):- cneg(expensive(X)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(X) :- q(X).

% q(a) V q(b) = ¬q(a) -> q(b) /\  ¬q(b) -> q(a) 

q(a) :- cneg(q(b)).

q(b) :- cneg(q(a)).

or(q(a),q(b)) :- cneg(q(b)).

or(q(a),q(b)) :- cneg(q(a)).

q(a) :- or(q(a),q(b)), cneg(q(b)).
q(b) :- or(q(a),q(b)), cneg(q(a)).


% ? q(Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

