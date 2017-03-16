:- module(ex_intneg_03, _, [.(intneg)]). 
% use [.(intneg)] for local calls.
% :- module('ex_intneg_03', _).


% Constructive predicates arity 1.
p_01(0).
p_01(s(X)) :- p_01(X).

p_02(X) :- p_01(X) ; p_01(s(X)).
p_03(X) :- p_01(X) , p_02(s(X)).

% Constructive predicates arity 2 and 3.
p_11(X,Y) :- p_01(X), p_02(Y).
p_12(X,Y,Z) :- p_03(X), p_03(Y), p_03(Z).

% Constructive predicates arity 0.
p_21.
p_21 :- p_01(0).
p_21 :- p_01(1), p_02(1), p_03(1).

% Constructive predicates with ovs clauses.
p_31.
p_31 :- p_01(0).
p_31 :- p_01(1), p_02(1), p_03(1).
p_31 :- p_01(_X).
p_31 :- p_01(X), p_01(Y), p_11(X, Y).

p_32(X) :- p_01(X).
p_32(X) :- p_02(X).

p_33(0, X) :- p_01(X).
p_33(X, 0) :- p_01(X).
p_33(X, Y) :- p_01(X), p_01(Y).

p_34(_X, _Y, _Z).
p_34(_X, _Y, _Z) :- p_01(0).
p_34( X, _Y, _Z) :- p_01(X).
p_34( X,  Y,  Z) :- p_01(X), p_01(Y), p_11(X, Y), p_01(Z).

% Constructive predicates with Exists -> Universal
p_41(X) :- p_33(X,_Z).

% Non constructive predicates
mbr1(X, l(X, _L)).
mbr1(X, l(_Y, L)) :- mbr1(X, L).

mbr2(X, [_Y| L]) :- mbr2(X, L).
mbr2(X, [X|_L]).

p_51 :- p_61(0).
p_61(X) :- mbr2(X, [_Y, _Z, _T]).
% p_62(X) :- mbr2(X, [_Y, _Z, _T]), intneg(mbr2(X, [_Y, _Z, _T])).

% 

% WFS.
p_101(X) :- p_101(X).

p_111(X) :- p_101(X).
p_112(X) :- intneg(p_101(X)).


