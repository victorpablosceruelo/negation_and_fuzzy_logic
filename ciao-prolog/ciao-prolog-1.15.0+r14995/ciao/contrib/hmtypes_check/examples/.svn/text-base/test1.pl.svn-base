:- module(test1, [p/1, mem/2], [assertions, hiord, hmtypes_check]).

:- runtime_hm_type_check(on).

:- prop color/1 + hmtype.
color(red). color(green). color(blue).

% TODO: What should happen here? 
%:- prop warmcolor/1 + hmtype.
%warmcolor(red).

:- pred creverse/2 :: color * color + hmtyped.
creverse(red, blue).
creverse(green, green).
creverse(blue, red).

:- pred greater_than_1/2 :: integer * boolean + hmtyped.
%:- hm_pred greater_than_1(integer, boolean).
greater_than_1(X, R) :- ( X > 1 -> R = true ; R = false ).

:- pred p/1 :: list(integer) + hmtyped.
%:- hm_pred p(list(integer)).
p([]).
p([1,2,3|Y]) :- p(Y).

:- pred mem/2 :: call(E) * list(E) + hmtyped.
%:- hm_pred mem(E,list(E)).
% mem/2's first argument is of any one type E and mem/2's argument is
% of type list(E).
mem(X,[X|_]).
mem(X,[_|T]) :- mem(X,T).
