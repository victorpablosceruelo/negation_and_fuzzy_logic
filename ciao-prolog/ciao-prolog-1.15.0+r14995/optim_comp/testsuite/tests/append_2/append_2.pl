:- module(_, [], [fsyntax]).

:- include(engine(spec_arithmetic)).

:- '$pragma'(analyze_all).

:- export(l/1).
:- '$props'(l/1, [impnat = ptoc]).
l([]).
l([_|Xs]) :- l(Xs).

:- export(reverse/3).
:- '$props'(reverse/3, [impnat = ptoc]).
reverse([], Xs, Xs).
reverse([X|Xs], Ys, Zs) :-
	reverse(Xs, [X|Ys], Zs).

:- export(dummy/1).
:- '$props'(dummy/1, [impnat = ptoc]).
dummy(X) :-
	X = [A,_B],
	A = 3,
	Y = [C,_D],
	X = Y,
	X = [E,_G],
	E = A,
	C = 3.

:- export(dummy2/4).
% TODO: analysis should infer 1,2,1,2
:- '$trust_entry'(dummy2/4, sht, [var,var,var,var]).
:- '$props'(dummy2/4, [impnat = ptoc]).
dummy2(A,B,C,D) :-
	X = [A,B],
	Y = [C,D],
	X = Y,
	X = [1,2].

:- export(rec/0).
:- '$props'(rec/0, [impnat = ptoc]).
rec :- rec.

:- export(rec1/1).
:- '$props'(rec1/1, [impnat = ptoc]).
rec1(X) :- X > 0, X1 is X - 1, rec1(X1).

%:- export(rec/1).
%% TODO: analysis should infer 1,2,1,2
%:- '$props'(rec/1, [impnat = ptoc]).
%rec(_).
%rec(X) :- rec(X).

:- export(new_sample/3).
:- '$props'(new_sample/3, [impnat = ptoc]).
new_sample(X, Y, Z) :-
	X = [Y|Z],
	true.

:- export(tev/2).
:- '$props'(tev/2, [impnat = ptoc]).
tev(X, Y) :-
 	var(X), !,
 	var(X), % TODO: this must be simplified!!
%	smallint_dec(a, _Z), % TODO: show an error when this is found
 	Y = no.
tev(X, Y) :-
	number(X), !,
	number(X),
	nonvar(X),
	Y = no.
tev(_, Y) :-
	Y = yes.

%:- export(skip/6).
% TODO: add $external_entry to overwrite default entry for exports
:- '$trust_entry'(skip/6, sht, [smallint, any, any, any, any, var]).
:- '$props'(skip/6, [impnat = ptoc, imp = semidet]).
skip(Disp, _, L, _, NL, _) :-
	( Disp == 0 ->
            L = NL
        ; true
        ).

:- '$trust_entry'(sk2/6, sht, [smallint, any, any, any, any, var]).
:- '$props'(sk2/6, [impnat = ptoc, imp = semidet]).
sk2(Disp, _, L, _, NL, _) :-
	( display(Disp) ->
            L = NL
        ; true
        ).

% :- export(current_env/2).
% current_env(Name, _Value) :- \+ ( var(Name); atom(Name) ), !,
% 	throw(error(domain_error(var_or_atom,Name),current_env/2-1)).
% current_env(_Name, Value) :- \+ ( var(Value); atom(Value) ), !,
% 	throw(error(domain_error(var_or_atom,Value),current_env/2-2)).
% current_env(Name, Value) :-
% 	( atom(Name) ->
% 	    c_get_env(Name,Value)
% 	; current_env__2(0, Name, Value)
% 	).

% current_env__2(I, Name, Value) :-
% 	c_current_env(I, Name2, Value2),
% 	( Name=Name2, Value=Value2
% 	; J is I + 1,
% 	  current_env__2(J, Name, Value)
% 	).

% :- '$props'(c_get_env/2, [impnat=cbool(prolog_c_get_env)]).
% :- '$props'(c_current_env/3, [impnat=cbool(prolog_c_current_env)]).

% :- export(extract_paths/0).
% :- '$props'(extract_paths/0, [impnat = ptoc]).
% extract_paths :-
%         extract_path([1], _Cs).

% :- '$props'(extract_path/2, [impnat = ptoc]).
% extract_path(_, a).
% extract_path(_, B) :-
%         extract_path([], B).

