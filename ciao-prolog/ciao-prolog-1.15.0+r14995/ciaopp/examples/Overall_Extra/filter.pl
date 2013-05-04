:- module(_,[main/1],[assertions,regtypes]).

:- use_module(library(write)).

%% Use pptypesfd
%% Describing external predicates for which the type analyzer has no info:
:- trust pred print(X) => atom(X).
:- trust pred get_code(X) => int(X).
:- trust pred put_code(X) => int(X).

main(X) :-
	print('Options'),nl,
	pls(X),
	print('Starting to Filter'),nl,
	filter.

filter :- 
	get_code(X), 
	X>=0,!,
	(filt(X,Y) ->  put_code(Y) ; true),
	 filter.
filter :- 
	nl,
	print('---'),
	nl.

filt(13,10).
filt(32,_) :- !,fail.
filt(X,X).

pls([]).
pls([H|T]) :- write(H), nl, pls(T).
