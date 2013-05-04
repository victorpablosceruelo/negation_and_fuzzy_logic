:- module(zebra_instrumented, [zebra/7], [assertions, nativeprops, regtypes,
		predefres(res_steps)]).

:- use_module(library(format)).
:- use_module(library(prolog_sys)).

:- entry zebra(A, B, C, D, E, F, G)
	: ( mshare([[A], [B], [C], [D], [E], [F], [G]]),
	    var(A), var(B), var(C), var(D), var(E), var(F),
	    var(G) ).

:- use_module(library(counters)).

zebra(Englishman, Spaniard, Japanese, Ukrainian, Norwegian, Zebra, Water) :-
	inccounter(zebra, _),
	Englishman = Red,
	Spaniard = Dog,
	Green = Coffee,
	Ukrainian = Tea,
	to_the_right(Green, Ivory),
	Winston = Snails,
	Kool = Yellow,
	Milk = third,
	Norwegian = first,
	next_to(Fox,   Chesterfield),
	next_to(Horse, Kool),
	Lucky = Juice,
	Japanese = Parliament,
	next_to(Norwegian, Blue),
	houses([Blue,      Green,      Red,      Yellow,   Ivory]),
	houses([Norwegian, Englishman, Spaniard, Japanese, Ukrainian]),
%% 	more(Dog,Zebra,Fox,Snails,Horse,
%% 	     Parliament,Kool,Lucky,Chesterfield,Winston,
%% 	     Milk,Juice,Water,Tea,Coffee).
%% 
%% 	more(Dog,Zebra,Fox,Snails,Horse,
%% 	     Parliament,Kool,Lucky,Chesterfield,Winston,
%% 	     Milk,Juice,Water,Tea,Coffee):-
	houses([Dog,        Zebra, Fox,   Snails,       Horse]),
	houses([Parliament, Kool,  Lucky, Chesterfield, Winston]),
	houses([Milk,       Juice, Water, Tea,          Coffee]).

houses(Prop) :-
	inccounter(zebra, _),
	domain(Prop, [first, second, third, fourth, fifth]).

domain([], _) :-
	inccounter(zebra, _).
domain([X|Rest], Domain) :-
	inccounter(zebra, _),
	select(X, Domain, NewDomain),
	domain(Rest, NewDomain).

select(X, [X|R], R) :-
	inccounter(zebra, _).
select(X, [Y|R], [Y|Rest]) :-
	inccounter(zebra, _),
	select(X, R, Rest).

next_to(fifth, fourth) :-
	inccounter(zebra, _).
next_to(fourth, fifth) :-
	inccounter(zebra, _).
next_to(fourth, third) :-
	inccounter(zebra, _).
next_to(third, fourth) :-
	inccounter(zebra, _).
next_to(third, second) :-
	inccounter(zebra, _).
next_to(second, third) :-
	inccounter(zebra, _).
next_to(second, first) :-
	inccounter(zebra, _).
next_to(first, second) :-
	inccounter(zebra, _).

to_the_right(fifth, fourth) :-
	inccounter(zebra, _).
to_the_right(fourth, third) :-
	inccounter(zebra, _).
to_the_right(third, second) :-
	inccounter(zebra, _).
to_the_right(second, first) :-
	inccounter(zebra, _).

%----------------------------------------------------------------------------
:- export(demo/0).
demo :-
	format('Zebra problem ~n~n', []),
	time(_, E),
	zebra(E, S, J, U, N, Z, W),
	time(T, S),
	write_sol(E, S, J, U, N, Z, W),
	format('Solved in ~w msec. ~n', [T]).

time(Time, _) :- statistics(runtime, [_, Time]).

write_sol(E, S, J, U, N, Z, W) :-
	person(Z, E, S, J, U, N, Owner),
	person(W, E, S, J, U, N, Drinker),
	format('The Zebra is owned by the ~w. ~nThe ~w drinks water.~n',
	    [Owner, Drinker]).

person(O, O, _, _, _, _, 'Englishman').
person(O, _, O, _, _, _, 'Spaniard').
person(O, _, _, O, _, _, 'Japanese').
person(O, _, _, _, O, _, 'Ukrainian').
person(O, _, _, _, _, O, 'Norwegian').
