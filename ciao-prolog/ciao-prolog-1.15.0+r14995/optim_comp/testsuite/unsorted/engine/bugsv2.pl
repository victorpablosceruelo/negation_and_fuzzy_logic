:- module(_, _, []).

/*
:- use_module(library(prolog_sys), [statistics/0]).
:- use_module(engine(internals), [
        term_to_meta/2,
        '$compile_term'/2,'$current_clauses'/2,'$inserta'/2,'$insertz'/2,
        '$ptr_ref'/2,'$current_instance'/5,'$instance'/3,'$erase'/1,
        '$close_predicate'/1, '$open_predicate'/1, '$unlock_predicate'/1]).

my_asserta_fact(Fact) :-
        '$compile_term'([Fact|true], Ptr),
	'$current_clauses'(Fact, Root),
	'$inserta'(Root, Ptr).

my_retract_fact(Fact) :-
	'$current_clauses'(Fact, Root),
        '$current_instance'(Fact, true, Root, Ptr, block),
	'$erase'(Ptr).
*/

%:- concurrent i/0.
:- data i/0.

x(_,_,_,_,_,_,_,_).

main :-
        asserta_fact('i'),
        asserta_fact('i'),
	fail.
main :-
	X = n(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a),
	true,
	x(a,a,a,a,a,a,a,1.0),
	true,
	nonvar(X),
        fail.
main :-
	List = n(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a),
	X = 1.0,
	true,
        current_fact('i'),
	nonvar(X),
	nonvar(List),
        rs(0,_).

q(_).

rs(Num,[a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a|Vs]) :- Num < 60000, !,
	N2 is Num + 1,
        rs(N2,Vs).
rs(Num,[]) :- Num > 60000, !.
