:- module(mm, [declare_dynamic/1, mm/1], []).

:- use_module(engine(internals)).

:- meta_predicate declare_dynamic(addmodule).
:- meta_predicate mm(addmodule).

declare_dynamic(F/A, Mod) :-
        mod_concat(Mod, F, MF),
        functor(Head, MF, A), !,
        asserta_fact(imports(Mod, Mod, F, A)),
	dynamic1(Head, declare_dynamic/2).

dynamic1(F, Goal) :-
	'$predicate_property'(F, _, Prop), !,
	(   Prop/\2 =:= 2 -> true		% dynamic, xref nondet.c
        ;   functor(F, N, A),
            throw(error(permision_error(modify, static_procedure, N/A), Goal))
	).
dynamic1(F, _) :-
	functor(F, Name, Ar),
	'$define_predicate'(Name/Ar, consult),
	'$set_property'(F, (dynamic)).		% xref indexing.c


mod_concat(builtin, A, A) :- !.
mod_concat(internals, A, A) :- !.
mod_concat(user(_), A, NA) :- !,
        mod_concat(user, A, NA).
mod_concat(M, A, NA) :-
	A=..[F|Args],
        atom(F), !,
	atom_concat(M, ':', Mc),
	atom_concat(Mc, F, NF),
	NA=..[NF|Args].
mod_concat(_, A, A). % If a number, do not change to complain later

mm(X, X).
