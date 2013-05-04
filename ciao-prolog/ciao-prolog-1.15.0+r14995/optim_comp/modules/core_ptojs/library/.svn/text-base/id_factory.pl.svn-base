:- module(id_factory, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).
:- doc(title, "A factory of unique identifiers").

% TODO: Only for the JavaScript backend? (at least the "id"+ part)

:- use_module(library(arithpreds)).

% TODO: use ":- mutattr c :: num"?
:- export(c/1).
:- attr c.

:- export(cons__/0).
cons__ :- ~c = ~mutables_rt.nb_mut_num(0).

:- export(new_id/1).
new_id := Id :-
        Id = "id" + ~((@(~c)).'$to_str'),
	~c += 1.
