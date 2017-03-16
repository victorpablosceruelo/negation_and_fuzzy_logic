:- module(gvars_res,
	    [
		close_gvars_list/1,
		insert_gvars_field/4,
		find_gvars_field/4
	    ], [assertions, nortchecks, resources(inferres_decl)]).

%
%  gvars.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for manipulating the ground
%  variables list.
%
%  The structure of the ground variables list:
%	gv(Var,pos)
%

:- use_module(resources(top_res(utility_res)), [close_list/1]).


%
%  Insert an entry for variable Var in the ground variables list.
%  If it is already in, return the entry as Entry;
%  otherwise, an entry Entry for Var is inserted and returned.
%
insert_gvars_entry(Gvars, Var, Entry) :-
	var(Gvars),
	!,
	Entry = gv(Var, _, _, _, _, _, _),
	Gvars = [Entry|_].
insert_gvars_entry(Gvars, Var, Entry) :-
	nonvar(Gvars),
	Gvars = [E|_],
	E = gv(VAR, _, _, _, _, _, _),
	Var == VAR,
	Entry = E,
	!.
insert_gvars_entry(Gvars, Var, Entry) :-
	nonvar(Gvars),
	Gvars = [E|G],
	E = gv(VAR, _, _, _, _, _, _),
	Var \== VAR,
	insert_gvars_entry(G, Var, Entry).

%
%  Insert a bound position for variable Var into the ground variables list.
%
insert_gvars_field(Gvars, Var, Type, Pos) :-
	insert_gvars_field_(Type, Gvars, Var, Pos).

insert_gvars_field_(def, Gvars, Var, Pos) :-
	insert_gvars_entry(Gvars, Var, gv(Var, PosList, _, _, _, _, _)),
	insert_gvars_pos(PosList, Pos).
insert_gvars_field_(use, Gvars, Var, Pos) :-
	insert_gvars_entry(Gvars, Var, gv(Var, _, PosList, _, _, _, _)),
	insert_gvars_pos(PosList, Pos).
insert_gvars_field_(relation, Gvars, Var, Sol) :-
	insert_gvars_entry(Gvars, Var, gv(Var, _, _, Sol, _, _, _)).
insert_gvars_field_(det, Gvars, Var, Sol) :-
	insert_gvars_entry(Gvars, Var, gv(Var, _, _, _, Sol, _, _)).
insert_gvars_field_(redge, Gvars, Var, Edge) :-
	insert_gvars_entry(Gvars, Var, gv(Var, _, _, _, _, Edge, _)).
insert_gvars_field_(sedge, Gvars, Var, Edge) :-
	insert_gvars_entry(Gvars, Var, gv(Var, _, _, _, _, _, Edge)).

insert_gvars_pos(PosList, Pos) :-
	var(PosList),
	!,
	PosList = [Pos|_].
insert_gvars_pos(PosList, Pos) :-
	nonvar(PosList),
	PosList = [Pos|_],
	!.
insert_gvars_pos(PosList, Pos) :-
	nonvar(PosList),
	PosList = [P|PList],
	P \== Pos,
	insert_gvars_pos(PList, Pos).

%
%  Find the entry for variable Var in the ground variables list.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%
find_gvars_entry(Gvars, _, _) :-
	var(Gvars),
	!.
find_gvars_entry(Gvars, Var, Entry) :-
	nonvar(Gvars),
	Gvars = [E|G],
	E = gv(VAR, _, _, _, _, _, _),
	(
	    Var == VAR ->
	    Entry = E
	;
	    find_gvars_entry(G, Var, Entry)
	).

%
%  Find a field for variable Var into the ground variables list.
%
find_gvars_field(Gvars, Var, Type, PosList) :-
	find_gvars_field_(Type, Gvars, Var, PosList).

find_gvars_field_(def, Gvars, Var, PosList) :-
	find_gvars_entry(Gvars, Var, gv(Var, PosList, _, _, _, _, _)).
find_gvars_field_(use, Gvars, Var, PosList) :-
	find_gvars_entry(Gvars, Var, gv(Var, _, PosList, _, _, _, _)).
find_gvars_field_(relation, Gvars, Var, Sol) :-
	find_gvars_entry(Gvars, Var, gv(Var, _, _, Sol, _, _, _)).
find_gvars_field_(det, Gvars, Var, Sol) :-
	find_gvars_entry(Gvars, Var, gv(Var, _, _, _, Sol, _, _)).
find_gvars_field_(redge, Gvars, Var, Edge) :-
	find_gvars_entry(Gvars, Var, gv(Var, _, _, _, _, Edge, _)).
find_gvars_field_(sedge, Gvars, Var, Edge) :-
	find_gvars_entry(Gvars, Var, gv(Var, _, _, _, _, _, Edge)).

%
%  Close the postion lists of the ground variable list.
%
:- pred close_gvars_list/1 + (is_det, not_fails).
close_gvars_list(Gvars) :-
	var(Gvars),
	!.
close_gvars_list(Gvars) :-
	nonvar(Gvars),
	Gvars = [gv(_, Def, Use, _, _, _, _)|G],
	close_list(Def),
	close_list(Use),
	close_gvars_list(G).

/* PBC: not used
%
%  Print out the ground variables list.
%
	print_gvars_list( Gvars ) :-
	tell( gvars_list ),
	p_gvars_list( Gvars ),
	told.

p_gvars_list( [] ).
p_gvars_list( [ E|Gvars ] ) :-
	write( E ),
	nl,
	p_gvars_list( Gvars ).
*/
