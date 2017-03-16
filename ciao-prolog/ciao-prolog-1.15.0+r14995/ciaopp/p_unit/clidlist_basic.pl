:- module(clidlist_basic, [lit_ppkey/3, clid_of_atomid/2],
	    [assertions, basicmodes]).

:- use_module(library(lists)).
:- use_module(program(clidtypes)).

:- doc(bug, "This structure is too complex to be handled easily,
	it mixes several formats... Why not to use only one like the
	one defined by the first clause?  Tip: To define it, look for the
	predicate rewrite_source_body/5 in clidlist.pl. -- EMM").

:- pred lit_ppkey/3 :: literal_ppkey * callable * bodykey.

lit_ppkey(Lit:PPKey,  Lit, PPKey) :- !.
lit_ppkey(!,          !,   noinfo) :- !.
lit_ppkey(builtin(B), B,   noinfo) :- !.
lit_ppkey(Lit,        Lit, noinfo) :- warn_nokey(Lit).

warn_nokey(Lit) :- warning(['This literal: ', Lit, ' does not have a key']).

:- pred clid_of_atomid(+AtomId, -ClId) :: atm * atm

# "Returns in @var{ClId} the clause identifier corresponding to
  @var{AtomId} goal identifier.".

clid_of_atomid(GoalKey, ClauseKey) :-
	atom_codes(GoalKey, Gs),
	append(Cs, [0'/|GoalId], Gs),
	number_codes(_, GoalId),
	atom_codes(ClauseKey, Cs).
