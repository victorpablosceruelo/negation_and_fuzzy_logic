:- module(clidtypes, [
		clause/1,
		clausebody/1,
		literal_ppkey/1,
% 		regtypes
		bodykey/1,
		clid/1,
		predid/1
	    ], [assertions, regtypes]).

:- doc(doinclude, bodykey/1).
:- regtype bodykey(Key)
# "@var{Key} is an atom that uniquely identifies a program point
	   of a program clause body literal.".

bodykey(Key) :- atm(Key).

:- doc(doinclude, clid/1).
:- regtype clid(Id)
# "@var{Id} is an atom that uniquely identifies a program clause.".
clid(Clid) :- atm(Clid).


:- doc(doinclude, predid/1).
:- regtype predid(Id)
# "@var{Id} is an atom that uniquely identifies a program predicate.".

predid(Clid) :- atm(Clid).


%----------------------------------------------------------------------------
% The type of the program clauses

:- regtype clause/1.
:- doc(clause/1, "Defined as: @includedef{clause/1}").

clause(clause(Head, Body) :Clid) :-
	callable(Head),
	clausebody(Body),
	clid(Clid).
clause(directive(Body) :Clid) :-
	clausebody(Body),
	drid(Clid).

:- doc(doinclude, drid/1).
:- regtype drid(Id)
# "@var{Id} is a number that uniquely identifies a program directive.".

drid(Clid) :- nnegint(Clid).

:- doc(doinclude, clausebody/1).
:- prop clausebody/1.
:- doc(clausebody(Body), "@var{Body} is a conjunction of simple goals;
	if a goal is a meta-predicate, its meta-arguments are terms of
	the form @tt{'$'(Term,Body,Type)}, where @tt{Term} is the original
	goal term, @tt{Type} the type of meta-term as in the meta_predicate
	directive, and, if @tt{Type} corresponds to an executable form of
	meta-term, @tt{Body} is the @tt{clausebody} that corresponds to
	@tt{Term}.
	@includedef{clausebody/1}").

clausebody(L) :-
	literal_ppkey(L).
clausebody((L, B)) :-
	literal_ppkey(L),
	clausebody(B).

:- regtype literal_ppkey/1.
literal_ppkey(Lit:PPKey) :-
	callable(Lit),
	bodykey(PPKey).
literal_ppkey(builtin(B) :noinfo) :-
	callable(B).
literal_ppkey(!).
