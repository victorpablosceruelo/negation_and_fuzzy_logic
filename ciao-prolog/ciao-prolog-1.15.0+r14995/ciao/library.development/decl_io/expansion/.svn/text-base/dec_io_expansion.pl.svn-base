:- module(dec_io_expansion, [dec_io_translation/3], [assertions]).

:- use_module(library(lists), [append/3, list_concat/2]).
:- use_module(library(terms), [copy_args/3]).
:- use_module(library(write), [portray_clause/2]).

:- data translated_source_stream/1.

% FIXME Tune LPdoc settings to obtain the operator definitions
% without putting them inside this file.

:- pred dec_io_translation(+InputClause, -OutputClause, +Module) => nonvar(OutputClause) # "Translates @var{InputClause} into @{OutputClause}".

dec_io_translation(C1, C2, M) :-
	dec_io_translate(C1, C2, M),
	% TODO Remove as soon as code gets 'release' status
	translated_source_stream(Stream),
	portray_clause(Stream, C2).

:- pred dec_io_translate(+InputClause, -OutputClause, +Module) => nonvar(Outputclause) # "Translates @var{InputClause} into @var{OutputClause}.".

% Compile-time directives are not processed
dec_io_translate((:-B), (:-B), _) :- !.

dec_io_translate((H->>B), (H1:-B1), _) :-
	!,
	% We use Tail to know if some goals in the body of the rule have been chained
	translate_rule_body(B, I, O, Tail, B1),
	( Tail \== O -> warn_no_chain(H), H1 = H
	; translate_goal(H, I, O, H1)).

dec_io_translate((H:-B), (H:-B1), _) :-
	!,
	translate_rule_body(B, _, _, _, B1).

dec_io_translate(C1, C2, M) :-
	functor(C1, C1Name, C1Arity),
	dec_io_translate_functor(C1, C1Name, C1Arity, C2, M).

dec_io_translate_functor(C, 0, 0, C, M) :-
	get_translation_filepath(M, AtomFilename),
	open(AtomFilename, write, Stream),
	asserta_fact(translated_source_stream(Stream)).

dec_io_translate_functor(C, end_of_file, 0, C, _) :-
	translated_source_stream(Stream),
	close(Stream),
	retract_fact(translated_source_stream(Stream)).

dec_io_translate_functor(C, _, _, C, _).

get_translation_filepath(M, AtomFilename) :-
	(M = user(Atom); M = Atom),
	 atom_codes(Atom, AtomName),
	 append(AtomName, "_translated.pl", Filename),
	 atom_codes(AtomFilename, Filename).

translate_goal(Term, I, O, NewTerm) :-
	functor(Term, TermName, TermArity),
	A1 is TermArity + 1,
	A2 is TermArity + 2,
	functor(NewTerm, TermName, A2),
	copy_args(TermArity, Term, NewTerm),
	arg(A1, NewTerm, I),
	arg(A2, NewTerm, O).

translate_rule_body((X, Y), I, O, Tail, (X1, Y1)) :-
	!,
	translate_rule_body(X, I, O1, T1, X1),
	translate_rule_body(Y, O1, O, T2, Y1),
	( T1 \== O1, T2 \== O -> true
	; Tail = O
	).

translate_rule_body((X; Y), I, O, Tail, (X1; Y1)) :-
	!,
	translate_rule_body(X, I, O, TX1, X1),
	translate_rule_body(Y, I, O, TX2, Y1),
	(TX1 == O -> Tail = TX1; TX2 == O -> Tail = TX2; true).

translate_rule_body(X, I, O, Tail, X1) :-
	( functor(X, dec, 1) -> arg(1, X, NewGoal), translate_goal(NewGoal, I, O, X1), Tail = O
	; X1 = X
	).

warn_no_chain(ClauseHead) :-
	functor(ClauseHead, Name, Arity),
	atom_codes(Name, NameString),
	number_codes(Arity, ArityString),
	list_concat(["No predicates in the body of a rule of predicate ", NameString, "/", ArityString, " are tagged with dec/1"], Message),
	warning($$(Message)).
