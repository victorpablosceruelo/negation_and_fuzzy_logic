%:- module(fol_checker_tr, [sent_tr/3], [assertions]).
:- module(fol_checker_tr, _, [assertions, indexer]).

:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sets), [ord_subtract/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(aggregates), [findall/3]).

% For error messages
:- use_module(library(messages)).
:- use_module(library(compiler(c_itf_internal)), [location/3]).

% ---------------------------------------------------------------------------

:- doc(title, "Verifier of Prolog programs through FOL theorem provers.").
:- doc(author, "Jose F. Morales").

% Translate the program to first-order form (FOF).
% The translation depends on the chosen semantics.
% The axioms for a program must always be consistent.
fofify_program(S, M) :-
	% Use Clark's completion for each formula
	( % (failure-driven loop)
	  is_pred(F, N, Count, M),
	    cwa_fof(M, F, N, Count, FOF_Clause),
	    tptp_clause(S, FOF_Clause),
	    fail
	; true
	).

fofify_assertion(S, M, Asrt, Count) :-
	( asrt_fof(M, Asrt, Count, FOF_Clause) ->
	    tptp_clause(S, FOF_Clause)
	; error("Could not translate assertion `~w'", [Asrt]),
	  fail
	).

% TODO: Few cases treated in this translation
asrt_fof(_M, Asrt, Count, FOF_Clause) :-
	Asrt = prove(Formula),
	Formula = '=>'(A, B),
	varset(Formula, Vars),
	conj_to_list(A, A2),
	conj_to_list(B, B2),
	Formula2 = '\6\forall'(Vars, '=>'('\6\and'(A2), '\6\and'(B2))),
	FOF_Clause = fof(conjecture(Count), conjecture, Formula2).

% All the clauses of a given predicate 
pred_clauses(M, F, N, Cs) :-
	functor(Head, F, N),
	findall(clause(Head,Body), clause(Head,Body,M), Cs).

% Obtain the FOF axiom for the predicate, using Clark's completion
% (CWA, closed-world assumption; for each predicate).
cwa_fof(M, F, N, Count, FOF_Clause) :-
	pred_clauses(M, F, N, Cs),
	cwa_fof_(Cs, Count, FOF_Clause).

cwa_fof_(Cs, Count, FOF_Clause) :-
	flat_heads(Cs, Cs2),
	join_bodies(Cs2, Head, Body),
	% Obtain quantifiers:
	%  - universally quantify head vars
	%  - existential quantify body vars not in the head
	% e.g., given "p(X) :- q(X,Y)."
        %   HVars = [X]
        %   BNotVars = [Y]
	varset(Head, HVars),
	varset(Body, BVars),
	ord_subtract(BVars, HVars, BNotHVars),
	%
	Formula = '\6\forall'(HVars, '<=>'(Head, '\6\some'(BNotHVars, Body))),
	FOF_Clause = fof(axiom(Count), axiom, Formula).

% Join all bodies and unify all heads (which has been flattened).
join_bodies(Cs, Head, '\6\or'(Body)) :-
	join_bodies_(Cs, Head, Body).

join_bodies_([], _, []).
join_bodies_([clause(Head,Body)|Cs], Head, ['\6\and'(Body)|Bodies]) :-
	join_bodies_(Cs, Head, Bodies).

% ---------------------------------------------------------------------------
% Flatten head arguments

:- use_module(library(dict)).

flat_heads([], []).
flat_heads([clause(Head,Body)|Cs], [clause(Head2,Body2)|Cs2]) :-
	flat_head(Head, Head2, Unifs),
	append(Unifs, Body, Body2),
	flat_heads(Cs, Cs2).

flat_head(Head, Head2, Unifs) :-
	Head =.. [N|As],
	Seen = _, % empty dictionary
	flat_head_(As, Seen, As2, Unifs),
	Head2 =.. [N|As2].

flat_head_([], _, [], []).
flat_head_([A|As], Seen, [A2|As2], Unifs) :-
	( var(A) ->
	    % A variable, add as unif only if it appeared before
	    ( dic_get(Seen, A, _) ->
	        Unifs = [A2 = A|Unifs0]
	    ; Unifs = Unifs0,
	      A2 = A,
	      dic_lookup(Seen, A, yes)
	    )
	; Unifs = [A2 = A|Unifs0]
	),
	flat_head_(As, Seen, As2, Unifs0).

% ===========================================================================

:- doc(section, "The program and assertion database (minimal)").

% ---------------------------------------------------------------------------
% Counters

:- data count/3.

% Increment count 'Kind'
inc_count(M, Kind, C) :-
	( retract_fact(count(Kind,M,C0)) ->
	    true
	; C0 = 0
	),
	C is C0 + 1,
	assertz_fact(count(Kind,M,C)).

% ---------------------------------------------------------------------------
% Clauses and assertions

:- data is_pred/4.
:- data clause/3.
:- data assrt/3.

% ---------------------------------------------------------------------------

% Clean the database
init_db(M) :-
	retractall_fact(count(_,M,_)),
	retractall_fact(is_pred(_,_,_,M)),
	retractall_fact(clause(_,_,M)),
	retractall_fact(assrt(_,_,M)).

% Add a new clause
add_clause(Head, Body0, M) :-
	mark_pred(Head, M),
	conj_to_list(Body0, Body),
	assertz_fact(clause(Head, Body, M)).

mark_pred(Head, M) :-
	functor(Head, F, N),
	( is_pred(F, N, _, M) ->
	    true
	; inc_count(M, pred, Count),
	  assertz_fact(is_pred(F, N, Count, M))
	).

% Add a new assertion
add_assrt(Assrt, M) :-
	inc_count(M, asrt, Count),
	assertz_fact(assrt(Assrt, Count, M)).

% ---------------------------------------------------------------------------

:- doc(section, "Sentence translations (package)").

sent_tr(0, [], M) :- !,
	% initialization
	init_db(M).
sent_tr(end_of_file, [], M) :- !,
	% initialization
	check_mod(M).
sent_tr(Sent, Sent, M) :- Sent = (:- Decl), !,
        ( Decl = prove(_) ->
	   add_assrt(Decl, M)
	; fail
	).
sent_tr(Sent, Sent, M) :-
	treat_clause(Sent, M).

treat_clause(Sent, M) :-
	norm_clause(Sent, Head, Body),
	add_clause(Head, Body, M).

norm_clause(Sent, Head, Body) :-
	( Sent = (Head :- Body) ->
	    true
	; Head = Sent, Body = true
	).

check_mod(M) :-
	% Assertions are checked one by one
	( % (failure-driven loop)
	  assrt(Asrt, Count, M),
	    check_asrt(M, Asrt, Count),
	    fail
	; true
	).

check_asrt(M, Asrt, Count) :-
	tptp_file(M, in, Count, In),
	open(In, write, S),
	fofify_program(S, M),
	fofify_assertion(S, M, Asrt, Count),
	close(S),
	invoke_eprove(M, Count),
	tptp_file(M, out, Count, Out),
	% TODO: read output
	simple_message("Proof for assertion ~w written in ~w", [Count, Out]).

% ---------------------------------------------------------------------------

:- doc(section, "Some helper predicates (formulae)").

% Conjunction to list
conj_to_list(A, Xs) :-
	conj_to_list_(A, Xs, []).

conj_to_list_(A, Xs, Xs0) :- var(A), !, Xs = Xs0.
conj_to_list_((A,B), Xs, Xs0) :- !,
	conj_to_list_(A, Xs, Xs1),
	conj_to_list_(B, Xs1, Xs0).
conj_to_list_(true, Xs, Xs0) :- !, Xs = Xs0.
conj_to_list_(A, [A|Xs], Xs).

% ---------------------------------------------------------------------------

:- doc(section, "Error reporting").

error(Msg, Args) :-
	current_location(Loc),
	error_message(Loc, Msg, Args).

current_location(Loc) :-
	location(S, L0, L1),
	Loc = loc(S, L0, L1),
	!.
current_location(_).

% ===========================================================================

:- doc(section, "Output in TPTP format").

% Write a clause in TPTP format
:- index tptp_clause(?,+).
tptp_clause(S, fof(Name, Type, Formula)) :-
	display(S, 'fof('),
	tptp_name(S, Name), display(S, ','),
	display(S, Type), display(S, ','),
	tptp_formula(S, Formula),
	display(S, ').'), nl(S).

:- index tptp_name(?,+).
tptp_name(S, axiom(Count)) :-
	display(S, a), display(S, '_'), display(S, Count).
tptp_name(S, conjecture(Count)) :-
	display(S, c), display(S, '_'), display(S, Count).

:- index tptp_formula(?,+).
tptp_formula(S, '<=>'(A,B)) :- !,
	display(S, '('),
	tptp_formula(S, A), display(S, '<=>'), tptp_formula(S, B),
	display(S, ')').
tptp_formula(S, '=>'(A,B)) :- !,
	display(S, '('),
	tptp_formula(S, A), display(S, '=>'), tptp_formula(S, B),
	display(S, ')').
tptp_formula(S, '\6\forall'([],A)) :- !,
	tptp_formula(S, A).
tptp_formula(S, '\6\forall'(Vars,A)) :- !,
	display(S, '(!'), display(S, Vars), display(S, ':'),
	tptp_formula(S, A), display(S, ')').
tptp_formula(S, '\6\some'([],A)) :- !,
	tptp_formula(S, A).
tptp_formula(S, '\6\some'(Vars,A)) :- !,
	display(S, '(?'), display(S, Vars), display(S, ':'),
	tptp_formula(S, A), display(S, ')').
tptp_formula(S, '\6\and'(Xs)) :- !,
	display(S, '('), tptp_and(S, Xs), display(S, ')').
tptp_formula(S, '\6\or'(Xs)) :- !,
	display(S, '('), tptp_or(S, Xs), display(S, ')').
tptp_formula(S, A=B) :- !,
	tptp_expr(S, A), display(S, '='), tptp_expr(S, B).
tptp_formula(S, Lit) :-
	tptp_lit(S, Lit).

:- index tptp_and(?,+).
tptp_and(S, [A]) :- !, tptp_formula(S, A).
tptp_and(S, [A|As]) :- tptp_formula(S, A), display(S, '&'), tptp_and(S, As).

:- index tptp_or(?,+).
tptp_or(S, [A]) :- !, tptp_formula(S, A).
tptp_or(S, [A|As]) :- tptp_formula(S, A), display(S, '|'), tptp_or(S, As).

:- index tptp_args(?,+).
tptp_args(S, [A]) :- !, tptp_expr(S, A).
tptp_args(S, [A|As]) :- tptp_expr(S, A), display(S, ','), tptp_args(S, As).

tptp_lit(S, X) :-
	X =.. [N|As],
	display(S, N),
	( As = [] -> true
	; display(S, '('), tptp_args(S, As), display(S, ')')
	).

tptp_expr(S, A) :- atom(A), !,
	% use distinguised elements
	% TODO: give interpretation of those functions as axioms instead?
	display(S, '"'), display(S, A), display(S, '"').
tptp_expr(S, A) :- display(S, A).

% ===========================================================================

:- doc(section, "Invoke external prover (TPTP)").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [system/1]).

tptp_file(M, Ver, Count, Name) :-
	number_codes(Count, Cs),
	atom_codes(C, Cs),
	atom_concat([M, '_fof_', Ver, '_', C, '.tptp'], Name).

invoke_eprove(M, Count) :-
	tptp_file(M, in, Count, In),
	tptp_file(M, out, Count, Out),
	% TODO: Limit to 3 seconds (we check trivial problems at this moment)
	atom_concat(['PATH=/usr/local/bin:$PATH eprover --cpu-limit=3 -xAuto -tAuto ',
	             '--tptp3-in ', In, ' --tptp3-out -o ', Out], Cmd),
	system(Cmd),
	% TODO: Read the output and check if a proof has been found or not.
	true.

% Just proof:
% eprover -xAuto -tAuto --tptp3-in lst.tptp --tptp3-out -o out
%
% With answer extraction:
% eprover -xAuto -tAuto --tptp3-in --answers=2 lst.tptp --tptp3-out -o out

