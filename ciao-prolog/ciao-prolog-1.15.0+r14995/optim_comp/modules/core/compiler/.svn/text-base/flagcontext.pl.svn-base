:- module(flagcontext, [], [compiler(complang)]).

:- use_module(library(operators)).
:- use_module(library(aggregates), [findall/3]).

% ---------------------------------------------------------------------------
% Statically defined compilation (related) options (operators and flags)

:- public flagcontext__do/1.
flagcontext__do(push_prolog_flag(Flag, Value)) :-
        ( push_prolog_flag(Flag, Value) ->
	    flagcontext__add(push_prolog_flag(Flag,Value),
	                  pop_prolog_flag(Flag))
        ; fail
        ).
flagcontext__do(pop_prolog_flag(Flag)) :-
        ( current_prolog_flag(Flag, Value),
          pop_prolog_flag(Flag) ->
	    flagcontext__add(pop_prolog_flag(Flag),
	                  push_prolog_flag(Flag,Value))
        ; fail
        ).
flagcontext__do(set_prolog_flag(Flag, Value)) :-
        ( prolog_flag(Flag, Old, Value) ->
	    flagcontext__add(set_prolog_flag(Flag,Value),
	                  set_prolog_flag(Flag,Old))
        ; fail
        ).
flagcontext__do(op(Prec, F, Ops)) :-
        % note: this can give errors
        ensure_op_undone(Prec, F, Ops),
        op(Prec, F, Ops).

ensure_op_undone(Prec, F, Ops) :-
        integer(Prec), 0=<Prec, Prec=<1200,
        nonvar(F),
        op_type(F, T),
        atom_or_atom_list(Ops), !,
        ensure_ops_undone(Ops, F, T, Prec).
ensure_op_undone(_, _, _). % do not fail to give errors

ensure_ops_undone([Op|Ops], F, T, Prec) :- !,
        ensure_ops_undone(Op, F, T, Prec),
        ensure_ops_undone(Ops, F, T, Prec).
ensure_ops_undone([], _, _, _) :- !.
ensure_ops_undone(Op, F, T, Prec) :-
        ( current_op(CPrec, CF, Op), op_type(CF, T) ->
            true
        ; CPrec = 0, CF = F
        ),
	flagcontext__add(op(Prec,F,Op), op(CPrec,CF,Op)).

atom_or_atom_list(A) :- atom(A), !.
atom_or_atom_list([A|L]) :-
        atom(A),
        atom_or_atom_list(L).

op_type(fy, pre).
op_type(fx, pre).
op_type(yfx, in).
op_type(xfy, in).
op_type(xfx, in).
op_type(yf, post).
op_type(xf, post).

:- data flagcontext__item/2.

:- meta_predicate flagcontext__add(goal, goal).
flagcontext__add(Redo, Undo) :-
	asserta_fact(flagcontext__item(Redo, Undo)).

% TODO: optimize
% enter a fresh flag context and save old settings
:- public flagcontext__new/1.
flagcontext__new(_) :-
        flagcontext__item(_, UndoGoal),
	  '$trust_metatype'(UndoGoal, goal),
          call(UndoGoal),
        fail.
flagcontext__new(Gs) :-
        findall(i(Redo, Undo), flagcontext__item(Redo, Undo), Gs),
	retractall_fact(flagcontext__item(_, _)).

% restore old settings
:- public flagcontext__switch/1.
flagcontext__switch(Gs) :-
        flagcontext__new(_),
        call_list_rev(Gs).

call_list_rev([]).
call_list_rev([i(Redo, Undo)|Gs]) :-
        call_list_rev(Gs),
	asserta_fact(flagcontext__item(Redo, Undo)),
	'$trust_metatype'(Redo, goal),
        ( call(Redo), fail ; true ).
