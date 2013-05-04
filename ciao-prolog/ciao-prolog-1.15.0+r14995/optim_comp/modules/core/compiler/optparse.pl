% An command-line argument parser

:- use_module(library(lists)).

% TODO: Merge with other implementations in Ciao

% TODO: This should be a class
:- class optparse {
    :- '$statemodel'(single).
    :- '$raw_state'.

    :- static set_args/1.
    set_args(Args) :-
        assertz_fact(optparse__args(Args)).
    :- static get_args/1.
    get_args := Args :-
        current_fact(optparse__args(Args)).

    :- static set_optval/2.
    set_optval(Opt, Val) :-
        assertz_fact(optparse__optval(Opt, Val)).
    :- static get_optval/2.
    get_optval(Opt) := Val :-
        current_fact(optparse__optval(Opt, Val)).

    :- static set_opttype/2.
    set_opttype(Opt, Type) :-
        assertz_fact(optparse__opttype(Opt, Type)).
    :- static get_opttype/2.
    get_opttype(Opt) := Type :-
        current_fact(optparse__opttype(Opt, Type)).

    :- static set_opt/1.
    set_opt(Opt) :-
        assertz_fact(optparse__opt(Opt)).
    :- static get_opt/1.
    get_opt(Opt) :-
        current_fact(optparse__opt(Opt)).

    :- static parse_args/1.
    parse_args([]).
    parse_args(Xs) :-
	( Xs = [X|Xs0],
	  atom_codes(X, Codes),
	  parse_opt(Codes, OptCodes, MaybeValueCodes) ->
	    treat_opt(OptCodes, MaybeValueCodes),
	    parse_args(Xs0)
	; % Finish, all options parsed
	  set_args(Xs)
	).

    :- static treat_opt/2.
    treat_opt(OptCodes0, MaybeValueCodes) :-
        OptCodes = ~norm_opt_codes(OptCodes0),
	atom_codes(Opt, OptCodes),
	( get_opttype(Opt, Type) ->
	    true
	; bad_opt(Opt)
	),
	( Type = atom, MaybeValueCodes = yes(ValueCodes) ->
	    atom_codes(Value, ValueCodes),
	    set_optval(Opt, Value)
	; Type = number, MaybeValueCodes = yes(ValueCodes) ->
	    number_codes(Value, ValueCodes),
	    set_optval(Opt, Value)
	; Type = fact, MaybeValueCodes = no ->
	    set_opt(Opt)
	; bad_opt_value(Opt, MaybeValueCodes)
	).

    :- static norm_opt_codes/2.
    norm_opt_codes([]) := [].
    norm_opt_codes([X0|Xs]) := [X| ~norm_opt_codes(Xs)] :-
        X = ( X0 = 0'- ? 0'_ | X0 ).

    :- static bad_opt/1.
    bad_opt(Opt) :-
        display(user_error, 'Bad option `'),
	display(user_error, Opt),
        display(user_error, '\''),
	nl(user_error),
	fail.

    :- static bad_opt_value/2.
    bad_opt_value(Opt, _MaybeValueCodes) :-
        display(user_error, 'Bad value for option `'),
	display(user_error, Opt),
        display(user_error, '\''),
	nl(user_error),
	fail.
}.

parse_opt(ArgCodes, OptCodes, MaybeValueCodes) :-
	append("--", OptValueCodes, ArgCodes),
	( append(OptCodes, "="||ValueCodes, OptValueCodes) ->
	    MaybeValueCodes = yes(ValueCodes)
	; OptValueCodes = OptCodes,
	  MaybeValueCodes = no
	).
        
:- data optparse__args/1.
:- data optparse__opttype/2.
:- data optparse__optval/2.
:- data optparse__opt/1.

