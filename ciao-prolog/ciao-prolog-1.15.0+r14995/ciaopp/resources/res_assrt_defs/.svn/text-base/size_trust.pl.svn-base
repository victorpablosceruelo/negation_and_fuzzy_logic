:- module(size_trust,
	    [apply_trusted_size/6,
		apply_trusted_size0/5
	    ],
	    [assertions, regtypes, basicmodes]).

:- doc(title,  "Trust assertions processing library for sizes").
:- doc(author, "Jorge Navas").
:- doc(module, " This module defines some predicates which process the
trust assertions for size property.").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(messages), [warning_message/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(ciaopp(p_unit(assrt_db)), [assertion_of/9]).
:- use_module(ilciao(resources_java), [get_java_cost/3]).
:- use_module(spec(s_simpspec), [make_atom/2]).
:- use_module(resources(res_assrt_defs(resources_trust)), 
	    [min_gen_form/3,
		max_gen_form/3]).
:- use_module(resources(init_res(trusted_res)), 
	    [translate_assrt_function_to_gen_form/4]).

:- use_module(resources(resources_basic)).

:- doc(bug, "1. The trust assertions are NOT filtered by the call
                   pattern of each corresponding call.").

:- pred apply_trusted_size(+Approx, +Pred, +Modes, +Measures, +Sizes0, -Size)
# "@var{Size} is a list of size expressions for @var{Pred} predicate, when
analysis results @var{Sizes0} are available. @var{Sizes0} is given in
general form.".

apply_trusted_size(Approx, Pred, Modes, Measures, Sizes1, Sizes) :-
	Pred = F/A,
	atom_concat(X, F0,  F),
	atom_concat(_, ':', X),
	functor(Goal, F0, A),
	( current_pp_flag(prog_lang, java) ->
	    trusted_java_size(Goal, Approx, Modes, Measures, Sizes0)
	;
	    trusted_size(Goal, Approx, Modes, Measures, Sizes0)
	),
	apply_glb_size_inferred(Sizes0, Sizes1, Approx, Goal, Sizes).

trusted_size(Goal, Approx, Modes, Measures, Sizes) :-
	findall(Ls,
	    (read_asr_size(Goal, Approx, Modes, Measures, Ls)),
	    Sizess),
	apply_glb_size_inferred_each(Sizess, Approx, Goal, Sizes0),
	head_if_empty(Sizes0, Sizes).

trusted_java_size(Goal, _, _Modes, _Measures, Sizes) :-
	functor(Goal, F, A),
	make_atom([F, A], Key),
	atom_concat([Key, '/', '1'], SubKey),
	get_java_cost(size, SubKey, Sizes).

head_if_empty([],  []) :- !.
head_if_empty([S], S).

apply_trusted_size0(Approx, F/A, Modes, Measures, Sizes) :-
	atom_concat(X, F0,  F),
	atom_concat(_, ':', X),
	functor(Goal, F0, A),
	( current_pp_flag(prog_lang, java) ->
	    trusted_java_size(Goal, Approx, Modes, Measures, Sizes)
	;
	    trusted_size(Goal, Approx, Modes, Measures, Sizes)
	).

:- pred read_asr_size(+Pred, +Approx, +Modes, +Measures, -Sizes) #
"@var{Sizes} is a list of size expressions for @var{Pred}
	predicate given by the user.".

read_asr_size(Goal, Approx, _Modes, Measures, Sizes0) :-
	assertion_of(Goal, _M, trust, _T, (_::_:_Call=>Succ+_#_), _D,
	    _Source, _LB, _LE),
	Goal =.. [_|Vars],
	get_sizes_assrt(Vars, Approx, Succ, Sizes),
	translate_list_assrt_function_to_gen_form(Sizes, Goal, Measures,
	    Sizes0).
read_asr_size(_Goal, _Approx, _Modes, _Measures, []) :- !.

get_sizes_assrt([],         _,      _,     []).
get_sizes_assrt([Var|Vars], Approx, Props, [M|Ms]) :-
	get_size_assrt(Props, Approx, Var, M),
	get_sizes_assrt(Vars, Approx, Props, Ms).

get_size_assrt([], Approx, _, Size) :-
	approx_bottom(Approx, Size), !.
get_size_assrt([size(Var, Size)|_Ps], _Approx, Arg, Size) :-
	Var == Arg, !.
get_size_assrt([_|Ps], Approx, Arg, Size) :- !,
	get_size_assrt(Ps, Approx, Arg, Size).

conv_bottom(bot, Ap, Val) :- approx_bottom(Ap, Val), !.
conv_bottom(Val, _,  Val).

:- pred apply_glb_size_inferred(+Exprs0, +Exprs1, +Approx, +Pred,
	    -Exprs) # "@var{Exprs} is the result of applying the glb
	operation to the list of size expressions @var{Expr0s} (given
	by user) and @var{Expr1s} (inferred by analysis) corresponding
	to predicate @var{Pred}. @var{Approx} is the type of
	approximation in the size analysis: @tt{ub},@tt{lb}, or
	@tt{o}.".

apply_glb_size_inferred(Exprs, [],    _, _, Exprs) :- !.
apply_glb_size_inferred([],    Exprs, _, _, Exprs) :- !.
apply_glb_size_inferred([Exp0|Exp0s], [Exp1|Exp1s],
	    Approx, Goal, [Exp|Exps]) :-
	conv_bottom(Exp1, Approx, Exp1_),
	approx_to_bound(Approx, Bound),
	glb_size(Bound, Exp0, Exp1_, Exp2), !,
	( Exp2 == bot ->
	    warning_message(
		"invalid size trust for ~w:~n ~w~n analysis infers:~n ~w", [
		    Goal, Exp1_, Exp0]),
	    Exp = Exp0
	;
	    Exp2 = Exp
	),
	apply_glb_size_inferred(Exp0s, Exp1s, Approx, Goal, Exps).

glb_size(upper, Expr0, Expr1, Expr) :-
	min_gen_form(Expr0, Expr1, Expr), !.
glb_size(lower, Expr0, Expr1, Expr) :-
	max_gen_form(Expr0, Expr1, Expr), !.

apply_glb_size_inferred_each([],                _Approx, _Pred, []) :- !.
apply_glb_size_inferred_each([Sizes],           _Approx, _Pred, [Sizes]) :- !.
apply_glb_size_inferred_each([Sz1s, Sz2s|Szss], Approx,  Pred,  Sz0ss) :-
	apply_glb_size_inferred(Sz1s, Sz2s, Approx, Pred, Sz0s),
	apply_glb_size_inferred_each([Sz0s|Szss], Approx, Pred, Sz0ss).


translate_list_assrt_function_to_gen_form([],     _,    _,      []).
translate_list_assrt_function_to_gen_form([A|As], Pred, [M|Ms], [G|Gs]) :-
	translate_assrt_function_to_gen_form(Pred, M, size(A), size(G)),
	translate_list_assrt_function_to_gen_form(As, Pred, Ms, Gs).
