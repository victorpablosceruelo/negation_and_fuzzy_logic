:- module(resources_eval, _, [assertions]).

:- doc(author, "Edison Mera").

eval_term(Term, _Data, _Value) :-
	\+ ground(Term),
	!,
	throw(error(type_error(ground, Term), eval_term/3 -1)).
eval_term(Term, Data, Value) :-
	eval_term_2(Term, Data, Value),
	!.

eval_term_2(Term, Data, Value) :-
	eval_term_list(Term, Data, Value).
eval_term_2(Term, Data, Value) :-
	eval_term_(Term, Data, Value).

eval_term_list([],     _,    []).
eval_term_list([A|As], Data, [Value|Values]) :-
	eval_term_(A, Data, Value),
	eval_term_list(As, Data, Values).

eval_term_(resource(ResourceName, Expr), Data,
	    resource(ResourceName, Value)) :-
	display('{WARNING: Evaluation of resource is not efficient!!!}\n'),
	eval_term_(Expr, Data, Value).

eval_term_(Expr, Data, Value) :-
	eval_term__(Expr, Data, Value),
	!.
eval_term_(Expr, _Data, Value) :-
	Value is Expr.

eval_term__(A + B, Data, Value) :-
	!,
	eval_term_(A, Data, AV),
	eval_term_(B, Data, BV),
	Value is AV + BV.
eval_term__(A - B, Data, Value) :-
	!,
	eval_term_(A, Data, AV),
	eval_term_(B, Data, BV),
	Value is AV - BV.
eval_term__(A * B, Data, Value) :-
	eval_term_(A, Data, AV),
	eval_term_(B, Data, BV),
	Value is AV * BV.
eval_term__(A / B, Data, Value) :-
	eval_term_(A, Data, AV),
	eval_term_(B, Data, BV),
	Value is AV / BV.
eval_term__(+(A), Data, Value) :-
	eval_term_(A, Data, Value).
eval_term__(-(A), Data, Value) :-
	eval_term_(A, Data, AV),
	Value is - AV.
eval_term__(sum(Counter, Ini, End, Expr), Data, Value) :-
	eval_term_(Ini, Data, IniV),
	eval_term_(End, Data, EndV),
	eval_term_sum(Counter, IniV, EndV, Expr, Data, 0, Value).
eval_term__(exp(Base, Exponent), Data, Value) :-
	eval_term_(Base,     Data, BaseV),
	eval_term_(Exponent, Data, ExponentV),
	Value is BaseV ** ExponentV.
eval_term__(fact(X), Data, Value) :-
	eval_term_(X, Data, XV),
	fact(XV, Value).
eval_term__(sin(X), Data, Value) :-
	eval_term_(X, Data, XV),
	Value is sin(XV).
eval_term__(cos(X), Data, Value) :-
	eval_term_(X, Data, XV),
	Value is cos(XV).
eval_term__($(0, N), data(_Extra, Data, UsedMetrics), Value) :-
	!,
	arg(N,         UsedMetrics, DataIndex),
	arg(DataIndex, Data,        Value).
eval_term__($(Label), data(Extra, _Data, _UsedMetrics), Value) :-
	member(v($(Label), Value), Extra),
	!.
eval_term__(inf, _Data, 0.Inf).

fact(N, F) :-
	fact_(N, 1, F).

fact_(0, F,  F) :- !.
fact_(N, F0, F) :-
	N > 0,
	!,
	F1 is F0 * N,
	N1 is N - 1,
	fact_(N1, F1, F).

eval_term_sum(Counter, IniV, EndV, Expr, data(Extra, Data, UsedMetrics),
	    Value0, Value) :-
	eval_term_(Expr, data([v(Counter, IniV)|Extra], Data,
		UsedMetrics), Value1),
	IniV2 is IniV + 1,
	( IniV2 > EndV ->
	    Value is Value0 + Value1
	; Value2 is Value0 + Value1,
	    eval_term_sum(Counter, IniV2, EndV, Expr, data(Extra, Data,
		    UsedMetrics), Value2, Value)
	).
