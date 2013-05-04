:- module(builtin,
	    [init_buildin_table/1
		, init_buildin_table/2
		, legal_pred_arg/1
		, second_order_predicate/1
		, second_order_predicate_pred_arg/2
		, second_order_predicate_pred_num/3
		, enum_trusted_facts/2
	    ], [assertions]).

%:- max_length_line(200).

:- use_module(program(assrt_db)).

:- use_module(infer(gather_modes_basic), [translate_to_modes/2]).
:- use_module(library(terms_vars)).

%
%  builtin.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for initializing builtin table.
%
%  The structure of the builtin table:
%	st(Pred/Arity,clause,mode,measure,mutex,det,size,solution,time,domain)
%

:- use_module(infercost(init(symtable)), [insert_symbol_entry/3]).
:- use_module(infercost(size(clause)), [number_of_literals/3]).

:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).

approximation(X) :- current_pp_flag(cost_approximation, X).

%
%  Initialize the buildin table.
%


% ----------------------------------------------------------------------------

% Added by PLG 6 April 1997

:- push_prolog_flag(multi_arity_warnings, off).

init_buildin_table(BT) :-
	approximation(Approx),
	init_buildin_table(Approx, BT).

enum_trusted_facts(Pred, st(Pred, _, Mode, Measure, _Mutex, Det, _Size,
	    _Solution, _Time, _Domain)) :-
	Det = 1,
	assertion_read(Pred, _M, true, success, Body1, _Dict, _Source, _LB, _LE),
	assertion_body(Pred, _Compat, Call, Succ, _Comp, _Comm, Body1),
	(member('native_props:size'(_, _), Succ) -> true),
	vartypes_to_mode(Pred, Call, Succ, Mode),
	succinfo_to_measure(Pred, Succ, Measure).

succinfo_to_measure(Pred, Succ, Measure) :-
	copy_term([Pred, Succ], [Pred0, Succ0]),
	Pred0 =.. [_|Measure],
	match_measure(Succ0),
	translate_to_measures(Measure).

translate_to_measures([]    ).
translate_to_measures([A|As]) :-
	(
	    var(A) -> A = (void)
	;
	    true
	),
	translate_to_measures(As).

match_measure(['native_props:size'(V, Metric)|Succ]) :-
	Metric =.. [MetricName, V],
	!,
	V = MetricName,
	match_measure(Succ).
match_measure(['native_props:size'(V, _Metric)|Succ]) :-
	V = void,
	!,
	match_measure(Succ).
match_measure([_|Succ]) :-
	match_measure(Succ).
match_measure([]).

vartypes_to_mode(Pred, Call, Succ, Mode) :-
	copy_term([Pred, Call], [Pred0, Call0]),
	Pred0 =.. [_|Args0],
	match_call(Call0),
	copy_term([Pred, Succ], [Pred1, Succ1]),
	Pred1 =.. [_|Args1],
	match_succ(Succ1),
	Args0 = Args1,
	translate_to_modes(Args0, Mode).

match_call(['term_typing:var'(V)|Call]) :-
	!,
	match_call(Call),
	V = n/_.
match_call([C|Call]) :-
	varset(C, V),
	match_call(Call),
	V = [y/_|_].
match_call([]).

match_succ(['term_typing:var'(V)|Call]) :-
	!,
	match_succ(Call),
	V = _/ n.
match_succ([C|Call]) :-
	varset(C, [V]),
	!,
	match_succ(Call),
	V = _/ y.
match_succ([_C|Call]) :-
	match_succ(Call).
match_succ([]).

init_buildin_table(upper, BT) :-
% Place here builtin information only related with upper bound analysis
	init_buildin_table_(BT).
init_buildin_table(lower, BT) :-
% Place here builtin information only related with lower bound analysis
	init_buildin_table_(BT).

init_buildin_table_(BT) :-
% Place here builtin information common to upper and lower analysis
	insert_symbol_entry(BT, '\+'/1,                  st('\+'/1,                  _, [+],          [void],                  _, [1], _,  inf, [0   ], _)), % Added Jul-22-05 -PLG
	insert_symbol_entry(BT, 'is'/2,                  st('is'/2,                  _, [-, +],       [int, int],              _, [1], [], inf, [calc], _)),
	insert_symbol_entry(BT, '='/2,                   st('='/2,                   _, [(?), (?)],   [void, void],            _, [1], [], inf, [0   ], _)),
	insert_symbol_entry(BT, 'term_basic:functor'/3,  st('term_basic:functor'/3,  _, [+, -, -],    [size, size, int],       _, [1], [], inf, [prof], _)),
	insert_symbol_entry(BT, 'term_basic:arg'/3,      st('term_basic:arg'/3,      _, [+, +, -],    [int, size, size],       _, [1], [], inf, [prof], _)),
	insert_symbol_entry(BT, functor1/3,              st(functor1/3,              _, [-, +, +],    [size, size, int],       _, [1], [], inf, [0   ], _)),
	insert_symbol_entry(BT, arg/4,                   st(arg/4,                   _, [+, +, +, -], [int, size, size, size], _, [1], [], inf, [0   ], _)),
	insert_symbol_entry(BT, 'term_compare:=='/2,     st('term_compare:=='/2,     _, [+, +],       [void, void],            _, [1], _,  inf, [0   ], _)),
	insert_symbol_entry(BT, 'term_compare:\\=='/2,   st('term_compare:\\=='/2,   _, [+, +],       [void, void],            _, [1], _,  inf, [0   ], _)),
	insert_symbol_entry(BT, 'arithmetic:=:='/2,      st('arithmetic:=:='/2,      _, [+, +],       [int, int],              _, [1], _,  inf, [calc], _)),
	insert_symbol_entry(BT, 'arithmetic:=\\='/2,     st('arithmetic:=\\='/2,     _, [+, +],       [int, int],              _, [1], _,  inf, [calc], _)),
	insert_symbol_entry(BT, 'arithmetic:<'/2,        st('arithmetic:<'/2,        _, [+, +],       [int, int],              _, [1], _,  inf, [calc], _)),
	insert_symbol_entry(BT, 'arithmetic:>'/2,        st('arithmetic:>'/2,        _, [+, +],       [int, int],              _, [1], _,  inf, [calc], _)),
	insert_symbol_entry(BT, 'arithmetic:=<'/2,       st('arithmetic:=<'/2,       _, [+, +],       [int, int],              _, [1], _,  inf, [calc], _)),
	insert_symbol_entry(BT, 'arithmetic:>='/2,       st('arithmetic:>='/2,       _, [+, +],       [int, int],              _, [1], _,  inf, [calc], _)),
	insert_symbol_entry(BT, 'term_typing:atomic'/1,  st('term_typing:atomic'/1,  _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'lists:nth'/3,           st('lists:nth'/3,           _, [+, +, -],    [int, length, void],     _, [1], _,  inf, [1   ], _)),
	insert_symbol_entry(BT, 'term_typing:atom'/1,    st('term_typing:atom'/1,    _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'term_typing:number'/1,  st('term_typing:number'/1,  _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'term_typing:integer'/1, st('term_typing:integer'/1, _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'basic_props:atm'/1,     st('basic_props:atm'/1,     _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'basic_props:num'/1,     st('basic_props:num'/1,     _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'basic_props:int'/1,     st('basic_props:int'/1,     _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'basic_props:gnd'/1,     st('basic_props:gnd'/1,     _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'term_typing:float'/1,   st('term_typing:float'/1,   _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'term_typing:var'/1,     st('term_typing:var'/1,     _, [+],          [void],                  _, [1], _,  inf, [0   ], _)),
	insert_symbol_entry(BT, 'term_typing:nonvar'/1,  st('term_typing:nonvar'/1,  _, [+],          [void],                  _, [1], _,  inf, [prof], _)),
	insert_symbol_entry(BT, 'write:write'/1,         st('write:write'/1,         _, [+],          [void],                  _, [1], _,  inf, [0   ], _)),
	insert_symbol_entry(BT, 'io_basic:tab'/1,        st('io_basic:tab'/1,        _, [+],          [void],                  _, [1], _,  inf, [0   ], _)),
	insert_symbol_entry(BT, 'io_basic:nl'/0,         st('io_basic:nl'/0,         _, [],           [],                      _, [1], _,  1,   [0   ], _)),
	insert_symbol_entry(BT, 'basiccontrol:fail'/0,   st('basiccontrol:fail'/0,   _, [],           [],                      _, [0], _,  0,   [0   ], _)),
	insert_symbol_entry(BT, 'basiccontrol:true'/0,   st('basiccontrol:true'/0,   _, [],           [],                      _, [1], _,  1,   [prof], _)),
	insert_symbol_entry(BT, '!'/0,                   st('!'/0,                   _, [],           [],                      _, [1], _,  1,   [prof], _)),
	insert_symbol_entry(BT, findall/3,               st(findall/3,               _, [-, +, -],    [void, void, length],    _, [1], _,  inf, [0   ], _)).
%	insert_symbol_entry(BT, '=..'/2,                 st('=..'/2,                 _, [+, -],       [size, length],          _, [1], [], inf, [0   ], _)),

:- pop_prolog_flag(multi_arity_warnings).

second_order_predicate(findall/3).

second_order_predicate_pred_arg(findall(_, P, _), P).

second_order_predicate_pred_num(Body, LitNum, Num) :-
	number_of_literals(Body, 1, Num1),
	Num is Num1+LitNum.

legal_pred_arg(Pred) :-
	functor(Pred, F, N),
	F/N \== ','/2, % single literal
	\+ second_order_predicate(F/N). % non-second-order predicate

/* PBC: not used
%
%  Print out the buildin table.
%
print_buildin_table(BT) :-
	tell(buildin_table),
	p_buildin_table(BT),
	told.

p_buildin_table(BT) :-
	var(BT).
p_buildin_table(BT) :-
	nonvar(BT),
	BT = [E|B],
	write(E),
	nl,
	p_buildin_table(B).
*/
