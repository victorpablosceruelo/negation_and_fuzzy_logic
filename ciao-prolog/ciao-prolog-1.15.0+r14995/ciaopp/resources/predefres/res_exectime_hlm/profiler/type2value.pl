:- module(type2value, [type_value/2], []).

:- use_module(library(random)).
:- use_module(library(lists),          [length/2]).
:- use_module(library(hiordlib),       [map/3]).
:- use_module(library(math(datadist)), [uniform/3]).
:- use_module(typeslib(typedef)).
:- use_module(typeslib(equiv_type)).

% basic_type_value('basic_props:character_code', _ExtraParams, Value) :-
% 	random(0'a,0'z,Value0),
% 	random(0'A,0'Z,Value1),
% 	random(0'0,0'9,Value2),
% 	random(1,3,X),
% 	(
% 	    X == 1 -> Value = Value0
% 	;   X == 2 -> Value = Value1
% 	;   X == 3 -> Value = Value2
% 	).

:- multifile basic_type_value/3.

basic_type_value(num, ExtraParams, Value) :-
	random(P),
	( P > 0.5
	-> basic_type_value(flt, ExtraParams, Value)
	; basic_type_value(int, ExtraParams, Value)
	).
basic_type_value(int, _ExtraParams, Value) :-
	random(-32768, 32767, Value).
basic_type_value(flt, _ExtraParams, Value) :-
	uniform(-100.0, 100.0, Value).
basic_type_value('basic_props:nnegint', _ExtraParams, Value) :-
	random(0, 32767, Value).
basic_type_value('basic_props:character_code', _ExtraParams, Value) :-
	random(1, 255, Value).
basic_type_value(var,               _ExtraParams, _Value).
basic_type_value('term_typing:var', _ExtraParams, _Value).
basic_type_value(atm,               _ExtraParams, Value) :-
%% these params avoid the overload of the atom table
	type_value2('basic_props:string', params(1.0/3.0, 10), Value0),
	atom_codes(Value, Value0).
basic_type_value('term_typing:ground', ExtraParams, Value) :-
	basic_type_value(gnd, ExtraParams, Value).
basic_type_value(gnd, ExtraParams, Value) :-
	random(2, 5, X),
	params(P1, K) = ExtraParams,
	( X == 2 ->
	    basic_type_value(num, ExtraParams, Value)
	; X == 3 ->
	    basic_type_value(atm, ExtraParams, Value)
	; P2 is (K * P1) / (1 + (K - 1) * P1),
	    type_value2('basic_props:list'('basic_props:gnd'),
		params(P2, K), Args),
	    ( X == 4 ->
		Value = Args
	    ; basic_type_value(atm, ExtraParams, Functor),
		Value =.. [Functor|Args]
	    )
	).
basic_type_value(term, ExtraParams, Value) :-
	random(1, 5, X),
	params(P1, K) = ExtraParams,
	( X == 1 ->
	    Value = _
	; X == 2 ->
	    basic_type_value(num, ExtraParams, Value)
	; X == 3 ->
	    basic_type_value(atm, ExtraParams, Value)
	; P2 is (K * P1) / (1 + (K - 1) * P1),
	    type_value2('basic_props:list', params(P2, K), Args),
	    ( X == 4 ->
		Value = Args
	    ; basic_type_value(atm, ExtraParams, Functor),
		Value =.. [Functor|Args]
	    )
	).
basic_type_value([],    _, []).
basic_type_value(Const, _, Const) :-
	num(Const).

pattern_value(Pattern, P1, K, Value) :-
	P2 is (K * P1) / (1 + (K - 1) * P1),
	(
	    typedef(Pattern, _) ->
	    type_value2(Pattern, params(P2, K), Value)
	;
	    Pattern =.. [Functor|Types],
	    map(Types, pattern_value(P2, K), Values),
	    Value =.. [Functor|Values]
	;
	    Pattern = Value
	).

type_value2(Type, ExtraParams, Value) :-
	basic_type_value(Type, ExtraParams, Value),
	!.
type_value2(^(Pattern), params(P, K), Value) :-
	pattern_value(Pattern, P, K, Value).
type_value2([Type|Other], ExtraParams, [Value|Values]) :-
	type_value2(Type,  ExtraParams, Value),
	type_value2(Other, ExtraParams, Values).
type_value2(Type, ExtraParams, Value) :-
%	display([Type,Value]),nl,
	( paramtypedef(Type, TypeDef) -> true
	; ( param_type_symbol_renaming(Type, RenType) -> true
	    ; Type = RenType
	    ),
	    ( typedef(RenType, TypeDef) -> true
	    ; equiv_type(RenType, EqType),
		typedef(EqType, TypeDef)
	    )
	),
	length(TypeDef, N),
	TypeDef = [First|_],
	random(X),
	params(P, _K) = ExtraParams,
	(
	    (N == 1 ; X < P) ->
	    type_value2(First, ExtraParams, Value)
	;
	    random(2, N, I),
	    A =.. [p|TypeDef],
	    arg(I, A, Possible),
	    type_value2(Possible, ExtraParams, Value)
	).

type_value(Type, Value) :-
	type_value2(Type, params(0.05, 10), Value),
	!.
