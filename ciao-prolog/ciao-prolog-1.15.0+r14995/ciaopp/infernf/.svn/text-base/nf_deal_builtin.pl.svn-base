%:- max_length_line(200).

% Always act as tests.

% Unification tests

pred_assertion((=) /2, Literal, _CallType, unification, Literal, fail, fail) :- !.
% Note: since input variables are bound to ground terms (=)/2 and (==)/2 are equivalent.
pred_assertion(('term_compare:==') /2,   Literal, _CallType, unification, Literal, fail, fail) :- !.
pred_assertion(('term_compare:\\==') /2, Literal, _CallType, unification, Literal, fail, fail) :- !.

% Arithmetic tests
pred_assertion(('arithmetic:=:=') /2,             Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('arithmetic:=\\=') /2,            Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('arithmetic:<') /2,               Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('arithmetic:>') /2,               Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('arithmetic:=<') /2,              Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('arithmetic:>=') /2,              Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('arithmetic:arithexpression') /1, Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
% For Sicstus
pred_assertion(('term_typing:number') /1,  Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('term_typing:integer') /1, Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('term_typing:atom') /1,    Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
% For CIAO
pred_assertion(('basic_props:num') /1, Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('basic_props:int') /1, Literal, _CallType, arithmetic, Literal, fail, fail) :- !.
pred_assertion(('basic_props:atm') /1, Literal, _CallType, arithmetic, Literal, fail, fail) :- !.

% Meta-tests
pred_assertion((var) /1, Literal, _CallType, meta, Literal, fail, fail) :- !.
% For Sicstus
pred_assertion(('term_typing:ground') /1,  Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('term_typing:number') /1,  Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('term_typing:integer') /1, Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('term_typing:float') /1,   Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('term_typing:atom') /1,    Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('term_typing:atomic') /1,  Literal, _CallType, meta, Literal, fail, fail) :- !.
% For CIAO
pred_assertion(('basic_props:gnd') /1, Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('basic_props:num') /1, Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('basic_props:int') /1, Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('basic_props:flt') /1, Literal, _CallType, meta, Literal, fail, fail) :- !.
pred_assertion(('basic_props:atm') /1, Literal, _CallType, meta, Literal, fail, fail) :- !.

% Sometimes may act as tests and sometimes succeed. 

pred_assertion(('io_basic:get_code') /1,    _Literal, _CallType, _TestFlag, _Test, true, true) :- !.
pred_assertion(('operators:current_op') /3, _Literal, _CallType, _TestFlag, _Test, true, true) :- !.
pred_assertion(('term_basic:functor') /3,   _Literal, _CallType, _TestFlag, _Test, true, true) :- !.
pred_assertion((findall) /3,                _Literal, _CallType, _TestFlag, _Test, true, true) :- !.
%% pred_assertion((findall)/3, '$'(LitOrig,_,_), CallType, TestFlag, Test, Nfail_flag, Cover_flag):-
%%   !,
%%   functor(LitOrig, F, A),
%%   pred_assertion(F/A, LitOrig, CallType, TestFlag, Test, Nfail_flag, Cover_flag).
pred_assertion(('term_basic:arg') /3, _Literal, _CallType, _TestFlag, _Test, true, true) :- !.
%
pred_assertion((is) /2, _Lit,     is(var, _), _TestFlag,  _Test,     true, true) :- !.
pred_assertion((is) /2, is(X, Y), CallType,   arithmetic, =:=(X, Y), fail, fail) :-
	!,
	warning_message("call to builtin is a test: ~q.", [CallType]).
% Always either succeed or fail
pred_assertion((!) /0,                    _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('io_basic:nl') /0,        _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('ttyout:ttynl') /0,       _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('ttyout:ttyput') /1,      _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('write:write') /1,        _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('io_basic:tab') /1,       _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('write:writeq') /1,       _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('io_basic:display') /1,   _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('write:print') /1,        _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion((true) /0,                 _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion((fail) /0,                 _Literal, _CallType, _TestFlag, _Test, fail,        fail) :- !.
pred_assertion((false) /0,                _Literal, _CallType, _TestFlag, _Test, fail,        fail) :- !.
pred_assertion((\+) /1,                   _Literal, _CallType, _TestFlag, _Test, _Nfail_flag, _Cover_flag) :- !.
pred_assertion((check) /1,                _Literal, _CallType, _TestFlag, _Test, true,        true) :- !.
pred_assertion(('native_props:indep') /1, _Literal, _CallType, _TestFlag, _Test, fail,        fail) :- !.
pred_assertion(('native_props:indep') /2, _Literal, _CallType, _TestFlag, _Test, fail,        fail) :- !.
