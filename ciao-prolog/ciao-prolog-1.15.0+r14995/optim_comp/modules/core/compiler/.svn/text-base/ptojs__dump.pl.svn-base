% Dump middle-level definitions

% Notation:
%
%   wcode: middle-level WAM-like instructions
%   middefs: middle-level definitions

:- use_module(library(write), [write/2]).

dump_middefs(FsId, Defs, Suffix) :-
	OutName = ~fsR(dyn(Suffix, FsId)),
	'$open'(OutName, w, OutStream),
	outstream :: any <- OutStream,
	indent :: m_any <- 0,
	dump_defs(Defs),
	close(OutStream).

dump_middefs_to_stdout(Defs) :-
	outstream :: any <- user_output,
	indent :: m_any <- 0,
	dump_defs(Defs).

{
:- fluid outstream :: any.
:- fluid indent :: m_any.
    
dump_defs([]).
dump_defs([X|Xs]) :-
        ( indent, dump_def(X) ->
	    true
	; throw(bug_failed_dump_def(X))
	),
        dump_defs(Xs).

dump_nested_def(Ref, Kind, Defs) :-
	dump_h(def(Ref, Kind)),
	inner,
        ( Defs = [ctor(CtorKind)|Defs0] ->
	    % Emit a constructor (non-basal) for the class
	    indent, dump(ctor(CtorKind)), dump_nl
	; Defs = Defs0
	),
        dump_defs(Defs0),
	outer.

dump_def(nested_def(Ref, Kind, Defs)) :- !, % definition of a module/class/trait/etc.
        dump_nested_def(Ref, Kind, Defs).
dump_def(js_raw_native(X)) :- !, % raw JS native code
	dump_h('raw JS'),
	dump_native(X).
dump_def(module_init(ModuleR)) :- !, % module initialization
	dump_h(module_init(ModuleR)).
dump_def(declvar(VName)) :- !, % (global) variable declaration
	vardic :: cdic <- ~cdic.new,
	dump_ins(declvar(VName)).
dump_def(extends(BaseRef)) :- !,
	vardic :: cdic <- ~cdic.new,
        dump_ins(extends(BaseRef)).
dump_def(extends_nonvar(BaseRef, BCN, N, A)) :- !,
	vardic :: cdic <- ~cdic.new,
        dump_ins(extends_nonvar(BaseRef, BCN, N, A)).
dump_def(functor_trait(BCN, N, A)) :- !,
	vardic :: cdic <- ~cdic.new,
        dump_ins(functor_trait(BCN, N, A)).
dump_def(prop(PropId, Value)) :- !,
	vardic :: cdic <- ~cdic.new,
        dump_ins(prop(PropId, Value)).
dump_def(bcode_a(MetR, Cs, PLWAMCs)) :- !,
	vardic :: cdic <- ~cdic.new, % TODO: use a different vardic per clause?
	dump_head(bcode_a(MetR), []),
	dump_or(Cs),
	indent, dump('**PLWAM version**'), dump_nl,
	dump_or(PLWAMCs). % TODO: merge (not really used now)
dump_def(bcode(Ref, Args, Code)) :- !, % basal predicate/method
	vardic :: cdic <- ~cdic.new,
	dump_head(bcode(Ref), Args),
	dump_code(Code).
dump_def(rawcode(Code)) :- !, % (not a real def, only for show_bc_code)
	vardic :: cdic <- ~cdic.new,
        dump_code(Code).
dump_def(X) :-
	throw(bug_unknown_dump_def(X)).

% Dump native JS code
dump_native(X) :- inner, dump_native_(X), outer.

dump_native_([]).
dump_native_([X|Xs]) :- dump_native__(X), dump_native_(Xs).

dump_native__(X) :- indent, dump_native_stat(X).

dump_native_stat(function(H,B)) :- !,
	dump_h(function(H)),
	dump_native(B).
dump_native_stat(X) :- dump(X), dump_nl.

{
:- fluid vardic :: cdic. % Dictionary for naming variables

% Obtain a name for a variable
varname(K, N) :-
	( vardic.get(K, N) ->
	    true
	; V = ~vardic.n,
	  N = v(V),
	  vardic.insert(K, N)
	).

% Dump alternatives (code for each clause)
dump_or([]).
dump_or([X|Xs]) :- % TODO: show metargs?
	indent,
	( X = ma_clause(_, Cs, Data) -> true
	; X = clause(Cs, Data) -> true % TODO: PLWAM
	; fail
	),
	dump('clause'), dump(' '), dump_idxdata(Data), dump(':'), dump_nl,
	dump_code(Cs),
	dump_or(Xs).

% Dump indexing data
dump_idxdata(Data) :-
	idxdata_term(Data, Term),
	dump(Term).

% Dump code
dump_code(X) :- inner, dump_code_(X), outer.

dump_code_([]).
dump_code_([X|Xs]) :- indent, dump_ins(X), dump_code_(Xs).

% Dump instruction
dump_ins(set_success_cont(BB)) :- !,
	trust(BB instance_of bblock),
	dump(set_success_cont(~BB.ref)), dump_nl.
dump_ins(push_failure_cont(BB)) :- !,
	trust(BB instance_of bblock),
	dump(push_failure_cont(~BB.ref)), dump_nl.
dump_ins(low_test(X)) :- !,
	dump('low_test '), dump_insterm(X),
	dump_nl.
dump_ins(X) :-
	dump_insterm(X),
	dump_nl.

dump_args([]).
dump_args([X]) :- !, dump_arg(X).
dump_args([X|Xs]) :- dump_arg(X), dump(' '), dump_args(Xs).

dump_arg(X) :- var(X), !, dump_varname(X). % for dump_head/2
dump_arg(X) :- X instance_of predicate_s, !,
	dump(~X.get_id).
dump_arg(X) :- X instance_of termvar, !, dump_var(X).
dump_arg(native_string(Xs)) :- !, dump_string(Xs).
dump_arg(X) :- string_codes(X, Cs), !, dump_string(Cs).
dump_arg(cons(X)) :- string_codes(X, Cs), !, dump_string(Cs).
dump_arg(cons(X)) :- !, dump(X).
dump_arg(X) :- X = [_|_], !, dump('['), dump_args(X), dump(']').
dump_arg(X) :- dump(X).

dump_varname(X) :-
	v(N) = ~varname(X),
	dump('V'), dump(N).

dump_var(X) :-
	X instance_of termvar,
	dump_varname(~X.name),
	( Mem = ~X.getp(mem) ->
	    dump('{'),
	    dump_mem(Mem),
	    dump('}')
	; true
	).

dump_mem(X) :- var(X), !, dump('?').
dump_mem(x(N)) :- var(N), !, dump('x(?)'). % (was tmpvar)
dump_mem(strmem(Str, I)) :- !, dump_var(Str), dump('.'), dump(I).
dump_mem(cargmem(I)) :- !, dump('a'), dump(I).
dump_mem(rawmem(X)) :- !, dump(X).
dump_mem(X) :- dump(X).

% Dump some 'head-like' text (reference with arguments)
dump_head(X, Args) :-
	dump(X), dump(' ['), dump_args(Args), dump(']:'), dump_nl.

% Dump f(X1...Xn) as 'f Y1 ... Yn', where Yi is the dump of Xi
dump_insterm(X) :-
	X =.. [N|As],
	dump(N), dump(' '),
	dump_args(As).
}.

:- use_module(library(strings)).
:- use_module(compiler(write_c_common), [escape_codes/2]).

% Basic dump operations 
dump(X) :- write(~outstream, X).
dump_string(X) :- dump('\"'), write_string(~outstream, ~escape_codes(X)), dump('\"').
dump_codes(X) :- write_string(~outstream, X).
dump_nl :- nl(~outstream).

dump_h(X) :-
	X =.. [N|As],
	dump(N), dump(' '),
	dump_h_args(As), dump(':'), dump_nl.

dump_h_args([]).
dump_h_args([X]) :- !, dump(X).
dump_h_args([X|Xs]) :- dump(X), dump(' '), dump_h_args(Xs).

% Indentation level
inner :-
	I is ~indent + 1,
	indent <- I.
outer :-
	I is ~indent - 1,
	( I < 0 -> throw(bug_negative_tab) ; true ),
	indent <- I.

indent :-
	indent_(~indent).

indent_(0) :- !.
indent_(I) :- indent1, I1 is I - 1, indent_(I1).

indent1 :-
	dump('  ').
}.
