% TODO: I need to make linker__bytecode:create_init optional
% TODO: add a definition for main (in ImProlog)
% TODO: and I need to include some definitions of absmach_def here...
% TODO: maybe it is a good idea to add a include decl in improlog (and make parent_absmach optional)

:- '$forbid_def'(fail/0).
:- '$forbid_def'(true/0).
:- '$forbid_def'(','/2).
:- '$forbid_def'((;)/2).
:- '$forbid_def'((->)/2).
:- '$forbid_def'((\+)/1).
:- '$forbid_def'(if/3).
:- '$forbid_def'((^)/2).

% TODO: kludge to avoid creation of init module
:- export('$purest_init'/0).
:- '$props'('$purest_init'/0, [impnat=indefinable]).

:- include(compiler(improlog_ops)).

% :- use_module(engine(basiccontrol)).
% :- use_module(engine(term_basic)).
% :- use_module(engine(atomic_basic)).
% 
% :- '$pragma'(analyze_all).
% :- '$default_preddef'(ptoc).
% 
% :- export(main/1).
% main([Arg]) :-
% 	atom_codes(Arg, Codes),
% 	number_codes(Num, Codes),
% 	benchmark_start(Num).
% 
% % TODO: do not show time in stdout, since it may interfere with the benchmark output
% benchmark_start(Num) :-
% 	'$trust_type'(Num, smallint),
% 	do(Num).
% 
% :- include(engine(spec_arithmetic)).

% :- '$trust_entry'(do/1, sht, [smallint]).
% :- '$props'(do/1, [argmodes=[in]]).
% do(X) :- do1(X).
% 
% % TODO: improve interface with ImProlog: define sets of properties (e.g. no_worker_interference), add arguments properties in a more automatic fashion (derefs, modes and types) 
% % A predicate where the code is specified as ImProlog and unfolded where it is called.
% :- '$props'(do1/1, [
% 	imp = det,
% 	argmodes = [in],
% 	argderefs = [true],
% 	argmems = [cvar],
% 	sht_usermemo = shtdef([smallint], [smallint]),
% 	argunboxs = [true],
% 	saveregs = all, noderefmod = true,
% 	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
% %	nosideeffdet = true, nosideeff = true,
% 	nosideeffdet = false, nosideeff = false,
% 	impnat = ptoc_macro(imacro_def([X],
% 	   (begin(@X))
% 	))]).

% TODO: kludge
:- '$native_include_c_header'(engine(engine__os)).

:- include(.(include(c_stdlib))). % for atoi

:- '$improlog_begin'.

% TODO: kludge
% Include at the beginning generated header .h file
:- lowinclude(predef_h, engine(absmach_predef)).

:- pred malloc/2 + foreignfun([intmach], cptr, malloc).
:- pred realloc/3 + foreignfun([iany/*inaccurate*/, intmach], cptr, realloc).
:- pred alloca/2 + foreignfun([intmach], cptr, alloca).
:- pred free/1 + foreign([iany], det, 'free').

:- pred memset/3 + foreign([cptr, uint8, intmach], det, 'memset').
:- pred memcpy/3 + foreign([cptr, cptr, intmach], det, 'memcpy').

% I between A..N-1
:- iterator_def(intrange2(A, N), I,
	/*Init =*/ (I = ~initmut(intmach, A)),
	/*Cond =*/ (@I < N),
	/*Next =*/ (I <- @I + 1),
	/*MaybeEmpty =*/ yes).
% I between A..N-1, incrementing in S step
:- iterator_def(intrange2step(A, N, S), I,
	/*Init =*/ (I = ~initmut(intmach, A)),
	/*Cond =*/ (@I < N),
	/*Next =*/ (I <- @I + S),
	/*MaybeEmpty =*/ yes).
% I between A..N
:- iterator_def(intrangeclosed(A, N), I,
	/*Init =*/ (I = ~initmut(intmach, A)),
	/*Cond =*/ (@I =< N),
	/*Next =*/ (I <- @I + 1),
	/*MaybeEmpty =*/ yes).
% I between A..N, step S
:- iterator_def(intrangeclosedstep(A, N, S), I,
	/*Init =*/ (I = ~initmut(intmach, A)),
	/*Cond =*/ (@I =< N),
	/*Next =*/ (I <- @I + S),
	/*MaybeEmpty =*/ yes).
% I between 0.0..N-1.0, float
:- iterator_def(flt64range(N), I,
	/*Init =*/ (I = ~initmut(flt64, 0.0)),
	/*Cond =*/ (@I < N),
	/*Next =*/ (I <- @I + 1.0),
	/*MaybeEmpty =*/ yes).

% TODO: kludge!! worker must not be necessary... but it is required for <module>__init  C functions
:- lowtype(worker).
:- class worker {
  :- struct.
  :- mut foo :: intmach.
}.

% boolean
% TODO: share with definition in absmach_def
:- lowtype(bool).
:- type(bool/1) + enum.
bool(T) :- T = ~'$base'(intmach), T = (false|true).
:- enum_encode(bool/1, [
	(false, 0),
	(true, 1)]).

% TODO: define as a array to char (with some properties)
% TODO: share with definition in absmach_def
% :- lowtype(cstring).
:- type(cstring/1) + equiv.
cstring(T) :- T = ~mut(char).

% TODO: this is a temporal type for C pointers, it should not be used anywhere!
% TODO: share with definition in absmach_def
% :- lowtype(cptr).
:- type(cptr/1) + equiv.
cptr(T) :- T = ~mut(char).

:- pred main/3 + lowentryfun([intmach, ref1(array(ref1(cstring)))], intmach, 'main').
main(Argc, Argv, Result) :-
	( Argc > 1 ->
	    N = ~atoi(Argv[1]),
	    begin(N),
	    Result = 0
	; Result = -1
	).

:- '$improlog_end'.
