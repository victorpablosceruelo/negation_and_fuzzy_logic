:- module(_, _, [compiler(compiler_object), dcg]).

% Dump several compilation files of Ciao

:- use_module(library(prolog_sys)).

:- use_module(compiler(module_deps)).
:- use_module(compiler(module_itf)).
:- use_module(compiler(module_exp)).
:- use_module(compiler(module_ideps)).
:- use_module(compiler(errs)).
:- use_module(compiler(linker__bytecode)).
:- use_module(compiler(linker__bootstrap)).
:- use_module(compiler(factory)).

:- use_module(library(system), [system/2]).
:- use_module(library(terms), [atom_concat/2]).

% TODO: document and change names, What is not a good name...

main(['--resolve', Type, Spec0]) :- !,
	factory:normalized_spec(Spec0, Spec), resolve(Type, Spec).
main(['--module', What, Spec0]) :- !,
	factory:normalized_spec(Spec0, Spec), dump(What, Spec).
main(['--file', What, FileName]) :- !,
	dump__2(What, FileName).

% ---------------------------------------------------------------------------
% Resolve the file name of a module
resolve(Type, Spec) :-
	errs:create(Errs),
	Errs/errs:add_verbose(off),
	factory:create(Errs, Factory),
	Factory/factory:enter,
	( Factory/factory:addr(Type, Spec, Name),
	  display(Name), nl -> Ok = yes
	; Ok = no
	),
	Factory/factory:leave,
	Factory/factory:delete,
	Errs/errs:delete,
	Ok = yes.

% ---------------------------------------------------------------------------
% Dump contents (of element Type) of a module

:- use_module(engine(ql_inout)).
:- use_module(library(write)).

dump(What, Spec) :-
	errs:create(Errs),
	Errs/errs:add_verbose(off),
	factory:create(Errs, Factory),
	Factory/factory:enter,
	what_type(What, Type),
	Factory/factory:addr(Type, Spec, Name),
	( dump__2(What, Name) -> Ok = yes ; Ok = no ),
	Factory/factory:leave,
	Factory/factory:delete,
	Errs/errs:delete,
	Ok = yes.

% ---------------------------------------------------------------------------
% Dump contents of a given type of a file

what_type(dectok, compile__emu) :- !.
what_type(X, X) :- !.

dump__2(What, Name) :- ql_type(What), !,
	bytecode_disassemble(Name).

:- use_module(engine(internals)).
p :-
	'$empty_gcdef_bin',
	statistics,
	dump(compile, [library, lists]),
	p.

text_type(compile__c).
text_type(prolog_source).
text_type(prolog_native_c).
text_type(prolog_native_s).
text_type(prolog_ptocdump).

objdump_type(archcompile__o).
objdump_type(archcompile__so).

ql_type(archcompile).
ql_type(compile).
ql_type(compile__emu).
ql_type(compile__emu_tempz).
ql_type(split).
ql_type(split__src).
ql_type(split__itf).
ql_type(expand).
ql_type(expand__src).
ql_type(expand__sym).

% ---------------------------------------------------------------------------

bytecode_disassemble(Name) :-
        '$open'(Name, r, Stream),
	'$push_qlinfo',
	repeat,
	  '$qread'(Stream, Goal),
          ( Goal = -1
          ; ( Goal = b(PredName, Bytecode, f(Bits, Key)) ->
	        writeq(PredName), write(':'),
	        write(' bits='), writeq(Bits),
	        ( functor(Key, N, A) ->
		    write(' key='), writeq(N/A)
		; write(' nokey')
		),
		write(' code='),
	        nl,
	        '$disasm'(Bytecode)
            ; Goal = d(PredName, Bits) ->
	        writeq(PredName), write(':'),
		write(' def='),
	        writeq(Bits),
	        nl
            ; Goal = c(X) ->
	        write('count='), writeq(X),
	        nl
            ; Goal = f(X) ->
	        writeq(X),
	        nl
	    ; writeq(Goal), nl % TODO: incomplete...
	    ),
	    fail
          ), !,
	'$pop_qlinfo',
	close(Stream).

% ---------------------------------------------------------------------------
% Count used opcodes in a bytecode file

:- data used_opcode/1.
:- data size/1.
size(0).

:- use_module(compiler(open_and_protect)).
:- use_module(compiler(emit_emulator), [ins_op/2, initialize/0]).

bytecode_dectok(BytecodeName) :-
	emit_emulator:initialize,
	reset_dectok,
	TmpName = '/tmp/dectok.txt',
	write_dectok_to_file(BytecodeName, TmpName),
	read_dectok_from_file(TmpName),
	dump_size,
	dump_opcodes,
	reset_dectok.

reset_dectok :-
	retractall_fact(used_opcode(_)),
	retractall_fact(size(_)),
	asserta_fact(size(0)).

write_dectok_to_file(BytecodeName, TmpName) :-
        open_and_protect(TmpName, TmpStream, Ref),
	write_dectok(BytecodeName, TmpStream),
        close(TmpStream),
        end_protect(Ref).

write_dectok(Name, TmpStream) :-
	current_output(OldOutput),
	set_output(TmpStream),
	( write_dectok__2(Name) ->
	    Ok = yes
	; Ok = no
	),
	set_output(OldOutput),
	Ok = yes.

write_dectok__2(Name) :-
        '$open'(Name, r, Stream),
	'$push_qlinfo',
	repeat,
	  '$qread'(Stream, Goal),
          ( Goal = -1
          ; ( Goal = b(PredName, Bytecode, f(Bits, Key)) ->
	        write('% '), writeq(PredName),
                write(' '),
                writeq(f(Bits, Key)), nl,
	        '$dectok'(Bytecode)
	    ; true
	    ),
	    fail
          ), !,
	'$pop_qlinfo',
	close(Stream).

:- use_module(library(read)).
read_dectok_from_file(TmpName) :-
        '$open'(TmpName, r, Stream),
	repeat,
	  read(Stream, X),
	  ( X = end_of_file
	  ; X = bytecode(S, Xs),
	    sum_size(S),
	    mark_opcodes(Xs),
	    fail
	  ), !,
	close(Stream).

mark_opcodes([]).
mark_opcodes([X|Xs]) :-
	( current_fact(used_opcode(X)) ->
	    true
	; assertz_fact(used_opcode(X))
	),
	mark_opcodes(Xs).

sum_size(S) :-
	current_fact(size(T0)),
	T is T0 + S,
	set_fact(size(T)).

dump_size :-
	current_fact(size(T)),
	% TODO: only instructions, not symbols or atoms
	display(bytecode_size(T)), display('.'), nl.

dump_opcodes :-
	ins_op(Ins, Op), 
	  ( current_fact(used_opcode(Op)) ->
	      true
	  ; display(unused_opcode(Op)), display('. % '), display(Ins), nl
          ),
	  fail.
dump_opcodes.
