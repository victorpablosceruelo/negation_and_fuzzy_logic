:- module(demo2, [], [contextcore]). 
:- use_module(library(lists), [append/3]).

% ======================================================================
% This module illustrates concepts of contextual notation, as
% implemented by the package `contextual', and especially its use to
% implement object-oriented design.

% ======================================================================
% First, let us define notions of (1) a stack, (2) a variable binding,
% (3) an environment (a list of variable bindings), and (4) a stack
% machine, which is a stack with an associated environment.  We give
% context macros capitalized names and quote them to distinguish them
% from atoms denoting contextual variables.

:- def_context         stack = stack_::list(number).
:- def_context       binding = ^(name:: atom = value::number).
:- def_context           env = env_::list(binding).
:- def_context  stack_machine = ^m(stack, env).

new_stackmachine(Env)[-stack_machine] :-
	stack_ <- [], % Syntax for: set([])[stack_]
	env_ <- Env. % Syntax for: set(Env)[env_]

% ----------------------------------------------------------------------
% Let us also define some useful predicates to work with lists as
% stacks.

push(X, A, [X|A]).
pop(X, [X|A], A).

% ----------------------------------------------------------------------
% Now, we define an abstract root class Operation for all operations
% on a stack machine.

:- def_context operation = (addition ; subtraction ; const ; store ; recall ; dup ; mul ; swap ; drop ; plus1 ; double).
:- def_context addition = ^addition.
:- def_context subtraction = ^subtraction.
:- def_context const = ^const(const_::number).
:- def_context store = ^store(var::atom).
:- def_context recall = ^recall(var::atom).
:- def_context dup = ^dup.
:- def_context mul = ^mul.
:- def_context swap = ^swap.
:- def_context drop = ^drop.
:- def_context plus1 = ^plus1(op::operation).
:- def_context double = ^double.

% ----------------------------------------------------------------------
% Some more additional context macros.

:- def_context        program = program_::list(operation).
:- def_context   empty_program = ^[].

% ----------------------------------------------------------------------
% Predicate parse_program/2 takes a list (1st arg) of terms and
% converts it into a list of Operations (2nd arg).

empty_program[-empty_program].

parse_program([])[-P] :-
	empty_program[-P].
parse_program([Word|Rest])[-P] :-
	parse_program(Rest)[-P],
	parse(Word)[-Op],
	push(Op)[P].

% ----------------------------------------------------------------------
% Predicate run_program/3 takes a program and an initial stack
% machine, and returns the stack machine that is the result of the
% execution of the program on the initial stack machine.

run_program[+P, _M] :- P = [].
run_program[+P, M] :-
	pop(Operation)[P],
	Op <- Operation,
	do[+Op, M],
	run_program[+P, M].

% ======================================================================
% Everything that is left is to define subclasses of Operation and
% implement the two methods defined by the op_inteface(operation).

:- discontiguous do/3.
:- discontiguous parse/2.

% ----------------------------------------------------------------------

do[+addition, stack_machine] :-
	pop(X)[stack_],
	pop(Y)[stack_],
	Z is X+Y,
	push(Z)[stack_].

parse(add)[-addition].

% ----------------------------------------------------------------------

do[+subtraction, stack_machine] :-
	pop(X)[stack_],
	pop(Y)[stack_],
	Z is Y-X,
	push(Z)[stack_].

parse(sub)[-subtraction].

% ----------------------------------------------------------------------

do[+const, stack_machine] :-
	get(X)[const_],
	push(X)[stack_].

parse(N)[-const] :-
	number(N),
	const_ <- N.

% ----------------------------------------------------------------------

do[+store, stack_machine] :-
	get(Var)[var],
	pop(Val)[stack_],
	get(Env0)[env_],
	( append(A, [Var=_|B], Env0) ->
	    append(A, [Var=Val|B], Env1)
	; Env1 = [Var=Val|Env0]
	),
	env_ <- Env1.

parse(sto(Var))[-store] :-
	atom(Var),
	var <- Var.

% ----------------------------------------------------------------------

do[+recall, stack_machine] :-
	get(Var)[var],
	get(Env)[env_],
	member(Var=Val, Env), 
	push(Val)[stack_].

parse(rcl(Var))[-recall] :-
	atom(Var),
	var <- Var.

% ----------------------------------------------------------------------

do[+dup, stack_machine] :-
	pop(X)[stack_],
	push(X)[stack_],
	push(X)[stack_].

parse(dup)[-dup].

% ----------------------------------------------------------------------

do[+mul, stack_machine] :-
	pop(X)[stack_],
	pop(Y)[stack_],
	Z is X*Y,
	push(Z)[stack_].

parse(mul)[-mul].

% ----------------------------------------------------------------------

do[+swap, stack_machine] :-
	pop(X)[stack_],
	pop(Y)[stack_],
	push(X)[stack_],
	push(Y)[stack_].

parse(swap)[-swap].

% ----------------------------------------------------------------------

do[+drop, stack_machine] :-
	pop(_)[stack_].

parse(drop)[-drop].

% ----------------------------------------------------------------------

do[+plus1, stack_machine] :-
	do[+op, stack_machine],
	pop(X)[stack_],
	X1 is X+1,
	push(X1)[stack_].

parse(plus1(Op))[-plus1] :-
	parse(Op)[-op].

% ----------------------------------------------------------------------

parse(double)[-double].

do[+double, stack_machine]:-
	pop(X)[stack_],
	Y is 2*X,
	push(Y)[stack_].

% ======================================================================
% An example of query, that creates an empty stack machine, compiles
% and runs a simple program (a list of stack machine operations),
% returning the resulting stack machine.

:- export(test/0).
test :-
	new_stackmachine([], SM), parse_program([1, 2, add, dup, sto(x), dup, mul], P),
	display(p(P)), nl,
	run_program(P, SM, SM_Result),
	display(r(SM_Result)), nl.
% p([Operation(Const(_456,1)),Operation(Const(_449,2)),Operation(Addition(_443)),Operation(Dup(_437)),Operation(Store(_430,x)),Operation(Dup(_424)),Operation(Mul(_418))])
% r(m([9],[=(x,3)]))
