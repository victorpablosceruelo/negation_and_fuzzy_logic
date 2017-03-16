:- module(_, _, [macro]).

:- use_module(library(strings)).

:- '$pragma'(ptoc__dump).
%:- '$pragma'(ptoc__global_jump_opt).

% Some definitions

:- '$native_inline'(declare(msg, pointer(char), "this is a message\n")).

:- '$nativedef'(q/2, ptoc_macro([
	imp = det,
	argmodes = [in, in],
	argderefs = [false, false],
	argmems = [push, push],
	argimptypes = [int, int],
	metatypes = [any, any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([X,Y], [],
      call(printf, ["%s x:%d y:%d\n", msg, X, Y])
    ))).
:- '$nativedef'(printf_str/1, ptoc_macro([
	imp = det,
	argmodes = [in],
	argderefs = [false],
	argmems = [push],
	argimptypes = [pointer(char)],
	metatypes = [any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([X], [],
      call(printf, ["%s", X])
    ))).
:- '$nativedef'(declare/2, ptoc_macro([
	imp = det,
	argmodes = [in, in],
	argderefs = [false, false],
	argmems = [push, push],
	argimptypes = [unknown, unknown],
	metatypes = [unknown, unknown],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([Var,Type], [],
      declare(Var, Type)
    ))).
:- '$nativedef'(set_int/2, ptoc_macro([
	imp = det,
	argmodes = [in, out],
	argderefs = [false, false],
	argmems = [push, push],
	argimptypes = [unknown, int],
	metatypes = [unknown, any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([Value], [Var],
      Var = Value
    ))).
:- '$nativedef'(set_string/2, ptoc_macro([
	imp = det,
	argmodes = [in, out],
	argderefs = [false, false],
	argmems = [push, push],
	argimptypes = [unknown, pointer(char)],
	metatypes = [unknown, any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([Value], [Var],
      Var = Value
    ))).
:- '$nativedef'(set_float/2, ptoc_macro([
	imp = det,
	argmodes = [in, out],
	argderefs = [false, false],
	argmems = [push, push],
	argimptypes = [unknown, double],
	metatypes = [unknown, any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([Value], [Var],
      Var = Value
    ))).
:- '$nativedef'(is_small/1, ptoc_macro([
	imp = semidet,
	argmodes = [in],
	argderefs = [true],
	argmems = [push],
	argimptypes = [tagged_t],
	metatypes = [any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([X], [], Fail,
      if(logical_not(call('TagIsSmall', [X])), Fail)
    ))).
:- '$nativedef'(is_not_small/1, ptoc_macro([
	imp = semidet,
	argmodes = [in],
	argderefs = [true],
	argmems = [push],
	argimptypes = [tagged_t],
	metatypes = [any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([X], [], Fail,
      if(call('TagIsSmall', [X]), Fail)
    ))).
:- '$nativedef'(get_small/2, ptoc_macro([
	imp = det,
	argmodes = [in, out],
	argderefs = [true, false],
	argmems = [push, push],
	argimptypes = [tagged_t, int],
	metatypes = [any, any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([X], [Y],
      Y = call('GetSmall', [X])
    ))).
:- '$nativedef'(make_float/2, ptoc_macro([
	imp = det,
	argmodes = [in, out],
	argderefs = [false, true],
	argmems = [push, push],
	argimptypes = [double, tagged_t],
	metatypes = [any, any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([X], [Y],
      Y = call('MakeFloat', [X])
    ))).

% TODO: patching other predicates' info is not very clean
:- '$nativedef'('term_basic:$unify'/2, ptoc_itf([
	impspecialize = [on([int, int], 'ptoc_imp_b:unify_int')]
   ])).

:- '$nativedef'(unify_int/2, ptoc_macro([
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [push, push],
	argimptypes = [int, int],
	metatypes = [any, any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([X, Y], [], Fail,
      if(logical_not(X == Y), Fail)
    ))).
:- '$nativedef'(add_int/3, ptoc_macro([
	imp = det,
	argmodes = [in, in, out],
	argderefs = [false, false, false],
	argmems = [push, push, push],
	argimptypes = [int, int, int],
	metatypes = [any, any, any],
	fail_compose = id, saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false
    ],
    macro_def([X, Y], [Z],
      Z = X + Y
    ))).

% Examples

:- '$nativedef'(foo/1, ptoc([imp = semidet])).
foo(X) :-
	bar(X3),
	X3 = [_|X2],
	% TODO: the analyzer should tell that it fails!!
	X2 = [],
	X = X2.

:- '$nativedef'(bar/1, ptoc([imp = semidet, auto = true])).
bar([1,2,3]).

% TODO: not working with indexed preds!!
%:- '$nativedef'(nd/1, ptoc([imp = nondet, call_types = [int], indexed = false])).
:- '$nativedef'(nd/1, ptoc([imp = nondet, call_types = [int]])).
nd(1).
nd(2).
nd(a).

:- '$nativedef'(b/1, ptoc([imp = semidet, register = true])).
b(X) :-
	set_float(2, Float2),
	make_float(Float2, TFloat2),
	X1 = [TFloat2],
	set_float(3.1415, Float1),
	make_float(Float1, TFloat1),
	X0 = [TFloat1|X1],
	X0 = X.

:- '$nativedef'(a/2, ptoc([imp = semidet, indexed = false, register = true])).
a(X0, X2) :-
%	X0 = 3, !,
	is_not_small(X0), !,
	fail.
a(X0, X2) :-
	set_string("prolog rules!", Msg),
	mypr(Msg),
%	'$varmem'(_Unused, push(unused)),
%	'$trust_imptype'(_Unused, pointer_int),
%	declare(unused, pointer_int), % TODO: cannot define pointer(int) types... ptoc__backend_c is wrong or reg of unk values should return an encapsulated value (unk(_))
%	Foo = a(3), % must give an error
	is_small(X0),
	get_small(X0, Foo), % TODO: generated code has a unnecessary deref!!
	is_small(X2),
	d(X2), % TODO: generates unnecesary frame...
	get_small(X2, Foo),
	Foo = Foo2, % compare integers
	set_int(100000, Z),
	add_int(Z, Foo, Zb),
	add_int(Zb, Foo2, Zc),
	q(Foo,Foo2),
	% test macros at last_call
	q(Zb,Zc).

:- '$nativedef'(mypr/1, ptoc([imp = det, indexed = false, auto = true])).
mypr(X) :-
	printf_str(X),
	set_string("\n", Nl),
	printf_str(Nl).

% TODO: to get better results other properties like 'uses frame', etc. has to be inferred... 
:- '$nativedef'(d/1, ptoc([imp = semidet, auto = true, indexed = false])).
d(X) :-
	is_small(X).

:- '$nativedef'(test2/0, ptoc([imp = semidet])).
test2 :-
	test2__2(a),
	test2__2(1).

:- '$nativedef'(test2__2/1, ptoc([imp = semidet, auto = true, indexed = false])).
test2__2(_).
