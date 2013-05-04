:- module(_, [], [fsyntax]).

:- use_module(library(strings)).

% Some definitions

:- '$native_inline'(declare(msg, pointer(char), string("this is a message\n"))).

:- '$ptoc_typeprop'(cstring, native, true).
:- '$ptoc_typeprop'(cstring, imptype, mut(char)).
:- '$ptoc_typeprop'(cstring, box, cstring_box/2).
:- '$ptoc_typeprop'(cstring, unbox, cstring_unbox/2).
:- '$ptoc_typeprop'(float, imptype, flt64).
:- '$ptoc_typeprop'(smallint, imptype, intval).

:- '$pragma'(analyze_all).

:- include(engine(spec_arithmetic)).

% TODO: definition is too long!!! it would be good to define absmach code instead of C code and infer props about that code...
:- '$props'(q/2, [
	imp = det,
	argmodes = [in, in],
	argderefs = [false, false],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X,Y],
	  '$inline_c'([@X,@Y],[X1,Y1],call(printf, [string("%s x:%d y:%d\n"), msg, X1, Y1]))
	))]).
:- '$props'(printf_str/1, [
	imp = det,
	argmodes = [in],
	argderefs = [false],
	argmems = [cvar],
	sht_usermemo = shtdef([cstring], [cstring]),
	argunboxs = [true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X],
	  '$inline_c'([@X],[X1],call(printf, [string("%s"), X1]))
        ))]).

:- '$props'(cstring_new/2, [
	imp = det,
	argmodes = [param, out],
	argderefs = [false, false],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([any, var], [any, cstring]),
	argunboxs = [false, true], 
	saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([Value, Var],
	  Var <- ~'$trust_typed'(Value, mut(char))
	))]).
:- '$props'(cstring_box/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [false, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([cstring, var], [cstring, cstring]),
	argunboxs = [true, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(4), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~'IntmachToTagged'(~'$cast'(@X, intmach))
        ))]).
:- '$pragma'(ip((:- pred 'IntmachToTagged'/2 + foreignfun([intmach], tagged, 'IntmachToTagged') + prop(no_worker)))).
:- '$props'(cstring_unbox/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([any, var], [any, cstring]),
	argunboxs = [false, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~'$cast'(~'TaggedToIntmach'(@X), mut(char))
        ))]).
:- '$pragma'(ip((:- pred 'TaggedToIntmach'/2 + foreignfun([tagged], intmach, 'TaggedToIntmach') + prop(no_worker)))).

:- '$props'(is_smallint/1, [
	imp = semidet,
	argmodes = [in],
	argderefs = [true],
	argmems = [cvar],
	sht_usermemo = shtdef([any], [smallint]),
	saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X],
	  'TaggedIsSmall'(@X)
	))]).
:- '$props'(is_not_small/1, [
	imp = semidet,
	argmodes = [in],
	argderefs = [true],
	argmems = [cvar],
	sht_usermemo = shtdef([any], [any]),
	saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X],
	  \+ 'TaggedIsSmall'(@X)
	))]).
:- '$pragma'(ip((:- pred 'TaggedIsSmall'/1 + foreign([tagged], semidet, 'TaggedIsSmall') + prop(no_worker)))).

% TODO: patching other predicates' info is not very clean
:- '$forceprops'(term_basic:'$unify'/2, [
	impspecialize = [on([int, int], 'imp_c:unify_smallint')]
   ]).

:- '$props'(unify_smallint/2, [
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X, Y],
	  @X == @Y
	))]).

% Examples
:- export(test3/0).
:- '$props'(test3/0, [impnat = ptoc]).
test3 :-
	test3__2,
	test3__3.

:- '$props'(test3__2/0, [impnat = ptoc]).
test3__2.

:- '$props'(test3__3/0, [impnat = ptoc]).
test3__3.

:- '$trust_entry'(br/1, sht, [any]).
:- '$props'(br/1, [impnat = ptoc, imp = semidet]).
br(_) :-
	ba(_),
	bq.
:- '$props'(ba/1, [impnat = ptoc, imp = semidet]).
ba([]).
:- '$props'(bq/0, [impnat = ptoc, imp = semidet]).
bq.

:- '$trust_entry'(foo/1, sht, [any]).
:- '$props'(foo/1, [impnat = ptoc, imp = semidet]).
foo(X) :-
	bar(X3),
	X3 = [_|X2],
	% TODO: the analyzer should tell that it fails!!
	X2 = [],
	X = X2.

:- '$props'(bar/1, [impnat = ptoc, imp = semidet]).
bar([1,2,3]).

% TODO: not working with indexed preds!?!
:- '$trust_entry'(nd/1, sht, [smallint]).
:- '$props'(nd/1, [impnat = ptoc]).
nd(1).
nd(2).
nd(a).

:- export(ndi/1).
:- '$trust_entry'(ndi/1, sht, [var]).
:- '$props'(ndi/1, [impnat = ptoc]).
ndi(1).
ndi(2).
ndi(a).

:- export(b/1).
:- '$trust_entry'(b/1, sht, [any]).
:- '$props'(b/1, [impnat = ptoc, imp = semidet]).
b(X) :-
	X1 = [~float_new(2)],
	X0 = [~float_new(3.1415)|X1],
	X0 = X.

:- export(a/2).
:- '$trust_entry'(a/2, sht, [any, any]).
:- '$props'(a/2, [impnat = ptoc, imp = semidet]).
a(X0, _X2) :-
%	X0 = 3, !,
	is_not_small(X0), !,
	fail.
a(X0, X2) :-
	mypr(~cstring_new(string("prolog rules!"))),
%	'$varmem'(_Unused, cvar(unused)),
%	'$trust_imptype'(_Unused, pointer_int),
%	declare(unused, pointer_int), % TODO: cannot define pointer(int) types... ptoc__backend_c is wrong or reg of unk values should return an encapsulated value (unk(_))
%	Foo = a(3), % should fail when smallint_unbox is executed... (in previous versions: must give an error)
	is_smallint(X0),
	% TODO: generated code has a unnecessary deref!!
	Foo = X0,
	is_smallint(X2),
	d(X2), % TODO: generates unnecesary frame...
	Foo2 = X2,
%	Foo = Foo2, % compare integers
	unify_smallint(Foo, Foo2),
	Z = ~smallint_new(100000),
	smallint_add(Z, Foo, Zq),
	smallint_add(Zq, Foo, Zb),
	smallint_add(Zb, Foo2, Zc),
	q(Foo,Foo2),
	% test macros at last_call
	q(Zb,Zc).

:- '$props'(mypr/1, [impnat = ptoc, imp = det
%, argunboxs = [true]
]).
mypr(X) :-
	printf_str(X),
	printf_str(~cstring_new(string("\n"))).

% TODO: to get better results other properties like 'uses frame', etc. has to be inferred... 
:- '$props'(d/1, [impnat = ptoc, imp = semidet, argunboxs = [true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), trail_usage = max(0), should_trim_frame = false]).
d(X) :-
	is_smallint(X).

:- '$trust_entry'(test2/0, sht, []).
:- '$props'(test2/0, [impnat = ptoc, imp = semidet]).
test2 :-
	test2__2(a),
	test2__2(1).

:- '$props'(test2__2/1, [impnat = ptoc, imp = semidet, genvers = true]).
%:- '$props'(test2__2/1, [impnat = ptoc, imp = semidet]).
test2__2(_).

%:- '$trust_entry'(rectest/2, sht, [any,any]).
%:- '$props'(rectest/2, [impnat = ptoc]).
%rectest(a,X) :-
%	rectest(b,X).
%rectest(b,2).

:- '$trust_entry'(idx1/1, sht, [any]).
:- '$props'(idx1/1, [impnat = ptoc, imp = nondet, indexed = true]).
idx1(1).
idx1(2).
idx1([]).
idx1([foo|_]).
idx1([_|Xs]) :- idx1(Xs).
idx1(c(_,_)) :- idx1(4).
idx1(g(a,_,_)) :- idx1(3).
idx1(2).
idx1(2.4).
idx1(2).
idx1(a).

:- '$trust_entry'(idx2/1, sht, [any]).
:- '$props'(idx2/1, [impnat = ptoc, imp = nondet, indexed = true]).
idx2(1).
idx2(a(Xs)) :- idx2(Xs).
idx2(c(Xs)) :- idx2(Xs).

:- '$trust_entry'(idx3/2, sht, [smallint, var]).
:- '$props'(idx3/2, [argmodes = [in,out], impnat = ptoc, imp = semidet, indexed = true]).
idx3(1111, 1000).
idx3(2222, 2000).
idx3(3333, 3000).
idx3(4444, 4000).
idx3(5555, 5000).

:- '$trust_entry'(swnot/2, sht, [smallint, var]).
:- '$props'(swnot/2, [impnat = ptoc, imp = semidet, indexed = false, argmodes = [in, out]]).
swnot(0, Y) :- !, Y = 1.
swnot(_, Y) :- Y = 0.

% TODO: compilation fails!
% :- '$trust_entry'(swnotcvar/2, sht, [smallint, var]).
% :- '$props'(swnotcvar/2, [impnat = ptoc, imp = semidet, indexed = false, argmodes = [in, in], argmems = [cvar, cvar]]).
% swnotcvar(0, Y) :- !, Y = 1.
% swnotcvar(_, Y) :- Y = 0.

% TODO: compilation fails!
%:- '$trust_entry'(ucvar/2, sht, [any, any]).
%:- '$props'(ucvar/2, [impnat = ptoc, imp = semidet, indexed = false, argmodes = [in, in], argmems = [cvar, cvar]]).
%ucvar(X, Y) :-
%	A = X,
%	A = 33,
%	X = Y.

