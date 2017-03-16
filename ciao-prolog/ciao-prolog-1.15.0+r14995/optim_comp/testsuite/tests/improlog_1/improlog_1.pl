:- module(_, [], []).

% Some unit tests for ImProlog compilation

% TODO: merge readers
% TODO: first, read predicates from Exp, not the Impexp
% TODO: allow expanded expressions...

:- include(compiler(improlog_ops)).

:- '$default_preddef'(bytecode).

% TODO: do something to avoid absolute paths here
%:- '$native_weak_inline'(include('__root__.home.jfran.svn.ciaode.optim_comp.testsuite.tests.improlog_1.native.h')).
:- '$native_weak_inline'(include('__root__.Users.jfran.Documents.svn.ciaode.optim_comp.testsuite.tests.improlog_1.native.h')).

:- '$ptoc_typeprop'(smallint, box, smallint_box/2).
:- '$ptoc_typeprop'(smallint, unbox, smallint_unbox/2).
:- '$ptoc_typeprop'(smallint, imptype, intval).

:- '$props'(smallint_box/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [false, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
	argunboxs = [true, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~make_small(@X) % TODO: trunc nonsmall bits? here or in operations? raise error on overflow?
        ))]).
:- '$props'(smallint_unbox/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([any, var], [any, smallint]),
	argunboxs = [false, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~get_small(@X)
        ))]).
:- '$props'(smallint_new/2, [
	imp = det,
	argmodes = [param, out],
	argderefs = [false, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([any, var], [any, smallint]),
	argunboxs = [false, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([Value, Var],
	  Var <- ~'$trust_typed'(Value, intval)
        ))]).

:- export(test3/0).
:- '$props'(test3/0, [impnat = ptoc]).
test3 :-
	test3__2,
	test3__3(_).

:- '$props'(test3__2/0, [impnat = ptoc]).
test3__2.

:- '$props'(test3__3/1, [impnat = ptoc]).
test3__3(X) :- test4(X).

test4([1,2,3,1.0]).

% A predicate compiled to native code.
:- export(sump/1).
:- '$props'(sump/1, [impnat = ptoc]).
sump(C) :-
	smallint_new(10,I),
	sum1(I,100,C).

% A predicate where the code is specified as ImProlog and unfolded where it is called.
:- '$props'(sum1/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [false, false, false],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  sum(@X,@Y,Z)
	))]).

% An ImProlog predicate generated as native code.
:- '$improlog_begin'.
:- pred sum/3 + lowentry(det, [intval,intval,mut(intval)], 'sum').
sum(A,B,C) :-
	C <- A + B + ~'zero'.
:- '$improlog_end'.

% An ImProlog predicate generated as a macro.
:- '$improlog_begin'.
:- pred zero/1 + lowentrymacrocons(intval, 'zero').
zero := 0.
:- '$improlog_end'.

% ---------------------------------------------------------------------------
% TODO: a sample data definition that tries all cases

% TODO: mmem is not possible when there are self-references AND the type has disjunctions
% TODO: document: those terms cannot be constructed by parts... (that is why I can optimize the representation)
:- '$improlog_begin'.
:- lowtype(coord).
:- class coord {
  :- struct.
  :- attr a :: ref0(intmach).
}.
:- '$improlog_end'.

% TODO: types are not so easy to represent as programs... (compat is strange)
% TODO: add const qualifiers in translation comments (although it is not valid in C to have const in structures, that represents the same 'fixed value on construction' idea)
% TODO: add ~ref(T) to indicate the same than T, but with 1-reference link, so that:
% TODO:   X = ~T -> X will only unify with elements of type T
% TODO:   X = ~ref0(T) -> X will only unify with elements of type T... There is 0 links from X to the value. 
% TODO:   X = ~ref1(T) -> X will only unify with elements of type T... There is 1 link from X to the value. 
% TODO: 
% TODO: 
% TODO:   ~ref0(~intmach) --> int (fixed on construction)
% TODO:   ~ref0(mut(ref0(intmach))) --> int (mutable)
% TODO:   ~ref1(intmach) --> int * (fixed on construction)
% TODO:   ~ref1(mut(ref0(intmach))) --> int * (a fixed pointer to a integer, which is mutable)
% TODO: Does a mut ref make sense?
% TODO:   ~ref0(mut(ref1(intmach))) --> int * (a mutable pointer to a integer, which is fixed)
% TODO:   ~ref0(mut(ref1(mut(ref0(intmach))))) --> int * (a mutable pointer to a integer, which is mutable)
% TODO: [...]
:- '$improlog_begin'.
:- lowtype(point).
:- class point {
  :- struct.
  % int a;
  :- attr a :: ref0(intmach). % fixed integer value on construction 
  % int *b;
  :- attr b :: ref1(mut(intmach)). % fixed reference to a mut(intmach) on construction
  % int c;
  :- mut c :: intmach. % a mut(intmach) that lives in the str
  %  int *d;
  :- mut d :: mut(intmach). % a mut(...) that lives in the str
  %  int e[3];
  :- attr e :: ref0(array(ref0(intmach), 3)). % fixed array of fixed integers on construction
  %  int f[3];
  :- attr f :: ref0(array(mut(intmach), 3)). % fixed array of mut(intmach) (that lives in the str)
  %  coord_t a1;
  :- attr a1 :: ref0(coord).
  %  coord_t *b1;
  :- attr b1 :: ref1(coord).
  %  coord_t c1;
  :- mut c1 :: coord.
  %  coord_t *d1;
  :- attr d1 :: ref1(mut(coord)).
  %  coord_t e1[3];
  :- attr e1 :: ref0(array(ref0(coord), 3)).
  %  coord_t f1[3];
  :- attr f1 :: ref0(array(mut(coord), 3)).
  %  coord_t *g1[3];
  :- attr g1 :: ref0(array(ref1(mut(coord)), 3)).
  %  coord_t *h1[3];
  :- attr h1 :: ref0(array(mut(mut(coord)), 3)).
  % :- attr a2 :: ref0(point). % NOT valid (infinite)
  %  point_t *b2;
  :- attr b2 :: ref1(point).
  % :- attr c2 :: ref0(mut(point)). % NOT valid (infinite) (must be replaced by ~ref1(mut(point)) and take a value on construction)
  %  point_t *d2;
  :- attr d2 :: ref1(mut(point)).
  % :- attr e2 :: ref0(array(ref0(point), 3)). % NOT valid (infinite)
  % :- attr f2 :: ref0(array(ref0(mut(point)), 3)). % NOT valid (infinite)
  %  point_t *g2[3];
  :- attr g2 :: ref0(array(ref1(mut(point)), 3)).
  %  point_t *h2[3];
  :- attr h2 :: ref0(array(mut(ref1(point)), 3)).
  %  int i[3][3];
  :- attr i :: ref0(array(array(mut(intmach), 3), 3)).
  %  int (*j)[3];
  :- attr j :: ref1(mut(array(ref0(intmach), 3))).
  %  int **k;
  :- attr k :: ref1(mut(ref1(intmach))).
  %  int *l;
  :- attr l :: ref1(mut(ref0(intmach))).
  % unsigned m:1;	
  :- attr m :: ref0(bitfield(1)).
}.

