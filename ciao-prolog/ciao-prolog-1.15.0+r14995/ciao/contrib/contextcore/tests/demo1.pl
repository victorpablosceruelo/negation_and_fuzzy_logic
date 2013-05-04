:- module(demo1, [], [assertions, contextcore, fsyntax, regtypes]).
:- fun_eval arith(true).

:- include(.(loops)).

% ======================================================================
% :- types(trees).

% new notation, as assertion
%:- regtype tree(^nil | ^node(tree, term, tree)).

%:- regtype tree/1.
%tree := nil | node(left::~tree, pivot::~term, right::~tree).

:- regtype tree/1.
tree := nil | node(~tree, ~term, ~tree).
:- def_context tree = (tree:empty ; tree:node).
:- sub_module tree {
  :- def_context empty = ^nil.
  :- def_context node = ^node(left::tree, pivot::term, right::tree).

  empty[-empty].
  singleton(P)[-node] :-
    empty[-left],
    empty[-right],
    pivot <- P.

  %:- redefining(member/2). % just to avoid a warning from the module system
% JF: not necessary: member(_)[+empty] :- fail.
  member(X)[+node] :-
    ( member(X)[+left]
    ; get(X)[pivot]
    ; member(X)[+right]
    ).

  ins(X)[empty-T] :-
    singleton(X)[-T].
  ins(X)[node] :-
    get(Y)[pivot],
    ( X @< Y ->
        ins(X)[left]
    ; ins(X)[right]
    ).

  interpolate[+empty, _Dlist].
  interpolate[+node, Dlist] :-
    interpolate[+left, Dlist],
    get(X)[pivot],
    accum(X)[Dlist],
    interpolate[+right, Dlist].

  interpolate2[+T, Dlist] :-
    for_each(X, T) do accum(X)[Dlist].

  accum(X)[L] :- L = [X|Xs], L <- Xs.

  :- control (for_each(X, Tree) do code(X)) {
% todo: contextual version does not work
%    '' :- loop[+(^Tree)].
%    loop[+empty] :- true.
%    loop[+node] :- loop[+left], code(pivot@), loop[+right].
    '' :- loop(Tree).
    loop(nil).
    loop(node(L, P, R)) :- loop(L), code(P), loop(R).
  }.
}.

% ---------------------------------------------------------------------------

/*
:- context tree = {
  :- def_context tree = (tree_empty ; tree_node).
}.
:- context tree_empty > tree = {
  :- def_context tree_empty = ^nil.
  -empty.
  +ins(X)[-T] :-
	singleton(X)[-T].
  +interpolate[_Dlist].
}.
:- context tree_node > tree = {
  :- def_context tree_node = ^node(left::tree, pivot::term, right::tree).
  -singleton(P) :-
	empty[-left],
	empty[-right],
	pivot <- P.
  +member(X) :-
	( member(X)[+left]
	; get(X)[pivot]
	; member(X)[+right]
	).
  ins(X) :-
	get(Y)[pivot],
	( X @< Y ->
	    ins(X)[left]
	; ins(X)[right]
	).
  +interpolate[Dlist] :-
	interpolate[+left, Dlist],
	get(X)[pivot],
	accum(X)[Dlist],
	interpolate[+right, Dlist].
}.
*/

% ======================================================================
% Some examples of OO-programming with (unbalanced) binary trees

% ----------------------------------------------------------------------

% Construct an empty tree.
goal1 :-
	tree:empty[-T],
	display(a(T)), nl.

% ----------------------------------------------------------------------

% Construct a tree node.
goal2 :-
	tree:singleton(2)[-T],
	display(b(T)), nl.

% ----------------------------------------------------------------------
% Start from an empty tree, then insert some elements, and convert the
% result into list.

goal3 :-
	tree:empty[-T],
	tree:ins(2)[T],
	tree:ins(1)[T],
	tree:ins(6)[T],
	Lst <- Answer,
	tree:interpolate[+T, +Lst, -(^[])],
	display(c1(Answer)), nl,
	Lst2 <- Answer2,
	tree:interpolate2[+T, +Lst2, -(^[])],
	display(c2(Answer2)), nl.

% ----------------------------------------------------------------------
% Start from an empty tree, then insert some elements, and enumerate
% members.

goal4 :-
	tree:empty[-T],
	tree:ins(2)[T],
	tree:ins(1)[T],
	tree:ins(6)[T],
	( tree:member(Answer)[+T], display(d(Answer)), nl, fail ; true ).

goal5 :-
	tree:empty[-T],
	tree:ins(2)[T],
	tree:ins(1)[T],
	tree:ins(6)[T],
	display(c(T)), nl.

goal6 :-
	tree:empty[-T],
	tree:ins(2)[T],
	tree:ins(1)[T],
	tree:ins(6)[T],
	tree:for_each(X, T) do (
          display(e(X)), nl
        ).

% ---------------------------------------------------------------------------

:- fun_eval(arith(true)).

arith_demo :-
	% Do some ops
	N <- 1,
	N <- N + 1,
	N <- N * N,
	% Init another var
	A <- text,
	% Swap
	T <- N, N <- A, A <- T,
	% Display
	display(this_should_be_text(N)), nl,
	display(this_should_be_4(A)), nl.

arith_demo2 :-
	A <- 1,
	B <- 2,
	^ (A,B) <- (B,A),
	display(this_should_be_2_1(A,B)), nl.

arith_demo3 :-
	P = foo(1,2,3),
	^foo(A,B,C) <- P,
	display(this_should_be_1_2_3(A,B,C)), nl,
	A <- 10,
	display(this_should_be_10_2_3(A,B,C)), nl,
	display(this_should_be_foo_1_2_3(P)), nl.

% ---------------------------------------------------------------------------

acc(X)[Acc] :- ^[Elem|Acc] <- Acc, Elem = X.

loop_demo2 :-
	Xs = [1,2,3],
	M = text,
	Sum <- 0,
	Acc <- List,
	list:for_each(X, Xs) do (
	  % todo: cannot put Sum instead of Elem
          A = X, display(m(M, A)), nl, Sum <- Sum + 1, acc(Sum)[Acc]
        ),
	Acc = [],
	display(sum(Sum)), nl,
	display(list(List)), nl.

% :- control (for_each_tree(X, Tree) do Code(X)) {
%   (Loop(Xs0) :- Xs0 = nil ; Xs0 = node(Left, X, Right), Loop(Left), Code(X), Loop(Right)),
%   Loop(Tree)
% }.

loop_demo3 :-
	Xs = [1,2,3],
	M = text,
	Sum <- 0,
% 	for_each(X, Xs) do (A = X, display(m(M, A)), nl, Sum <- Sum + 1),
% 	for_each_tree(X, node(nil, 3, node(nil, 5, nil))) do (A = X, display(m(M, A)), nl, Sum <- Sum + 1),
	list:for_each(X, Xs) do (
	  display(m(M)), nl,		       
	  list:for_each(Y, Xs) do (
	    A = X, display(m(M, A, Y)), nl, Sum <- Sum + 1
          )
        ),
	display(sum(Sum)), nl.

max(X)[Z] :- X > Z -> Z <- X ; true.

loop_demo :-
	Z <- 0,
	list:for_each(Y, [1,2,3]) do 
          list:for_each(X, [1,2,3]) do 
            max(X * Y)[Z],
	display(z(Z)), nl.

loop_demo4 :-
	Term = f(a,b,c,d),
	Acc <- List,
	functor:for_each_arg(X, Term) do acc(X)[Acc],
	Acc = [],
	display(List), nl.

loop_demo5 :-
	range:for_each(X, 1, 10) do display(X),
	nl.

loop_demo6 :-
	% Draw a circle
	ScaleX = 2,
	Radius = 16,
	InnerRadius = 14,
	RadiusScaleX = Radius * ScaleX,
	InnerRadius2 = InnerRadius * InnerRadius,
	Radius2 = Radius * Radius,
        closed_range:for_each(Y, -Radius, Radius) do (
  	  closed_range:for_each(X, -RadiusScaleX, RadiusScaleX) do (
            X2 = X / ScaleX,
	    R2 = X2 * X2 + Y * Y,
	    ( R2 < InnerRadius2 ->
	        display('.')
	    ; R2 < Radius2 ->
	        display(o)
	    ; display(' ')
	    )
          ),
	  nl
        ).

% ---------------------------------------------------------------------------

/*
:- submod person {
  new(Name, Age, p(Name, Age)).
  name(p(Name, _Age), Name).
  age(p(_Name, Age), Age).
}.
*/

/*
:- submod person {
  :- def_state person = p(name, age).
  new(Name, Age)[-person] :- set(Name)[name], set(Age)[age].
  name(Name)[+person] :- get(Name)[name].
  age(Age)[+person] :- get(Age)[age].
}.

:- struct person {
  :- var name, age.
  -new(Name, Age) :- set(Name)[name], set(Age)[age].
  +name(Name) :- get(Name)[name].
  +age(Age) :- get(Age)[age].
}.

:- struct person {
  :- var name, age.
  -new(Name, Age) :- name:set(Name), age:set(Age).
  +name(Name) :- name:get(Name).
  +age(Age) :- age:get(Age).
}.

:- struct person {
  :- var name, age.
  -new(Name, Age) :- name<-Name, age<-Age.
  +name := name@.
  +age := age@.
}.

:- struct person {
  :- var Name, Age.
  -new(Name0, Age0) :- Name<-Name0, Age<-Age0.
  +name := Name.
  +age := Age.
}.

:- struct person {
  :- var Name, Age.
  -new(Name0, Age0) :- Name<-Name0, Age<-Age0.
  +name := Name.
  +age := Age.
}.
*/

/*
submod_test :-
	person:new(john, 10, J),
	person:new(mary, 11, M),
	( ( P = J ; P = M ),
	  person:name(P, Name),
	  person:age(P, Age),
	  display(p(Name, Age)), nl,
	  fail
	; true
	).
*/

% A definition of a submodule
:- sub_module foo {
  show :- mydisplay(foo).
  mydisplay(X) :- display(X), nl.
}.

submod_demo :-
	foo:show.

% A definition of a submodule in several blocks
:- sub_module loop {
  for_each([]).
  for_each([X|Xs]) :- do(X), for_each(Xs).
}.
:- sub_module loop {
  do(X) :- display(X).
}.

submod_demo2 :-
	loop:for_each([1,2,3]), nl.

% A nested submodule and submodule extension
% todo: existentially quantified variables in clauses => global variables
% todo: extend idea of global variables with global states!!
% todo: related: generalize pairs as something different (a sideeff), add custom methods to combine sideeffs (<- equivalent to monads?)
:- sub_module person {
  :- abstract.
  say(X) :- 
    name(N), display(N), display(': `'), display(X), display('\''), nl.
  % todo: person:foo qualification is not currently renamed! to do that, add special modenv when copying clauses in 'extends'
}.

:- sub_module dad {
  :- extends(person).
  name(dad).
  hello :- say('Hello!').
  call_child :- say('Are you there my child?'), daughter:ask.
  ask :- say('Yes, I am.').
  parent_ask :- ask. % just a renaming (for implicit module names)
  :- sub_module daughter {
    :- extends(person).
    name(daughter).
    hello :- say('Hello!').
    ask :- say('Yes, I am.').
    call_parent :-
      say('Are you there Daddy? (implicit qualification)'),
      parent_ask, % call parent (implicit)
      say('Are you there Daddy? (explicit qualification)'),
      dad:ask. % call parent (explicit)
  }.
}.

submod_demo3 :-
	dad:hello,
	dad:daughter:hello,
	dad:call_child,
	dad:daughter:call_parent.

% Very simple 'id_interface' interfaces:
% - No runtime expansion is required.
% - Predicates in an interface are added an additional argument that
%   indicates the implementor submodule.
% - The declaration 'id(Atom)' declares each submodule identifier.
% - Submodules implementing an interface fills the interface entry
%   predicates, and can also be called normally (without any overhead).

:- sub_module set {
  :- id_interface.
  :- virtual(singleton/2).
}.

:- sub_module list {
  :- extends(set).
  :- id(list).
  singleton(X, [X]).
}.

:- sub_module bintree {
  :- extends(set).
  :- id(bintree).
  singleton(X, node(nil, X, nil)).
}.

submod_demo4 :-
	A = list, B = bintree,
	(A::set):singleton(1, X),
	display(X), nl,
	(B::set):singleton(1, Y),
	display(Y), nl.

/*
Module names as contextual variables?
submod_demo4 :-
	a alias list, b alias bintree,
	a:singleton(1, X),
	display(X), nl,
	b:singleton(1, Y),
	display(Y), nl.

Another idea: (note that modules with variables should be either taken from caller or being put into the module context)

:- sub_module hello {
  :- use_from_caller_sub_mod_var(io). % the module io is instantiable and takes the value from the caller
  say :- io:display(hello).
}.
*/

:- sub_module aux_name {
%  :- doc(title, "Counter for auxiliary names").
  :- data count/1.
  init :-
	assertz(count(0)).
  finish :-
	retractall(count(_)).
  clean :-
	retractall(count(_)),
	assertz(count(0)).
  new(Name) :-
	retract(count(N)),
	N1 is N + 1,
	assertz(count(N1)),
	%
	number_codes(N, NCodes),
	atom_codes(Na, NCodes),
	atom_concat('aux', Na, Name).
}.

submod_demo5 :-
	aux_name:init,
	range:for_each(X, 1, 4) do (
	  range:for_each(Y, 1, 4) do (
            aux_name:new(A), display(auxname(X,Y,A)), nl
          ),
	  aux_name:clean
        ),
	aux_name:finish.

% ---------------------------------------------------------------------------

:- control (try(Name) do Code) {
  '' :- ( Code -> true ; display(failed(Name)), nl ).
}.

:- export(test/0).
test :-
	goal1,
	goal2,
	goal3,
	goal4,
	goal5,
	goal6,
	arith_demo,
	arith_demo2,
	arith_demo3,
	try(l1) do loop_demo,
	try(l2) do loop_demo2,
	try(l3) do loop_demo3,
	try(l4) do loop_demo4,
	try(l5) do loop_demo5,
	try(l6) do loop_demo6,
	submod_demo,
	submod_demo2,
	submod_demo3,
	submod_demo4,
	submod_demo5.

