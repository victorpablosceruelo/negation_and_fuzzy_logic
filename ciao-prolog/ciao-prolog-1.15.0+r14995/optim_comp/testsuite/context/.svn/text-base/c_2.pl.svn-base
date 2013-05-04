% A test for context variables
%
% Author: Jose F. Morales

:- module(_, [main/0], [compiler(complang)]).

:- use_package(hiord).
:- use_module(library(write)).

main :-
	(push_pair(text2, 'unchanged'), push_pair(text3, 'changed in some branches')) '$ctx_on' (
	  text2 <- 'unchanged',
	  text3 <- 'changed in some branches',
	  push_pair(text, 'Text0') '$ctx_on' test1,
	  push_pair(text, 'Text0') '$ctx_on' test2,
	  push_pair(text, 'Text0') '$ctx_on' test3
	),
	push_pair(count, unknown) '$ctx_on' test_count,
	push_pair(x, 0) '$ctx_on' test_cut(_),
	test_dcgemu,
	test_hiord.

% ---------------------------------------------------------------------------
:- '$begin_context'(test1).
:- '$incctx'(pair(text)).
:- '$incctx'(pair(text2)).
:- '$incctx'(pair(text3)).
test1 :-	
	show_text('Text0'),
	text <- 'Text1',
	show_text('Text1'),
	text <- 'Text2',
	show_text('Text2').

test2 :-
	show_text('Text0'),
	( text <- 'Text1a', Text = 'Text1a', Text3 = 'changed in some branches'
	; text <- 'Text1b', Text = 'Text1b', text3 <- 'changed', Text3 = 'changed'
	; text <- 'Text1c', Text = 'Text1c', Text3 = 'changed in some branches'
	; text <- 'Text1d', Text = 'Text1d', Text3 = 'changed in some branches'
	),
	show_text(Text),
	show_text2('unchanged'),
	show_text3(Text3),
	fail.
test2.

test3 :-
	show_text('Text0'),
	push(foo, 'foo') '$ctx_on' (
	  text <- 'ChangeInsidePush',
	  show_text('ChangeInsidePush')
	),
	show_text('ChangeInsidePush').
:- '$end'.

:- '$begin_context'(show_text).
:- '$incctx'(pair_ro(text)).
show_text(Expected) :-
	display(text(Expected, ~text)), nl.
:- '$end'.

:- '$begin_context'(show_text2).
:- '$incctx'(pair_ro(text2)).
show_text2(Expected) :-
	display(text2(Expected, ~text2)), nl.
:- '$end'.

:- '$begin_context'(show_text3).
:- '$incctx'(pair_ro(text3)).
show_text3(Expected) :-
	display(text3(Expected, ~text3)), nl.
:- '$end'.

% ---------------------------------------------------------------------------
% Test of pair ctx vars
:- '$begin_context'(test_count).
:- '$incctx'(pair(count)).
test_count :-
	reset_count,
	show_count(0),
	inc_count,
	show_count(1),
	( ( inc_count, C = 2
	  ; inc_count, inc_count, C = 3
	  ; C = 1
	  ),
	  show_count(C),
	  fail
	; true
	).
:- '$end'.

:- '$begin_context'(reset_count).
:- '$incctx'(pair_wo(count)).
reset_count :-
	count <- 0.
:- '$end'.

:- '$begin_context'(inc_count).
:- '$incctx'(pair(count)).
inc_count :-
	Count is ~count + 1,
	count <- Count.
:- '$end'.

:- '$begin_context'(show_count).
:- '$incctx'(pair_ro(count)).
show_count(Expected) :-
	display(count(Expected, ~count)), nl.
:- '$end'.

% ---------------------------------------------------------------------------
:- '$begin_context'(test_cut).
:- '$incctx'(pair(x)).
% TODO: improve (right now it is necessary to look at the expanded code to check if the translation is correct)
test_cut(_) :-
	( foo -> bar
	; foo
	).
test_cut(X) :- X=30.
test_cut(X) :- foo,X=40.
test_cut(X) :- foo,!,X=50.
test_cut(X) :- foo,!,bar,X=50.
test_cut(_) :- !,
	( foo -> bar
	; foo
	).
:- '$end'.

foo.
bar.

% ---------------------------------------------------------------------------
% DCG emulation

test_dcgemu :-
	push_pair(accum, "Hello World   .") '$ctx_on' (
          is_hw, Rest = ~accum
        ),
	Rest = ".",
	!.
test_dcgemu :-
	display(user_error, 'test_dcgemu failed!'), nl(user_error).

:- '$begin_context'(dcgemu).
:- '$incctx'(pair(accum)).
is_hw :- str("Hello"), !, is_hw.
is_hw :- str(" "), !, is_hw.
is_hw :- str("World"), !, is_hw.
is_hw.

str(Xs) :-
	Accum = ~accum,
	Accum = ~append(Xs, Accum0),
	accum <- Accum0.
:- '$end'.

append([], Ys, Ys).	
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
	
% ---------------------------------------------------------------------------
% Hiord calls

:- '$def_binder'(point(X,Y), (x :: any <- X, y :: any <- Y)).

test_hiord :-
	point('xval', 'yval', test_hiord__2),
	push_pair(sum, 0) '$ctx_on' test_hiord__3.

:- '$begin_context'(point).
:- '$incctx'(single(x)).
:- '$incctx'(single(y)).
test_hiord__2 :-
	hiord_1(get_x, X), show_hiord(xval,X),
	hiord_1(get_y, Y), show_hiord(yval,Y),
	hiord_2(get_x, X2), show_hiord(xval2,X2),
	hiord_2(get_y, Y2), show_hiord(yval2,Y2),
	hiord_3(get_x, X3), show_hiord(xval,X3),
	hiord_3(get_y, Y3), show_hiord(yval,Y3).
:- '$end'.

show_hiord(Expected, V) :-
	display(hiord(Expected,V)), nl.

:- '$begin_context'(point_x).
:- '$incctx'(single(x)).
get_x := ~x.
:- '$end'.

:- '$begin_context'(point_y).
:- '$incctx'(single(y)).
get_y := ~y.
:- '$end'.
	
:- meta_predicate hiord_1(pred(1, (point)), ?).
:- '$begin_context'(point).
hiord_1(G, Val) :-
	G(Val).
:- '$end'.

:- meta_predicate hiord_2(pred(1, (point)), ?).
:- '$begin_context'(point).
hiord_2(G, Val) :-
	point('xval2', 'yval2', G(Val)).
:- '$end'.

:- meta_predicate hiord_3(pred(1), ?).
hiord_3(G, Val) :-
	G(Val).

:- '$begin_context'(sum_ctx).
:- '$incctx'(pair(sum)).
test_hiord__3 :-
	List = [1,2,3,4],
	Tree = node(1,leaf,node(4,node(2,leaf,node(3,leaf,leaf)),leaf)),
	sum <- 0,
	list_forall(List, inc), show_hiord(10,~sum),
	sum <- 0,
	tree_forall(Tree, inc), show_hiord(10,~sum),
	sum <- 123,
	tree_forall(Tree, none), show_hiord(123,~sum),
	sum <- 0,
	push(scale, 10) '$ctx_on' tree_forall(Tree, scale_inc), show_hiord(100,~sum).
:- '$end'.

:- '$begin_context'(scale_inc).
:- '$incctx'(single(scale)).
:- '$incctx'(sum_ctx).
scale_inc(X) :- S1 is ~sum + X * ~scale, sum <- S1.
:- '$end'.

% TODO: implement 'polymorphic context variables', so that I can write a single definition of list_forall for all pairs
:- meta_predicate list_forall(?, pred(1,sum_ctx)).
:- '$context'(list_forall/2, (sum_ctx)).
list_forall([], _P).
list_forall([X|Xs], P) :- P(X), list_forall(Xs, P).

:- meta_predicate tree_forall(?, pred(1,sum_ctx)).
:- '$context'(tree_forall/2, (sum_ctx)).
tree_forall(leaf, _P).
tree_forall(node(X,Left,Right), P) :- P(X), tree_forall(Left, P), tree_forall(Right, P).

:- '$context'(inc/1, (sum_ctx)).
inc(X) :- S1 is ~sum + X, sum <- S1.

none(_).



