:- module(_, [], [compiler(complang)]).

% Test suite for continuation passing control in Prolog.
% This is work in progress.
%
% Author: Jose F. Morales

:- export(main/0).
main :-
	testloop0,
	testloop1,
	testloop2,
	testloop3,
	testloop4,
	testloop5,
	testloop6,
	testloop7,
	testloop8,
	% test1 stops with a failure
	( test1 -> display(test1_incorrectly_succeeded), nl ; display(test1_correctly_failed), nl ),
	% test2 is internally based on continuations, stops normally
	( test2 -> display(test2_correctly_succeeded), nl ; display(test2_incorrectly_failed), nl ),
	% test3 is internally based on continuations, stops normally
	( test3 -> display(test3_correctly_succeeded), nl ; display(test3_incorrectly_failed), nl ).

% ---------------------------------------------------------------------------
% Loops

% TODO: use static subpr in mexpand.pl so that no metacalls are required
% TODO: improve syntax
% TODO: add iterator definitions

% Count from 0 to 9
testloop0 :-
	display(begin_testloop0), nl,
	(push_pair(i, 0), while(pair(i), ~i < 10)) '$ctx_on' (
          display(i(~i)), display(' '), '$ctx_inc'(i, 1)
        ),
	nl,
	display(end_testloop0), nl.
% Obtain the list of numbers between 0 and 9
testloop1 :-
	display(begin_testloop1), nl,
	(closed_accum(nums, Nums), push_pair(i, 0), while((pair(nums), pair(i)), ~i < 10)) '$ctx_on' (
          nums.add(~i), '$ctx_inc'(i, 1)
        ),
	display(nums(Nums)), nl,
	display(end_testloop1), nl.
% Like testloop1, but uses do_while (i.e. it checks condition later)
testloop2 :-
	display(begin_testloop2), nl,
	(closed_accum(nums, Nums), push_pair(i, 0), do_while((pair(nums), pair(i)), ~i < 10)) '$ctx_on' (
          nums.add(~i), '$ctx_inc'(i, 1)
	),
	display(nums(Nums)), nl,
	display(end_testloop2), nl.
% Like testloop1, but uses for_each
testloop3 :-
	display(begin_testloop3), nl,
	(closed_accum(nums, Nums), push_pair(i, 0), for_each((pair(nums), pair(i)), iter(true, ~i < 10, '$ctx_inc'(i, 1), no))) '$ctx_on' (
          nums.add(~i)
        ),
	display(nums(Nums)), nl,
	display(end_testloop3), nl.
% Nested loop
testloop4 :-
	display(begin_testloop4), nl,
	Sj = 2,
        (closed_accum(nums, Nums),
	 push_pair(i, 0), for_each((pair(nums), pair_ro(i)), iter(true, ~i < 10, '$ctx_inc'(i, 1), no)),
         push_pair(j, 0), for_each((pair(nums), pair_ro(j)), iter(true, ~j < 10, '$ctx_inc'(j, Sj), no))) '$ctx_on' (
          nums.add(p(~i, ~j))
        ),
	display(nums(Nums)), nl,
	display(end_testloop4), nl.
% Nested with predefined context constructors
% TODO: infer loop ctx automatically (i.e. the first parameter of for_each, while and do_while)
:- '$def_binder'(for_each_xrange(Ctx, Name, From, To),
	( Name :: any <- From,
	  for_each((Ctx, pair_ro(Name)), iter(true, ~'$ctx_value'(Name) < To, '$ctx_inc'(Name, 1), no)))).
:- '$def_binder'(for_each_xrange_step(Ctx, Name, From, To, Step),
	( Name :: any <- From,
	  for_each((Ctx, pair_ro(Name)), iter(true, ~'$ctx_value'(Name) < To, '$ctx_inc'(Name, Step), no)))).
testloop5 :-
	display(begin_testloop5), nl,
	Sj = 2,
        closed_accum(nums, Nums) '$ctx_on'
	  for_each_xrange(pair(nums), i, 0, 10,
	    for_each_xrange_step(pair(nums), j, 0, 10, Sj,
              nums.add(p(~i, ~j))
            )),
	display(nums(Nums)), nl,
	display(end_testloop5), nl.
% Loop on lists
testloop6 :-
	( testloop6_a,
	  fail
	; true
	).
testloop6_a :-
	display(begin_testloop6), nl,
	List = [1,2,3,4,5,6,7,8,9,10],
        push_pair(sum, 0) '$ctx_on' (
	  maplistn(pair(sum), [List], [I]) '$ctx_on' (
            '$ctx_inc'(sum, I)
          ),
	  Sum = ~sum
        ),
	display(sum_of_list(List, Sum)), nl,
	maplist(none, List, List2, X, Y) '$ctx_on' (Y is X + 1),
	display(list2(List2)), nl,
	% TODO: it is necessary to use different variables! use predabs? (ala Ruby?)
	maplistn(none, [List, List2, List3], [X2, Y2, Z2]) '$ctx_on' (Z2 is Y2 - X2),
	display(list3(List3)), nl,
	display('end_testloop6 should appear only once here'), nl,
	display(end_testloop6), nl.
testloop7 :-
	% Euclid's algorithm in imperative language:
% while x != y do
%          if x < y
%          then y := y-x
%          else x := x-y
%     return x
	% Euclid's algorithm with this dialect
	% TODO: even with this strange syntax, it looks a little shorter than the Haskell version with state-monads
	X = 100, Y = 24,
        (push_pair(x, X), push_pair(y, Y)) '$ctx_on' (
	  while((pair(x), pair(y)), \+ ~x = ~y) '$ctx_on' (
	    ( ~x < ~y ->
	        '$ctx_dec'(y, ~x)
	    ; '$ctx_dec'(x, ~y)
	    )
          ),
	  display(gcd(X,Y,~x)), nl
        ).
testloop8 :-
	Input = [1,2,3,4,5,6],
	T = _,
	maplist(none, Input, Output, _I, O) '$ctx_on' (
	  O = T
        ),
	display(res(Input, Output)), nl.

% Motivation example: stop an infinite loop 
%
% This test program stops the loop with a failure. We would like to
% write something that is not more complicate, but ends with a
% success.

test1 :-
	for_nat1(0, print1),
	nl.

print1(X) :-
	( X > 10 -> fail ; true ), % stop when X is greater than 10
	display(X), display(' ').

% Executes P for every natural number I
:- meta_predicate for_nat1(?, pred(1)).
for_nat1(I, P) :-
	P(I),
	I1 is I + 1,
	for_nat1(I1, P).

% ---------------------------------------------------------------------------
% (compiler assisted) continuation based loop.
% This test does a loop, counts numbers and calculates the sum of them.

% the continuation context

This context def is obsolete...
:- '$context_def'(cont(Ctx, Name), single(Name, primitive(pred(0, Ctx)))).

:- '$begin_context'(myctx).
:- '$incctx'(pair(sum)).
:- '$end'.
:- '$begin_context'(myctx_ro).
:- '$incctx'(pair_ro(sum)).
:- '$end'.
:- '$begin_context'(myctx_wo).
:- '$incctx'(pair_wo(sum)).
:- '$end'.
:- use_module(library(lists), [append/3]).

% TODO: only allow named continutions, stored in special context vars
% whose value cannot be read. Continuations in variables are
% troublesome, because it is difficult to scope their
% usage. I.e. calling contpass code from noncont code would result in
% incorrect code. The problem is however similar to hipair based
% dictionaries, and the solution may be similar (for continuations in
% variables use a continuation dictionary!).
%
% TODO: idea: how difficult would it be to interpret assembler code
% with all that stuff?

% TODO: create a new metatype called continuation(Context)
% TODO: a predicate with uses_continuation(Context) can execute a continuation of the same context
% TODO: what happens if contexts in the previous TODO are different?

% TODO: this predicate is called from expanded code (see mexpand.pl), FIX! 
% TODO: replace call/2 in hiord_rt by this predicate! remove $PA$ wrapper term?
'$subpr_call$'(PredAbs, NewArgs) :-
        PredAbs = '$PA$'(Goal0),
	Goal0 =.. [N|Args],
	Args2 = ~append(NewArgs, Args),
	Goal =.. [N|Args2],
        '$meta_call'(Goal).

test2 :-
	push_pair(sum, 0) '$ctx_on' (
	  % enter a predicate that uses continuations	
	  % TODO: add this one automatically
          '$enter_cont$'(myctx_ro, for_nat2_print2),
	  Sum2 = ~sum
	),
	display(final_sum(Sum2)), nl.

% Executes print2 for every natural number I
:- '$begin_context'(for_nat2_print2).
:- '$incctx'(myctx_ro).
:- '$incctx'(cont(myctx_ro, conttrue)).
for_nat2_print2 :-
	% TODO: both '$,cont$' and '$cont$'(conttrue) should be added automatically, so that it is simplified to:
	% '$catch_cont$'(for_nat2(0, print2), myctx_ro, 
        %   cont_end, (display(finish_nat2), nl)),
        % display(exit_nat2), nl.
	%
	%
	% catch the cont_end continuation
	'$,cont$'(myctx_ro, '$catch_cont$'(for_nat2(0, print2), myctx_ro, 
        % TODO: add $cont$ automatically (for last calls that do not use any cont)
          cont_end, (display(finish_nat2), nl, '$cont$'(conttrue))), % cont_end continuation
          (display(exit_nat2), nl, '$cont$'(conttrue))).
:- '$end'.

:- '$begin_context'(print2).
:- '$incctx'(myctx).
:- '$incctx'(cont(myctx_ro, conttrue)).
:- '$incctx'(cont(myctx_ro, cont_end)).
print2(X) :-
	( X > 10 ->
	    % stop when X is greater than 10
	    % (do nothing)
	    '$cont$'(cont_end)
	; display(X), display(' '),
	  '$ctx_inc'(sum, X),
	  % TODO: add this one automatically
	  '$cont$'(conttrue)
	).
:- '$end'.

% Executes P for every natural number I
:- '$context_def'(for_nat2, (myctx_ro, cont(myctx_ro, conttrue))).
:- meta_predicate for_nat2(?, pred(1, for_nat2)).
:- '$begin_context'(for_nat2).
for_nat2(I, P) :-
	'$,cont$'(myctx_ro, P(I), (
	  I1 is I + 1, % uses same cont!
	  for_nat2(I1, P)
	)).
:- '$end'.

test3 :-
	% enter a predicate that uses continuations	
	% TODO: add this one automatically
	display('test3: o o o ='), '$enter_cont$'(none, test3__2), nl,
	display('test3: o o o e ='), '$enter_cont$'(none, test3__3), nl,
	display('test3: 3 = '), push_pair(count, 0) '$ctx_on' '$enter_cont$'(pair(count), test3__4), nl.

:- '$begin_context'(test3__2).
:- '$incctx'(cont(none, conttrue)).
test3__2 :-
	'$,cont$'(none, test3__o, '$,cont$'(none, test3__o, test3__o)).
:- '$end'.

:- '$begin_context'(test3__3).
:- '$incctx'(cont(none, conttrue)).
test3__3 :-
	'$,cont$'(none, test3__o, '$,cont$'(none, test3__o, '$,cont$'(none, test3__o, display(' e')))).
:- '$end'.

:- '$begin_context'(test3__o).
:- '$incctx'(cont(none, conttrue)).
test3__o :-
	display(' o'),
	'$cont$'(conttrue).
:- '$end'.

% TODO: when there is a cont, use only the 'ro' part of the context (automatically)
:- '$begin_context'(test3__4).
:- '$incctx'(pair_ro(count)).
:- '$incctx'(cont(pair_ro(count), conttrue)).
test3__4 :-
	'$,cont$'(pair_ro(count), test3__inc, '$,cont$'(pair_ro(count), test3__inc, '$,cont$'(pair_ro(count), test3__inc, display(~count)))).
:- '$end'.

:- '$begin_context'(test3__inc).
:- '$incctx'(pair_ro(count)).
:- '$incctx'(cont(pair_ro(count), conttrue)).
test3__inc :-
	'$ctx_inc'(count, 1),
	'$cont$'(conttrue).
:- '$end'.
