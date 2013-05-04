:- module(_, [], [compiler(complang)]).

% Hand-coded continuation passing program.
% This is a test program used as scheme to implement predicates with continuation-passing control.
% Continuation-passing can model the (imperative-like) control in ImProlog.
%
% Author: Jose F. Morales

:- export(main/0).
main :-
%	test0bad,
%	test0,
	% test1 stops with a failure
	( test1 -> display(test1_incorrectly_succeeded), nl ; display(test1_correctly_failed), nl ),
	% test2 is internally based on continuations, stops normally
	( test2 -> display(test2_correctly_succeeded), nl ; display(test2_incorrectly_failed), nl ),
	% test2 is internally based on continuations, stops normally
	( test3 -> display(test3_correctly_succeeded), nl ; display(test3_incorrectly_failed), nl ).

%test0bad :-
%	A = ~'$meta_exp'(pred(1), foo(X)),
%	A(J),
%	display(after_foovars(X, J)), nl,
%	display(pa_was(A)), nl.
%
%foo(A, B) :-
%	display(foovars(A, B)), nl.

/*
test0 :-
	AnotherVar = _,
	MyVar = ok,
	display(Pred1), nl,
	'$subpr$'(Pred1, (display(MyVar), nl, YourVar = ok2)),
	display(YourVar), nl,
	display(Pred1), nl,
	display(AnotherVar), nl,
	'$meta_call'(Pred1),
	display(YourVar), nl.
*/

% ---------------------------------------------------------------------------
% Simple test, stops infinite loop with a failure

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
% continuation based (hand-written)
% This includes an accumulator to keep the sum

% the continuation context

THIS context def is obsolete
:- '$context_def'(cont(Ctx), single(conttrue, primitive(pred(0, Ctx)))).
:- '$context_def'(othercont(Ctx), single(contother, primitive(pred(0, Ctx)))).
:- '$begin_context'(myctx).
:- '$usectx'(pair(sum)).
:- '$end'.
:- '$begin_context'(myctx_ro).
:- '$usectx'(pair_ro(sum)).
:- '$end'.
:- '$begin_context'(myctx_wo).
:- '$usectx'(pair_wo(sum)).
:- '$end'.
:- '$context'('$continue_myctx'/0, (myctx_ro, cont(myctx_ro))).
'$continue_myctx' :-
%	Cont = ~conttrue, '$trust_metatype'(Cont, primitive(goal(myctx_ro))),
%	call(Cont).
%	Cont = ~conttrue, %'$trust_metatype'(Cont, pred(0, myctx)),
%	call(Cont).
	Cont = ~conttrue,
	% TODO: use a new predicate for B so that copy_term is not necessary!
%	display(Cont), nl,
        Cont = 'PA'(Sh,_H,_B),
	Args = ''(~sum),
%	display(cont(Args)), nl,
        copy_term(Cont, 'PA'(Sh,Args,Goal)),
        '$meta_call'(Goal).
:- '$context'('$continue_myctx_other'/0, (myctx_ro, othercont(myctx_ro))).
'$continue_myctx_other' :-
	Cont = ~contother,
        Cont = 'PA'(Sh,_H,_B),
	Args = ''(~sum),
%	display(conto(Args)), nl,
        copy_term(Cont, 'PA'(Sh,Args,Goal)),
        '$meta_call'(Goal).

test2 :-
	push_pair(sum, 0) '$ctx_on' (
          Cont = ~'$meta_exp'(primitive(pred(0, myctx_ro)), dummy_cont2(Sum)),
%	  display(sum(Sum)), nl,
%	  display(contother(Cont)), nl,
	  (push(contother, Cont), push(conttrue, Cont)) '$ctx_on' for_nat2(0, print2)
	),
	display(final_sum(Sum)), nl.

:- '$context'(dummy_cont2/1, (myctx_ro)).
dummy_cont2(Sum) :-
	Sum0 = ~sum,
%	display(dummy_cont2_0(Sum)), nl,
%	display(dummy_cont2_1(Sum0)), nl,
	Sum = Sum0.

:- '$context'(print2/1, (myctx, cont(myctx_ro), othercont(myctx_ro))).
print2(X) :-
%	Cont = ~contother,
%	display(contother_p2(Cont)), nl,
	( X > 10 ->
	    % stop when X is greater than 10
	    % (do nothing)
	    '$continue_myctx_other'
	; display(X), display(' '),
	  '$ctx_inc'(sum, X),
	  '$continue_myctx'
	).

% Executes P for every natural number I
:- meta_predicate for_nat2(?, pred(1, (myctx_ro, cont(myctx_ro)))).
:- '$context'(for_nat2/2, (myctx_ro, cont(myctx_ro))).
/*
for_nat2(I, P) :-
	% here is the magic: we force it to be a primitive(pred(0, myctx)), so the cont is bound in Cont to the current value
%	Cont = ~'$meta_exp'(primitive(pred(0, myctx_ro)), for_nat2__c0(I, P)),
	Cont = ~'$meta_exp'(primitive(pred(0, myctx_ro)), for_nat2__c0(I, P)),
	'$subpr$'(ContR, ''(S), push_pair(sum, S) '$ctx_on' for_nat2__c0(I, P)),
	display(cont(Cont)), nl,
%	display(contr(Sum0, ContR)), nl,
	display(contr(ContR)), nl,
%	display(p_is(P)), nl,
%	push(conttrue, Cont) '$ctx_on' P(I).
	push(conttrue, ContR) '$ctx_on' P(I).
*/
for_nat2(I, P) :-
	% here is the magic: we force it to be a primitive(pred(0, myctx)), so the cont is bound in Cont to the current value
%	Cont = ~'$meta_exp'(primitive(pred(0, myctx_ro)), for_nat2__c0(I, P)),
%	Cont = ~'$meta_exp'(primitive(pred(0, myctx_ro)), for_nat2__c0(I, P)),
	'$subpr$'(ContR, ''(S), push_pair(sum, S) '$ctx_on' (
	  I1 is I + 1, % uses same cont!
	  for_nat2(I1, P)
	)),
%	display(cont(Cont)), nl,
%	display(contr(Sum0, ContR)), nl,
%	display(contr(ContR)), nl,
%	display(p_is(P)), nl,
%	push(conttrue, Cont) '$ctx_on' P(I).
	push(conttrue, ContR) '$ctx_on' P(I).

/*
t :-
	ShVs = [for_nat2__c0(0,$:(PA([print2,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920))],(_1964,_1968,_1972),c1:print2(_1964,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),_1968,_1972,_1996)))),PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920))],
	Call = c1:for_nat2__c0(0,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),_2072,$:(PA([print2,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920))],(_1964,_1968,_1972),c1:print2(_1964,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),_1968,_1972,_1996)))),
	Cont = PA(ShVs,(_2072),Call),
	% cont(Cont),
	%
	% contr(MyCtx, ContR),
	MyCtx = _2108,
        ContR = c1:##4(0,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),_2108,$:(PA([print2,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920))],(_1964,_1968,_1972),c1:print2(_1964,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),_1968,_1972,_1996))))),
	% new
        ContR = c1:##4(0,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),$:(PA([print2,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920))],(_1964,_1968,_1972),c1:print2(_1964,PA(dummy_cont2(_1912),(_1920),c1:dummy_cont2(_1912,_1920)),_1968,_1972,_1996))))

*/

:- '$context'(for_nat2__c0/2, (myctx_ro, cont(myctx_ro))).
:- meta_predicate for_nat2__c0(?, pred(1, (myctx_ro, cont(myctx_ro)))).
for_nat2__c0(I, P) :-
	I1 is I + 1,
	% uses same cont!
	for_nat2(I1, P).

% ---------------------------------------------------------------------------
% continuation based (hand-written with predabs)

:- '$begin_context'(cont3).
:- '$usectx'(single(conttrue, primitive(goal))).
:- '$end'.
:- '$context'('$continue3'/0, cont3).
'$continue3' :-
	Cont = ~conttrue, '$trust_metatype'(Cont, goal),
	% TODO: use a new predicate for B so that copy_term is not necessary!
        Cont = 'PA'(Sh,_H,_B),
        copy_term(Cont, 'PA'(Sh,_Args,Goal)),
        '$meta_call'(Goal).

test3 :-
	'$enter_conttrue'(for_nat3(0, print3)),
	nl.

:- '$context'(print3/1, cont3).
print3(X) :-
	( X > 10 ->
	    % stop when X is greater than 10
	    % (do nothing)
	    true
	; display(X), display(' '),
	  '$continue3'
	).

% Executes P for every natural number I
:- meta_predicate for_nat3(?, pred(1, cont3)).
:- '$context'(for_nat3/2, cont3).
for_nat3(I, P) :-
	'$with_conttrue'(P(I), (I, P), ('$trust_metatype'(P, pred(1, cont3)), I1 is I + 1, for_nat3(I1, P))).
