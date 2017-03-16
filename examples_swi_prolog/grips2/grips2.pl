/*  grips2.pl  */


:- module( grips2, [ (grips)/1
                   , (grips)/2
                   ] ).


/*
GRIPS
-----

For an introduction to GRIPS, see the documentation. The current file adds a 
clause for trans_def (defined in this module) 
to term_expansion sp that it can handle GRIPS definitions.
It also exports grips/1, which can be used to call GRIPS expressions 
from the top-level interpreter; and /3, which just translates
an expression. This version of GRIPS is for SWI-Prolog.
*/


/*
Utilities.
----------

This section defines several utility predicates.
*/


/* conjoin( +G1, +G2, -G1G2 ):
     G1G2 is the result of conjoining goal G2 to G1.
     If one of the goals is 'true' or 'fail', the conjunction 
     is optimised accordingly.
*/
conjoin( true, G, G ) :- !.
conjoin( G, true, G ) :- !.
conjoin( fail, _, fail ) :- !.
conjoin( G1, G2, (G1,G2) ) :- !.


/* conjoin( +G1, +G2, +G3, -G1G2G3 ):
     G1G2G3 is the result of conjoining goal G3 to G2 to G1.
*/
conjoin( G1, G2, G3, G1G2G3 ) :-
    conjoin( G2, G3, G2G3 ),
    conjoin( G1, G2G3, G1G2G3 ).


/* conjoin( +G1, +G2, +G3, +G4, -G1G2G3G4 ):
     G1G2G3G4 is the result of conjoining goal G4 to G3 to G2 to G1.
*/
conjoin( G1, G2, G3, G4, G1G2G3G4 ) :-
    conjoin( G2, G3, G4, G2G3G4 ),
    conjoin( G1, G2G3G4, G1G2G3G4 ).


/*
Operator definitions.
---------------------

By choosing suitable precedence and associativities, we can find
operators to give us all the syntactic constructs we want.

The predicate 'check_syntax' is called when translating terms, to ensure
that operators are combined correctly. In check_syntax(Goal,M,C), M is the 
error message (an atom), and C is a list of "culprits": the particular 
things which caused the error. If Goal fails, the predicate writes the 
string M followed by 'culprits were' and C, and then aborts to
the top-level.
*/


:- op( 990, fy, user:(grips) ).
   % For evaluating GRIPS expressions from the top-level interpreter.
   %
   % I think I want it to bind more tightly than comma, so that
   % we can have goals such as (grips ....), nl. The argument to
   % 'grips' will normally be a function call, not a conjunction.

:- op( 990, fy, user:(echo) ).
   % Displays the result of translating an expression.

:- op( 1200, xfx, user:(<-) ).
   % Main connective for GRIPS function definitions.

:- op( 1200, xfx, user:(does) ).
   % Main connective for GRIPS command definitions.

:- op( 1100, xfy, user:(or) ).
   % Alternative to ; in definitions.

:- op( 1090, xfx, user:(if) ).
:- op( 1100, yfx, user:(else) ).
   % Make up GRIPS conditional expressions.

:- op( 1100, xfx, user:(=>) ).
   % Also makes conditional expressions, but written
   % with the condition before the action, not
   % after it as with 'if'.

:- op( 1000, xfy, user:(and) ).
   % Alternative to , in definitions.

:- op( 800, xfy, user:(where) ).
   % Introduces a subsidiary definition in a GRIPS
   % expression. 
   % Should bind less tightly than =.

:- op( 700, xfx, user:(grips) ).
   % (V grips E) translates E, calls it, and
   % makes V the result variable.
   % Should mimic assignment.

:- op( 150, fx, user:(`) ).
   % Try making it bind less tightly than :, so that
   % we can quote SWI module-predicate references:
   % `Module:Pred.
   % But also make it bind more tightly than =, so that
   % we can write
   % A = `a:b.
   %
   % No. On second thoughts, make it bind more tightly
   % even than - and other ops, so that we can write,
   % e.g. `a- `b.

:- op( 150, fx, user:('CONS') ).

:- op( 150, fx, user:(twist) ).


check_syntax( Goal, _, _ ) :-
    call( Goal ), !.
check_syntax( _, String, Culprits ) :-
    write( [String, Culprits] ), nl, abort.


/* Evaluating goals. 
--------------------
*/


:- module_transparent (grips)/2.

grips( V, E ) :-
    grips_options( Opt ),
    trans_expr( E, V, Goals, Opt ),
    call( Goals ).


:- module_transparent (grips)/1.

grips( echo(V) ) :-
    var(V), !,
    write('Argment of \'echo\' is a variable.'), nl.

grips( echo(G) ) :-
    !,
    grips_options( Opt ),
    trans_expr( G, ResultVar, Goals, Opt ),
    write( 'GOALS: ' ), write( Goals ), nl,
    write( 'RESULT VAR: ' ), write( ResultVar ), nl,
    try( Goals, ResultVar ).

grips( E ) :-
    !,
    grips_options( Opt ),
    trans_expr( E, ResultVar, Goals, Opt ),
    try( Goals, ResultVar ).


try( G, V ) :-
    call( G ),
    write( 'Result = ' ), print( V ), write( '.' ), nl, !.
try( _, _ ) :-
    write( 'Failed.' ), nl, !.


grips_options( [arithmetic_calls_is, expression_prefix(grips)] ).


/*
Translating definitions.
------------------------

Like Prolog, GRIPS definitions come as Prolog terms, usually read from a
file in standard Prolog syntax (augmented by the operator definitions
above) and terminated by a dot. Unlike Prolog, there may be several
different kinds of term: function definitions, predicate definitions,
command definitions. These are described in the GRIPS documentation.

GRIPS translates each definition into a Prolog clause and asserts that
clause. To do so, it calls one main predicate, trans_def( GD+, Clause- ).

Here, GD is a GRIPS definition. 'trans_def' translates it into a Prolog
clause in Clause, which can then be asserted. 'trans_def' fails if its
first argument is not a GRIPS definition (note that this means it FAILS
if given H:-T as a first argument).

The idea behind translating functions is given in the documentation.
Briefly, a definition of the form
    f(A,B) <- g(C) if D.
is translated into the clause
    f(A,B,V) :- D', !, g(C',V).
where V is a new variable introduced to carry the result. Guards such
as D are translated (into D') to unwind function evaluations in the
arguments to the predicates they call. Function arguments such as C are
also unwound. This may involve introducing auxiliary goals to evaluate
functions called inside these arguments.


Implementation.
---------------

'trans_def' calls trans_guard or trans_expr to translate the body. To
translate the head of function definitions, it calls
    trans_head( Head_G, V, Head ).

The first argument is a head, either an atom or a term of the form
    H(A1,...An).
The third argument will become a term with one more argument than Head_G,
where the final argument is V:
    H(A1,...An,V)

If the first argument is an atom H, then the third will become
    H(V).
*/


/* trans_def( +Def_G, -Def_P, +Options ):
     Def_P is the Prolog clause corresponding to GRIPS
     definition Def_G.
*/
trans_def( (Head_G <- Var_G), (Head_P :- !), Opt ) :-
    var( Var_G ),
    !,
    trans_head( Head_G, Var_G, Head_P, Opt ).

trans_def( (Head_G <- Expr_G) , (Head_P :- (Tail_P, !)), Opt ) :-
    !,
    trans_expr( Expr_G, ResultVar, Tail_P, Opt ),
    trans_head( Head_G, ResultVar, Head_P, Opt ).

trans_def( (Head_G does Var_G), (Head_G :- (Var_G, !)), _ ) :-
    var( Var_G ),
    !.

trans_def( (Head_G does Command_G) , (Head_G:-(Tail_P,!)), Opt ) :-
    !,
    trans_guard( Command_G, Tail_P, Opt ).

trans_def( (Head_G if Var_G), (Head_G :- (Var_G, !)), _ ) :-
    var( Var_G ),
    !.

trans_def( (Head_G if Guard_G), (Head_G:-(Tail_P,!)), Opt ) :-
    !,
    trans_guard( Guard_G, Tail_P, Opt ).


/* trans_head( +Head_G, -ResultVar, -Head_P, +Options ):
     Head_P is the Prolog clause head corresponding to
     Head_G, the head of a GRIPS definition. Head_P
     will contain one more argument than Head_P, at the
     right: this is ResultVar, the variable in which
     Head_G's result is returned. 
*/
trans_head( Head_G, ResultVar, Head_P, _ ) :-
    Head_G =.. [ F | Args_G ],
    append( Args_G, [ResultVar], Args_P ),
    Head_P =.. [ F | Args_P ].


/*
Translating guards.
-------------------

The main predicate for this is 'trans_guard( Gd, G )'. Here, Gd is a
GRIPS guard and G is its translation as a Prolog goal. The difference
between guards and goals is that arguments in guards may contain
function calls. 'trans_guard' translates these into auxiliary goals and
conjoins these goals with those in the original guard.

There is also a 'trans_ge_guard'. This implements macro expansion
using SWI-Prolog 'goal_expansion'. It expands out arguments of
the form grips(A) inside goals to generate new goals that will
evaluate the A. This is a convenient way of enabling us to write
goals that are mainly Prolog, but that contain the occasional
expression to be evaluated and substitited in place.


Implementation.
---------------

There are two tricky points. The first is the treatment of
    V = Expr
If V is unbound at translation-time, we assume that it is to be assigned
the result of Expr. This being so, we generate code that makes Var share
directly with the variable holding the result of Expr. This is an example
of what I call 'variable deletion' (see under trans_expr). 

But see note under trans_guard( Var=Expr ). The deletion code has been
removed, since it leads to double evaluation.

The other is the translation of system predicates. Consider the guards
    assert( t(u) )
    not( found(F) )
    phrase( sentence, L )
    1 + 2 > A

We know that
    (a) The user probably doesn't want t(u) treated as an evaluable
        expression: it's a term to be asserted as it stands.
    (b) The same is true of the first argument of 'phrase'. However, the
        second is presumably a list expression which is to be
        evaluated.
    (c) The argument to 'not' is to be treated as a goal.
    (d) Prolog will evaluate the arguments to >, as long as they're
        the same kind of term as that which 'is' can evaluate.
To deal with these special cases, we have a table of special predicates
and their argument classes. If trans_guard detects such a predicate
(by 'is_special_guard') then it calls 'trans_special_guard' to translate
the whole guard. 'trans_special_guard' runs over each argument, looking
up its class and translating it accordingly.
*/


/* trans_guard( +Guard_G, -Guard_P, +Options ):
     Guard_P is the Prolog translation of GRIPS guard Guard_G.
*/
trans_guard( Var, Var, _ ) :-
    var( Var ), !.

trans_guard( nothing, true, _ ) :- !.

trans_guard( pr(Goal_P), Goal_P, _ ) :- !.

trans_guard( (Guard_G=>Acts_G), Goal_P, Opt ) :-
    !,
    trans_condguard( (Guard_G=>Acts_G), Goal_P, Opt ).

trans_guard( (Acts_G if Guard_G), Goal_P, Opt ) :-
    !,
    trans_condguard( (Acts_G if Guard_G), Goal_P, Opt ).

trans_guard( else(If_G,Out_G), Goal_P, Opt ) :-
    !,
    trans_condguard( else(If_G,Out_G), Goal_P, Opt ).

trans_guard( (Guard1_G or Guard2_G), (Guard1_P;Guard2_P), Opt ) :-
    !,
    trans_guard( Guard1_G, Guard1_P, Opt ),
    trans_guard( Guard2_G, Guard2_P, Opt ).

trans_guard( (Guard1_G and Guard2_G), Goal_P, Opt ) :-
    !,
    trans_guard( Guard1_G, Guard1_P, Opt ),
    trans_guard( Guard2_G, Guard2_P, Opt ),
    conjoin( Guard1_P, Guard2_P, Goal_P ).

/*
trans_guard( Var=Expr_G, ExprGoal_P, Opt ) :-
    var( Var ),
    nonvar( Expr_G ),
    !,
    trans_expr( Expr_G, Var, ExprGoal_P, Opt ).

trans_guard( Expr_G=Var, ExprGoal_P, Opt ) :-
    var( Var ),
    nonvar( Expr_G ),
    !,
    trans_expr( Expr_G, Var, ExprGoal_P, Opt ).

These clauses not used, since they cause double evaluation
in expressions like
  X where X= `a.
If we have them in, X gets bound to the result of evaluating
`a in the 'where', and trans_expr then gets called on the
occurrence of X before the 'where'. Since this is now bound,
Grips can't tell it was a variable, and generates code to
evaluate it again.
*/

trans_guard( Guard_G, Goal_P, Opt ) :-
    is_special_guard( Guard_G, Opt ),
    !,
    trans_special_guard( Guard_G, Goal_P, Opt ).

trans_guard( Guard_G, Goal_P, Opt ) :-
    /*  Default case - translate all the arguments, treating them
        as expressions. PreGoals_P will hold any goals which are
        necessary to evaluate these arguments. Conjoin these goals
        with MainGoal_P, which is the goal corresponding to
        Guard_G itself, and return that as the result.
    */
    Guard_G =.. [ F | GuardsArgs_G ],
    trans_arglist( GuardsArgs_G, GuardsArgs_P, PreGoals_P, Opt ),
    MainGoal_P =.. [ F | GuardsArgs_P ],
    conjoin( PreGoals_P, MainGoal_P, Goal_P ).


/* trans_ge_guard( +Guard_G, -Guard_P, +Options ):
     Guard_P is the Prolog translation of GRIPS guard Guard_G.
*/
trans_ge_guard( Guard_G, Guard_P, Opt ) :-
    trans_ge_guard( Guard_G, Guard_P, S, Opt ),
    S = y.


/* trans_ge_guard( +Guard_G, -Guard_P, +Options ):
     Guard_P is the Prolog translation of GRIPS guard Guard_G.
*/
trans_ge_guard( Var, Var, n, _ ) :-
    var( Var ), !.

trans_ge_guard( Guard_G, Goal_P, S, Opt ) :-
    /*  Default case - translate all the arguments, treating them
        as expressions. PreGoals_P will hold any goals which are
        necessary to evaluate these arguments. Conjoin these goals
        with MainGoal_P, which is the goal corresponding to
        Guard_G itself, and return that as the result.
    */
    Guard_G =.. [ F | GuardsArgs_G ],
    trans_ge_arglist( GuardsArgs_G, GuardsArgs_P, PreGoals_P, S, Opt ),
    MainGoal_P =.. [ F | GuardsArgs_P ],
    conjoin( PreGoals_P, MainGoal_P, Goal_P ).


/* trans_special_guard( +Guard_G, -Guard_P, +Options ):
     Guard_P is the Prolog translation of GRIPS guard Guard_G.
     Guard_G is one of the special guards defined by is_special_guard.
*/
trans_special_guard( Guard_G, Goal_P, Opt ) :-
    functor( Guard_G, F, Arity ),
    functor( Template, F, Arity ),
    is_special_predicate( Template, Opt ),
    /*  Template now contains the argument-class template for the predicate
        that is Guard_G's principal functor.
    */

    functor( MainGoal_P, F, Arity ),
    /*  MainGoal_P is now a structure with the same functor and arity as
        Guard_G, but with all the arguments uninstantiated.
        'trans_call_by_template' will instantiate them.
    */
    trans_call_by_template( Template, Arity, 1, Guard_G, PreGoals_P, MainGoal_P, Opt ),

    /*  PreGoals_P is now a (possibly empty) sequence of goals that evaluates
        the arguments in MainGoal_P. Conjoining it to MainGoal_P gives us
        our result.
    */
    conjoin( PreGoals_P, MainGoal_P, Goal_P ).


/* trans_call_by_template( +Template, +Arity, +ArgNo, +Guard_G, -PreGoal_P, +MainGoal_P, +Options ):
     Guard_G is a GRIPS guard with arity Arity. Template is a term with the same
     principal functor. Each argument of Template is an atom indicating how the
     corresponding argument of Guard_G is to be translated. Their meanings are
     listed near the end of this file.
     trans_call_by_template instantiates PreGoal_P to a Prolog goal which will
     perform the evaluation of that argument, binding the result to the ArgNo'th
     argument of MainGoal_P.
*/
trans_call_by_template( Template, Arity, ArgNo, Guard_G, PreGoal_P, MainGoal_P, Opt ) :-
    ArgNo =< Arity,
    !,

    arg( ArgNo, Template, Form ),
    /*  Form is the argument class for argument ArgNo.  */

    arg( ArgNo, Guard_G, Arg_G ),
    /*  Arg_G is the corresponding argument itself.  */

    trans_arg( Form, Arg_G, ArgsGoal_P, Arg_P, Opt ),
    /*  Now ArgsGoal_P is a (possibly empty) goal to evaluate variable(s)
        occuring in Arg_P.
    */

    arg( ArgNo, MainGoal_P, Arg_P ),
    /*  Make Arg_P the corresponding argument of the final goal.  */

    NextArgNo is ArgNo + 1,
    trans_call_by_template( Template, Arity, NextArgNo, Guard_G, RestArgGoals_P, MainGoal_P, Opt ),
    conjoin( ArgsGoal_P, RestArgGoals_P, PreGoal_P ).

trans_call_by_template( _, _, _, _, true, _, _ ).
    /*  If we've finished, just return a dummy pre-goal and leave the main
        goal alone.
    */
%
% !!! We can make the above template-using code shorter, as I've
% done for trans_expr.


/* trans_arg( +ArgType, +Arg_G, -ArgGoal_P, -Arg_P, +Options ):
     Arg_G is an argument to a GRIPS term, and is of type ArgType.
     Arg_P is the corresponding argument in the Prolog translation
     of Arg_G, and ArgGoal_P is a goal that evaluates it.
*/
trans_arg( i, Arg_G, true, Arg_G, _ ) :- !.

trans_arg( e, Arg_G, ArgGoal_P, Arg_P, Opt ) :-
    !,
    trans_expr( Arg_G, Arg_P, ArgGoal_P, Opt ).

trans_arg( g, Arg_G, true, Arg_P, Opt ) :-
    !,
    trans_guard( Arg_G, Arg_P, Opt ).

trans_arg( a, Arg_G, ArgGoal_P, Arg_P, Opt ) :-
    !,
    trans_arith( Arg_G, Arg_P, ArgGoal_P, Opt ).


/*
Translating expressions.
------------------------

The main predicate for this is 'trans_expr( Expr, V, Goal )'. Here, Expr
is the GRIPS expression. V will become a term which is the result of
evaluating Expr, once Goal has been called. Thus, once you've done
    call( Goal )
then V contains the result of evaluating Expr.


Implementation.
---------------

'trans_expr' may call 'trans_guard'. To translate lists, it calls
'trans_list'; to translate arithmetic expressions that can be evaluated
by "is" (such as 2+4), it calls 'trans_arith'; and to translate the
arguments of a general expression, it calls 'trans_arglist', which
recursively calls 'trans_expr'. To translate conditional expressions,
it calls 'trans_condexpr'.

The idea behind GRIPS is that each GRIPS expression is translated into
a goal/term pair <G;T>. The term T is the expression itself. If this
expression is not ground, then the goal G binds some or all of the variables
in T.

For example:
                Expression          Goal G          Term T
                ----------          ------          ------
                a                   null            a
                1                   null            1
                eval(a)             a(V)            V
                f(x)                f(x,V)          V
                1+2                 V is 1+2        V
                L1++L2              append(L1,L2,V) V
                `(f(x))             null            f(x)
In general, if T is fully ground, G should be null. If G is not null, then
T contains at least one variable (in this version of GRIPS, it will always
_be_ a variable).

'trans_expr(E,T,G)' performs this translation. G will become the goal. If
null, it's represented by 'true'. If non-null, it may be a conjunction
(or alternation) of sub-goals. T becomes the term.

Given this, the translation of function heads is easy. To translate
    f <- rhs
we translate rhs, giving a goal G and a term T. We then insert T as the
final argument of f, and give it the tail G:
    f(T) :- G
This inserts results directly into f's argument, so that for example
    f(N) <- 2
becomes
    f(N,2).

A point that's important in translating conditional expressions is that it's
possible to do what I call 'variable introduction'. The pair
    <G;T>
is equivalent to
    < (G,V=T) ; V >
where V is a new variable that does not occur in G or T. Usually, we don't
want unnecessary equalses. However, in translating branches of an if..else,
we have to make sure that all the result expressions are variables. This
is 'trans_nonfree_expr' does. You can see why if you think about translating
    f(N) <- 1 if p(X) else 2
You can't insert the 1 or 2 directly into f's head; instead, you must
introduce a common variable which can be set by either branch:
    f(N,V) :- ( p(X), !, V=1 ; V=2 ).

The converse of variable introduction is variable deletion, where we can
optimise out a subgoal of the form
    V=W
and replace following uses of V by W. This is done by trans_guard.
*/


/* trans_expr( +Expr_G, -Expr_P, -ExprGoal_P, +Options ):
     Expr_G is a GRIPS expression. Expr_P is the corresponding
     Prolog term, and ExprGoal_P is the goal that evaluates it.
*/
trans_expr( Expr_G, Expr_G, true, _ ) :-
    var( Expr_G ), !.

trans_expr( Expr_G, Expr_P, ExprGoal_P, Opt ) :-
    memberchk( evaluate_specials_with(Pred), Opt ),
    call( Pred, Expr_G, Expr_P, ExprGoal_P ),
    !.

trans_expr( Expr_G, Expr_G, true, _ ) :-
    number( Expr_G ), !.

trans_expr( `(Expr_G), Expr_P, ExprGoal_P, Opt ) :- 
    !,
    trans_quoted_expr( Expr_G, Expr_P, ExprGoal_P, Opt ).

trans_expr( eval(Atom_G), V, Goal_P, _Opt ) :-
    ( atom( Atom_G ) ),
    !,
    Goal_P =.. [ Atom_G, V ].

trans_expr( eval(Expr_G), Expr_P, ExprGoal_P, Opt ) :-
    !,
    trans_expr( Expr_G, Expr_P, ExprGoal_P, Opt ).

trans_expr( 'CONS'(Expr_G), Expr_P, ExprGoal_P, Opt ) :- 
    !,
    trans_half_quoted_expr( Expr_G, Expr_P, ExprGoal_P, Opt ).

trans_expr( (Guard_G=>Acts_G), Expr_P, Goal_P, Opt ) :-
    !,
    trans_condexpr( (Guard_G=>Acts_G), Expr_P, Goal_P, Opt ).

trans_expr( (Acts_G if Guard_G), Expr_P, Goal_P, Opt ) :-
    !,
    trans_condexpr( (Acts_G if Guard_G), Expr_P, Goal_P, Opt ).

trans_expr( else(If_G,Out_G), Expr_P, Goal_P, Opt ) :-
    !,
    trans_condexpr( else(If_G,Out_G), Expr_P, Goal_P, Opt ).

trans_expr( Expr_G where Guard_G, Expr_P, ExprGoal_P, Opt ) :-
    !,
    trans_guard( Guard_G, Guard_P, Opt ),
    trans_expr( Expr_G, Expr_P, ExprMainGoal_P, Opt ),
    conjoin( Guard_P, ExprMainGoal_P, ExprGoal_P ).
    % Put this before the all..where case because Expr_G could be a variable.

trans_expr( [], [], true, _ ) :- !.

trans_expr( [H|T], Expr_P, ExprGoal_P, Opt ) :-
    trans_list( [H|T], Expr_P, ExprGoal_P, Opt ), !.

trans_expr( Expr_G, V, ExprGoal_P, Opt ) :-
    is_arithmetic_call( Expr_G ), memberchk( arithmetic_calls_is, Opt ), 
    !,
    trans_arith( Expr_G, ExprsIsPart_P, ExprPreGoal_P, Opt ),
    conjoin( ExprPreGoal_P, V is ExprsIsPart_P, ExprGoal_P ).

trans_expr( M:Expr_G, Expr_P, M:ExprGoal_P, Opt ) :-
    !,
    trans_expr( Expr_G, Expr_P, ExprGoal_P, Opt ).

trans_expr( twist Expr_G, Expr_P, ExprGoal_P, Opt ) :-
    Expr_G =.. [ F | Args_G ],
    ( functor( Expr_G, F, Arity ),
      functor( Template, F, Arity ),
      memberchk( mode(Template), Opt ) ->
        Template =.. [ _ | TemplateAsList ],
        trans_templated_arglist( TemplateAsList, Args_G, Args_P, ArgsGoal_P, Opt )
    ;
        trans_arglist( Args_G, Args_P, ArgsGoal_P, Opt )
    ),
    ArgsAndResult_P = [ Expr_P | Args_P ],
    ExprMainGoal_P =.. [ F | ArgsAndResult_P ],
    conjoin( ArgsGoal_P, ExprMainGoal_P, ExprGoal_P ).
%
% Not sure whether we'll extend this. Makes the result arg the first,
% rather than the final.

trans_expr( Expr_G, Expr_P, ExprGoal_P, Opt ) :-
    /*  This is the default case. We assume that Expr_G is to be taken
        as a function call. To turn this call into a Prolog goal, we
        do two things. (1) translate the arguments, generating code
        for pre-evaluating them if necessary. (2) Append a variable to
        the final argument list: that will hold the result of the
        call.
    */
    Expr_G =.. [ F | Args_G ],
    ( functor( Expr_G, F, Arity ),
      functor( Template, F, Arity ),
      memberchk( mode(Template), Opt ) ->
        Template =.. [ _ | TemplateAsList ],
        trans_templated_arglist( TemplateAsList, Args_G, Args_P, ArgsGoal_P, Opt )
    ;
        trans_arglist( Args_G, Args_P, ArgsGoal_P, Opt )
    ),
    append( Args_P, [Expr_P], ArgsAndResult_P ),
    ExprMainGoal_P =.. [ F | ArgsAndResult_P ],
    conjoin( ArgsGoal_P, ExprMainGoal_P, ExprGoal_P ).


/* trans_templated_arglist( +Template, +ArgList_G, -ArgList_P, -ArgListGoal_P, +Options ):
     Like trans_arglist, but the first argument is a list of argument
     specifiers stating how each argument in ArgList is to be translated.
*/
trans_templated_arglist( [ A1_T | An_T ], [ A1_G | An_G ], [ A1_P | An_P ], Goal_P, Opt ) :-
    trans_templated_arglist( An_T, An_G, An_P, AnGoal_P, Opt ),
    trans_arg( A1_T, A1_G, A1Goal_P, A1_P, Opt ),
    conjoin( A1Goal_P, AnGoal_P, Goal_P ).

trans_templated_arglist( [], [], [], true, _ ).


trans_quoted_expr( Expr_G, Expr_G, true, _ ) :-
    ( var( Expr_G ) ; atomic( Expr_G ) ), !.

trans_quoted_expr( eval(Expr_G), Expr_P, ExprGoal_P, Opt ) :- 
    !,
    trans_expr( Expr_G, Expr_P, ExprGoal_P, Opt ).

trans_quoted_expr( Expr_G, Expr_P, ArgsGoal_P, Opt ) :- 
    Expr_G =.. [ F | Args_G ],
    trans_quoted_arglist( Args_G, Args_P, ArgsGoal_P, Opt ),
    Expr_P =.. [ F | Args_P ].


trans_half_quoted_expr( Expr_G, Expr_G, true, _ ) :-
    ( var( Expr_G ) ; atomic( Expr_G ) ), !.

trans_half_quoted_expr( eval(Expr_G), Expr_P, ExprGoal_P, Opt ) :- 
    !,
    trans_expr( Expr_G, Expr_P, ExprGoal_P, Opt ).

trans_half_quoted_expr( Expr_G, Expr_P, ArgsGoal_P, Opt ) :- 
    Expr_G =.. [ F | Args_G ],
    trans_arglist( Args_G, Args_P, ArgsGoal_P, Opt ),
    Expr_P =.. [ F | Args_P ].


/* trans_ge_expr( +Expr_G, -Expr_P, -ExprGoal_P, +Options ):
     Expr_G is an expression for use in goal_expansion-expanded
     goals. Expr_P is the corresponding
     Prolog term, and ExprGoal_P is the goal that evaluates it.
*/
trans_ge_expr( Expr_G, Expr_G, true, n, _ ) :-
    ( var( Expr_G ) ; atomic( Expr_G ) ), !.

trans_ge_expr( Expr_G, Expr_P, ExprGoal_P, y, Opt ) :-
    member( expression_prefix(Prefix), Opt ),
    Expr_G =.. [ Prefix, Expr_G1 ],
    !,
    trans_expr( Expr_G1, Expr_P, ExprGoal_P, Opt ).

trans_ge_expr( Expr_G, Expr_P, ArgsGoal_P, S, Opt ) :- 
    Expr_G =.. [ F | Args_G ],
    trans_ge_arglist( Args_G, Args_P, ArgsGoal_P, S, Opt ),
    Expr_P =.. [ F | Args_P ].


/* trans_list( +List_G, -List_P, -ListGoal_P, +Options ):
     List_G is a GRIPS expression which is a list. List_P is the 
     corresponding Prolog list, and ListGoal_P is the goal that evaluates it.
*/
trans_list( Var, Var, true, _ ) :-
    var( Var ),
    !.

trans_list( [], [], true, _ ) :- !.

trans_list( [H_G|T_G], [H_P|T_P], Goal_P, Opt ) :-
    !,
    trans_expr( H_G, H_P, HGoal_P, Opt ),
    (
        trans_list( T_G, T_P, TGoal_P, Opt )
    ->
        true
    ;
        trans_expr( T_G, T_P, TGoal_P, Opt )
    ),
    conjoin( HGoal_P, TGoal_P, Goal_P ).
    /*  This conditional takes care of lists such as
            [ H | f(T) ]
        where the tail is not explicitly a list, and hence
        must be handled by trans_expr.
    */


/* trans_arglist( +ArgList_G, -ArgList_P, -ArgListGoal_P, +Options ):
     ArgList_G is a list of GRIPS expression arguments. ArgList_P is the 
     corresponding Prolog list, and ArgListGoal_P is the goal that 
     evaluates it.
*/
trans_arglist( [A1_G | An_G ], [ A1_P | An_P ], Goal_P, Opt ) :-
    trans_arglist( An_G, An_P, AnGoal_P, Opt ),
    trans_expr( A1_G, A1_P, A1Goal_P, Opt ),
    conjoin( A1Goal_P, AnGoal_P, Goal_P ).

trans_arglist( [], [], true, _ ).


/* trans_quoted_arglist( +ArgList_G, -ArgList_P, -ArgListGoal_P, +Options ):
     As trans_arglist, but applies trans_quoted_expr to the
     arguments.
*/
trans_quoted_arglist( [A1_G | An_G ], [ A1_P | An_P ], Goal_P, Opt ) :-
    trans_quoted_arglist( An_G, An_P, AnGoal_P, Opt ),
    trans_quoted_expr( A1_G, A1_P, A1Goal_P, Opt ),
    conjoin( A1Goal_P, AnGoal_P, Goal_P ).

trans_quoted_arglist( [], [], true, _ ).


/* trans_ge_arglist( +ArgList_G, -ArgList_P, -ArgListGoal_P, +Options ):
     ArgList_G is a list of expression arguments for use in
     goal_expansion-expanded goals. ArgList_P is the 
     corresponding Prolog list, and ArgListGoal_P is the goal that 
     evaluates it.
*/
trans_ge_arglist( [A1_G | An_G ], [ A1_P | An_P ], Goal_P, S, Opt ) :-
    trans_ge_arglist( An_G, An_P, AnGoal_P, Sn, Opt ),
    trans_ge_expr( A1_G, A1_P, A1Goal_P, S1, Opt ),
    conjoin( A1Goal_P, AnGoal_P, Goal_P ),
    ( Sn = y ; S1 = y ->
        S = y
    ; 
        S = n
    ).

trans_ge_arglist( [], [], true, n, _ ).


/*
Expressions in 'is'.
--------------------

If an expression is something like X+Y or sin(2*pi), i.e. something that
has the correct arity and functor to be evaluated by 'is', then it's
translated by 'trans_arith( Expr, IsPart, Goal )'. Here, Expr is the
arithmetic expression. IsPart becomes a term which can be evaluated
by doing
    V is IsPart
and Goal becomes a goal which pre-evaluates any variables in IsPart.
So to evaluate Expr, you do
    call( Goal ), V is IsPart
which puts the result into V.

Essentially, given an arithmetic expression, 'trans_arith' does NOT
unwind its arguments if they themselves are arithmetic expressions. In
this, it differs from 'trans_expr'. Given
    2 + 3*5
'trans_expr' would (if it didn't call 'trans_arith' to handle such
cases) generate calls of the form
    V is 3*5, V' is 2+V
    result variable = V'

However, 'trans_arith' would optimise.
*/


/* trans_arith( +Expr_G, -Expr_P, -ExprGoal_P, Opt ):
     Expr_G is a GRIPS expression which can be evaluated by 'is'. Expr_P 
     is the corresponding Prolog arithmetic expression, and ExprGoal_P 
     is the goal that evaluates it.
*/
trans_arith( Expr_G, Expr_G, true, _ ) :-
    ( number(Expr_G) ; var(Expr_G) ), !.

trans_arith( Expr_G, Expr_P, ArgsGoal_P, Opt ) :-
    is_arithmetic_call( Expr_G ),
    Expr_G =.. [ F | Args_G ],
    trans_arith_args( Args_G, Args_P, ArgsGoal_P, Opt ),
    Expr_P =.. [ F | Args_P ], !.

trans_arith( Expr_G, Expr_P, ExprGoal_P, Opt ) :-
    trans_expr( Expr_G, Expr_P, ExprGoal_P, Opt ).


/* trans_arith_args( +Args_G, -Args_P, -ArgsGoal_P, +Options ):
     Args_G is a list of arguments to a term translatable by
     trans_arith. Args_P is its translation into Prolog, and
     ArgsGoal_P is a goal that will evaluate them.
*/
trans_arith_args( [], [], true, _ ) :- !.

trans_arith_args( [A1_G|An_G], [A1_P|An_P], ArgsGoal_P, Opt ) :-
    trans_arith( A1_G, A1_P, A1Goal_P, Opt ),
    trans_arith_args( An_G, An_P, AnGoal_P, Opt ),
    conjoin( A1Goal_P, AnGoal_P, ArgsGoal_P ).


/*
Translating conditional expressions.
------------------------------------

These expressions are translated in two stages. The first removes the
operators defining the surface syntax and builds a list of the form
    [ Cond1, Expr1, Cond2, Expr2 ... ]
where the final condition may be 'true' if the expression finishes with
a default result. This is done by ifelses_to_list.

The second stage translates this list to a goal/term pair. As discussed
under "Translating expressions", if the list has more than one alternative,
we need to make all the alternative results assigned to a common variable.
This is done by 'trans_condexprlist_1'.

There are analogues for translating conditional guards.
*/


/* ifelses_to_list( +Cond, +In, -Out ):
     Cond is a GRIPS conditional expression. Out is this
     expression represented as a list, appended to the
     list In (an accumulator argument). In the initial call of 
     this predicate, In would normally be [].
*/
ifelses_to_list( else(E1,E2), In, Out ) :-
    !,
    check_syntax( (E1=if(_,_);E1=(_=>_);E1=else(_,_)), 'No IF before ELSE', [] ),
    ifelses_to_list( E2, In, In1 ),
    ifelses_to_list( E1, In1, Out ).

ifelses_to_list( if(Then,Cond), In, [ Cond, Then | In ] ) :-
    !.

ifelses_to_list( =>(Cond,Then), In, [ Cond, Then | In ] ) :-
    !.

ifelses_to_list( Default, In, [ true, Default | In ] ) :-
    !.


/* trans_condexpr( +Expr_G, -Expr_P, -ExprGoal_P, +Options ):
     Expr_G is a GRIPS conditional expression. Expr_P is the 
     corresponding Prolog term, and ExprGoal_P is the goal that evaluates it.
*/
trans_condexpr( Expr_G, Expr_P, ExprGoal_P, Opt ) :-
    ifelses_to_list( Expr_G, [], List ),
    trans_condexprlist( List, Expr_P, ExprGoal_P, Opt ).


/* trans_condexprlist( +ExprAsList_G, -ExprAsList_P, -ExprGoal_P, +Options ):
     ExprAsList_G is a GRIPS conditional expression represented as a 
     list. ExprAsList_P is the corresponding Prolog term, and ExprGoal_P is 
     the goal that evaluates it.
*/
trans_condexprlist( [true,Expr_G|_], Expr_P, ExprGoal_P, Opt ) :-
    !,
    trans_expr( Expr_G, Expr_P, ExprGoal_P, Opt ).

trans_condexprlist( [Guard_G,Expr_G], Expr_P, ExprGoal_P, Opt ) :-
    !,
    trans_guard( Guard_G, Guard_P, Opt ),
    trans_expr( Expr_G, Expr_P, ExprMainGoal_P, Opt ),
    conjoin( Guard_P, !, ExprMainGoal_P, ExprGoal_P ).

trans_condexprlist( L, V, Goal, Opt ) :-
    !,
    trans_condexprlist_1( L, V, Goal, Opt ).


/* trans_condexprlist_1( +ExprAsList_G, -ExprAsList_P, -ExprGoal_P, +Options ):
     This behaves like trans_condexprlist, but the translation
     of ExprAsList_G is always a variable. If necessary, a new
     variable and an instantiation is introduced to ensure
     this.
*/
trans_condexprlist_1( [true,Expr_G|_], V, ExprGoal_P, Opt ) :-
    !,
    trans_nonfree_expr( Expr_G, V, ExprGoal_P, Opt ).

trans_condexprlist_1( [Guard_G,Expr_G], V, ExprGoal_P, Opt ) :-
    !,
    trans_guard( Guard_G, Guard_P, Opt ),
    trans_nonfree_expr( Expr_G, V, ExprMainGoal_P, Opt ),
    conjoin( Guard_P, !, ExprMainGoal_P, ExprGoal_P ).

trans_condexprlist_1( [Guard_G,Expr_G|Rest], V, Goal_P, Opt ) :-
    !,
    trans_guard( Guard_G, Guard_P, Opt ),
    trans_nonfree_expr( Expr_G, V, ExprMainGoal_P, Opt ),
    conjoin( Guard_P, !, ExprMainGoal_P, ExprGoal_P ),
    trans_condexprlist_1( Rest, V, RestGoal_P, Opt ),
    Goal_P = ( ExprGoal_P ; RestGoal_P ).


/* trans_nonfree_expr( +Expr_G, -ResultVar, -ExprGoal_P, +Options ):
     This behaves like trans_expr, but the translation
     of Expr_G is always a variable. If necessary, a new
     variable and an instantiation is introduced to ensure
     this.
*/
trans_nonfree_expr( Expr_G, ResultVar, ExprGoal_P, Opt ) :-
    trans_expr( Expr_G, Expr_P, ExprMainGoal_P, Opt ),
    (
        var(Expr_P)
    ->
        ResultVar = Expr_P,
        ExprGoal_P = ExprMainGoal_P
    ;
        conjoin( ExprMainGoal_P, ResultVar=Expr_P, ExprGoal_P )
    ).


/* trans_condguard( +Guard_G, -Guard_P, +Options ):
     Guard_G is a GRIPS guard which is a conditional expression 
     (made from 'if', =>, 'else'). Guard_P is its translation
     as a Prolog goal.
*/
trans_condguard( Guard_G, GuardGoal_P, Opt ) :-
    ifelses_to_list( Guard_G, [], List ),
    trans_condguardlist( List, GuardGoal_P, Opt ).


/* trans_condguardlist( +GuardAsList_G, -Guard_P, +Options ):
     GuardAsList_G is a GRIPS guard which is a conditional expression,
     represented as a list. Guard_P is its translation
     as a Prolog goal.
*/
trans_condguardlist( [true,Act_G|_], ActGoal_P, Opt ) :-
    !,
    trans_guard( Act_G, ActGoal_P, Opt ).

trans_condguardlist( [Guard_G,Act_G], ActGoal_P, Opt ) :-
    !,
    trans_guard( Guard_G, Guard_P, Opt ),
    trans_guard( Act_G, ActMainGoal_P, Opt ),
    conjoin( Guard_P, !, ActMainGoal_P, ActGoal_P ).

trans_condguardlist( L, Goal, Opt ) :-
    !,
    trans_condguardlist_1( L, Goal, Opt ).


/* trans_condguardlist_1( +GuardAsList_G, -Guard_P, +Options ):
     This behaves like trans_condguardlist, but the translation
     of GuardAsList_G is always a variable. If necessary, a new
     variable and an instantiation is introduced to ensure
     this.
*/
trans_condguardlist_1( [true,Act_G|_], ActGoal_P, Opt ) :-
    !,
    trans_guard( Act_G, ActGoal_P, Opt ).

trans_condguardlist_1( [Guard_G,Act_G], ActGoal_P, Opt ) :-
    !,
    trans_guard( Guard_G, GuardGoal_P, Opt ),
    trans_guard( Act_G, ActMainGoal_P, Opt ),
    conjoin( GuardGoal_P, !, ActMainGoal_P, ActGoal_P ).

trans_condguardlist_1( [Guard_G,Act_G|Rest], Goal_P, Opt ) :-
    !,
    trans_guard( Guard_G, Guard_P, Opt ),
    trans_guard( Act_G, ActMainGoal_P, Opt ),
    conjoin( Guard_P, !, ActMainGoal_P, ActGoal_P ),
    trans_condguardlist_1( Rest, RestGoal_P, Opt ),
    Goal_P = ( ActGoal_P ; RestGoal_P ).


/*
Classifying predicates and functors.
------------------------------------

There are things GRIPS needs to know about the Prolog it's running on:

1. Predicates and their arguments.

    It needs to know, for built-in predicates, whether the arguments are
    to be left alone (as for assert), treated as goals (as for not),
    treated as expressions (as for the 2nd argument of phrase), or
    treated as arithmetic expressions (as for <). 'is_special_predicate'
    classifies accordingly. The meaning of the argument letters is:
        i = leave alone (Identity).
        g = goal.
        e = expression.
        a = arithmetic expression.
    The predicates listed are those I use in SWI-Prolog: YOU MAY NEED TO
    CHANGE THE DEFINITION OF is_special_predicate FOR YOUR SYSTEM.

2. Functors used by "is".

    When translating arithmetic expressions, GRIPS needs to know which
    functor/arity combinations are recognised by "is". This information
    is given by 'is_arithmetic_call', using an SWI-specific predicate.
    YOU MAY NEED TO CHANGE THE DEFINITION OF is_arithmetic_call FOR YOUR SYSTEM.

There is one other predicate built on these, to recognise terms
containing the special predicates or functors. 'is_special_guard'
succeeds if the argument is a term whose principal functor satisfies
'is_special_predicate'.
*/


/* is_special_guard( +E, +options ):
     E is a GRIPS term to be evaluated specially, rather than
     by the default method of evaluating all its arguments.
     is_special_guard creates a copy of the term with all its
     arguments unbound, and instantiates each one to an atom indicating
     what kind of thing the predicate expects in that argument position,
     and hence how GRIPS should translate it.
*/
is_special_guard( G, Opt ) :-
    functor( G, F, A ),
    functor( Copy, F, A ),
    is_special_predicate( Copy, Opt ).


/* is_special_predicate( +T, +Options ):
     E is a term with principal functor and arity the same as a predicate
     we need to evaluate specially, and all arguments unbound.
     is_special_predicate instantiates each argument to an atom indicating
     what kind of thing the predicate expects in that argument position,
     and hence how GRIPS should translate it.
*/
is_special_predicate( T, Opt ) :-
  memberchk( mode(T), Opt ), !.
is_special_predicate( T, _ ) :-
  is_special_predicate( T ).


is_special_predicate( assert(i) ).
is_special_predicate( asserta(i) ).
is_special_predicate( assertz(i) ).
is_special_predicate( retract(i) ).
is_special_predicate( retractall(i) ).

is_special_predicate( phrase(i,e) ).

is_special_predicate( call(g) ).
is_special_predicate( not(g) ).
is_special_predicate( ','(g,g) ).
is_special_predicate( ;(g,g) ).

is_special_predicate( a>a ).
is_special_predicate( a<a ).
is_special_predicate( a>=a ).
is_special_predicate( a=<a ).
is_special_predicate( a=:=a ).
is_special_predicate( a=\=a ).


/* is_arithmetic_call( +E ):
     E is a GRIPS expression.
     The predicate succeeds if it has the right functor and arity
     to be evaluated by 'is'.
*/
is_arithmetic_call( E ) :-
    current_arithmetic_function( E ).


:- multifile
        user:term_expansion/2.
:- dynamic
        user:term_expansion/2.

user:term_expansion( A, B ) :-
    grips_options( Opt ),
    trans_def( A, B, Opt ).
%
% Adds a clause expanding GRIPS definitions
% to term_expansion. See discussion on the SWI
% Prolog mailing list for December 9-10 2003.


:- multifile
        user:goal_expansion/2.
:- dynamic
        user:goal_expansion/2.

user:goal_expansion( A, B ) :- 
  grips_options( Opt ),
  trans_ge_guard( A, B, Opt ).




