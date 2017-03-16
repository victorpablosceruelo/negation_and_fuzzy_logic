:- module(term_compare, [], [pure, assertions, isomodes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).

:- doc(title,"Comparing terms").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "These built-in predicates are extra-logical. They
treat uninstantiated variables as objects with values which may be
compared, and they never instantiate those variables. They should
@em{not} be used when what you really want is arithmetic comparison or
unification.

The predicates make reference to a @index{standard total ordering} of terms,
which is as follows:

@begin{itemize}

@item Variables, by age (roughly, oldest first -- the order is @em{not}
 related to the names of variables).

@item Floats, in numeric order (e.g. -1.0 is put before 1.0). 

@item Integers, in numeric order (e.g. -1 is put before 1). 

@item Atoms, in alphabetical (i.e. character code) order. 

@item Compound terms, ordered first by arity, then by the name of the
    principal functor, then by the arguments in left-to-right
    order. Recall that lists are equivalent to compound terms with
    principal functor @tt{'.'/2}.

@end{itemize}

For example, here is a list of terms in standard order: 

@begin{verbatim}
[ X, -1.0, -9, 1, bar, foo, [1], X = Y, foo(0,2), bar(1,1,1) ]
@end{verbatim}
").

:- '$native_include_c_source'(.(term_compare)).

:- export((==)/2).
:- prop (Term1 == Term2)
	# "The terms @var{Term1} and @var{Term2} are strictly identical.".
:- true comp (@Term1 == @Term2) + ( sideff(free), native ).
:- '$props'('=='/2, [impnat=cblt(bu2_lexeq,m(0,0))]).

:- export((\==)/2).
:- true pred (@Term1 \== @Term2) + ( sideff(free), native )
	# "The terms @var{Term1} and @var{Term2} are not strictly identical.".
:- '$props'((\==)/2, [impnat=cblt(bu2_lexne,m(0,0))]).

:- export((@<)/2).
:- true pred (@Term1 @< @Term2) + ( sideff(free), native )
	# "The term @var{Term1} precedes the term @var{Term2} in the 
           standard order.".
:- '$props'((@<)/2, [impnat=cblt(bu2_lexlt,m(0,0))]).

:- export((@=<)/2).
:- true pred (@Term1 @=< @Term2) + ( sideff(free), native )
	# "The term @var{Term1} precedes or is identical to the term
           @var{Term2} in the standard order.".
:- '$props'((@=<)/2, [impnat=cblt(bu2_lexle,m(0,0))]).

:- export((@>)/2).
:- true pred (@Term1 @> @Term2) + ( sideff(free), native )
	# "The term @var{Term1} follows the term @var{Term2} in the 
           standard order.".
:- '$props'((@>)/2, [impnat=cblt(bu2_lexgt,m(0,0))]).

:- export((@>=)/2).
:- true pred (@Term1 @>= @Term2) + ( sideff(free), native )
	# "The term @var{Term1} follows or is identical to the term
           @var{Term2} in the standard order.".
:- '$props'((@>=)/2, [impnat=cblt(bu2_lexge,m(0,0))]).

:- export('$compare'/3).
:- '$props'('$compare'/3, [impnat=cfun(fu2_compare,no)]).

:- export(compare/3).
:- doc(compare(Op,Term1,Term2) , "@var{Op} is the result of
           comparing the terms @var{Term1} and @var{Term2}.").
:- true pred compare(?atm,@term,@term)
	=> member([(=),(>),(<)]) * term * term + ( sideff(free), native ).
% the compiler does this translation automatically (jf)
compare(Value, X, Y) :- '$compare'(X, Y, Value). 
