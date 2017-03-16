:- module(term_basic, [], [pure, assertions, isomodes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_typing), [nonvar/1]).

:- doc(title,"Basic term manipulation").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"This module provides basic term manipulation.").

:- true pred copy_term(Term, Copy) + ( sideff(free), native, iso )

        # "@var{Copy} is a renaming of @var{Term}, such that brand new
           variables have been substituted for all variables in
           @var{Term}.  If any of the variables of @var{Term} have
           @concept{attributes}, the copied variables will have copies
           of the attributes as well. It behaves as if defined by:

@begin{verbatim}
:- data 'copy of'/1.

copy_term(X, Y) :-
        asserta_fact('copy of'(X)),
        retract_fact('copy of'(Y)).
@end{verbatim}".

:- '$native_include_c_source'(.(term_basic)).

:- export(copy_term/2).
:- '$props'(copy_term/2, [impnat=cbool(prolog_copy_term)]).

% I added this predicate to copy a term, but sharing attributed variables.
% That is required for mutable variables implemented with attributes variables. 
% -- Jose F. Morales
:- export(copy_term_shattr/2).
:- '$props'(copy_term_shattr/2, [impnat=cbool(prolog_copy_term_shattr)]).

% Compiled inline -- these provide hooks for the interpreter and comments.

:- export((=)/2).
:- true prop '='(?X,?Y) + ( sideff(free), native, iso )
	# "@var{X} and @var{Y} unify.".
:- '$allow_def'((=)/2).
X=Y :- X=Y.

:- export((\=)/2).
:- true prop \=(?X,?Y) + ( sideff(free), native, iso )
	# "@var{X} and @var{Y} are not unifiable.".
% TODO: Document -- using negation as failure
X \= X :- !, fail.
_ \= _.

:- export(arg/3).
:- true pred arg(+ArgNo,+Term,?Arg)
	: int(ArgNo) + ( sideff(free), native, iso )
	# "Argument @var{ArgNo} of the term @var{Term} is @var{Arg}.".
:- '$props'(arg/3, [impnat=cfunre(fu2_arg,no)]).

:- export(functor/3).
:- true pred functor(?Term,?Name,?Arity) => ( atm(Name), num(Arity) )
	+ ( sideff(free), native, iso )
        # "The principal functor of the  term @var{Term} has name @var{Name}
           and arity @var{Arity}.".
:- '$props'(functor/3, [impnat=cblt(bu3_functor,256)]).

:- export((=..)/2).
:- true pred (?Term =.. ?List) => list(List) + ( sideff(free), native, iso )
	# "The functor and arguments of the term @var{Term} comprise the 
           list @var{List}.".
:- '$props'('=..'/2, [impnat=cblt(bu2_univ,512)]).

:- export('C'/3).
:- true pred 'C'(?S1,?Terminal,?S2) + ( sideff(free), native )
	# "@var{S1} is connected by the terminal @var{Terminal} to @var{S2}.
           Internally used in @em{DCG grammar rules}. Defined as if by the 
           single clause: @tt{'C'([X|S], X, S).}".
% This predicate is expanded by the compiler
'C'(X, Y, Z) :- 'C'(X, Y, Z).

% TODO: document, used in ptoc
:- export('$trust_type'/2).
:- '$props'('$trust_type'/2, [impnat = ptoc_builtin]).
:- export('$varmem'/2).
:- '$props'('$varmem'/2, [impnat = ptoc_builtin]).
:- export('$trust_imptype'/2).
:- '$props'('$trust_imptype'/2, [impnat = ptoc_builtin]).
:- export('$check_test_str'/2).
:- '$props'('$check_test_str'/2, [impnat = ptoc_builtin]).
:- export('$bind'/2).
:- '$props'('$bind'/2, [impnat = ptoc_builtin]).
:- export('$equal'/2).
:- '$props'('$equal'/2, [impnat = ptoc_builtin]).
:- export('$instance'/2).
:- '$props'('$instance'/2, [impnat = ptoc_builtin]).
:- export('$unify'/2).
:- '$props'('$unify'/2, [impnat = ptoc_builtin]).
:- export('$trail_if_conditional'/1).
:- '$props'('$trail_if_conditional'/1, [impnat = ptoc_builtin]).
