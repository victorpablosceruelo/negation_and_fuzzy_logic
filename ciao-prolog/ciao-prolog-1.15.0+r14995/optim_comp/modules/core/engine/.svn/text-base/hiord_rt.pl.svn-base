:- module(hiord_rt, [], [pure, assertions, isomodes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(exceptions)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).

:- doc(title,"Higher-order").

:- doc(author,"Daniel Cabeza Gras").

:- doc(module,"This module is a wrapper to the implementation
   defined predicate @pred{call/1}, and implements the @pred{call/2}
   predicate.").

% ---------------------------------------------------------------------------
:- export(call/1).
:- doc(call(G), "Executes goal @var{G}, restricting the scope of
   the cuts to the execution of @var{G}.  Equivalent to writing a
   variable @var{G} in a goal position.").

:- true pred call(+callable) + (iso, native). 
:- meta_predicate call(primitive(goal)).
:- '$props'(call/1, [impnat=cinsnp(code_call1)]).

% ---------------------------------------------------------------------------
:- export('SYSCALL'/1). % like call/1, but without the meta_predicate decl
:- '$props'('SYSCALL'/1, [impnat=cinsnp(code_call1)]).

% ---------------------------------------------------------------------------
:- export(call/2). /* call/N ? */ 
:- doc(call(Pred,Arg1), "There exists a set of builtin predicates
   of the form @pred{call/N} with @tt{N > 1} which execute predicate
   @var{Pred} given arguments @var{Arg1} ... @var{ArgX}. If @var{Pred}
   has already arguments @var{Arg1} is added to the start, the rest to
   the end. This predicate, when @var{Pred} is a variable, can be
   written using the special Ciao syntax @tt{Pred(Arg1,...,ArgX)}.").

% TODO: The assertion for this predicate call/2 was incorrectly
%       expanded, since it is natively treated by the system and
%       confused with call/N for N=2. A possible solution is to use a
%       completely different name (e.g. kall/2).

%:- true pred call(+callable,?) + native. 
:- meta_predicate(call(primitive(goal),?)).
call(V, Args) :- calln(V, Args).

%:- use_module(engine(io_basic)).
calln(V, _) :- var(V), !, throw(error(instantiation_error, call/n-1)).
calln(Pred, Args) :-
	% TODO: do some transformation to force Sh be an atom, if not we are also copying the shared terms!!
        % e.g. calls like call(foo(GiantTerm), X) could be unnecessarily slow (orders of magnitude)
        Pred = 'PA'(Sh,_H,_B),
%	display(user_error, copy_term__pa(Sh,_H,_B)), nl(user_error),
        copy_term(Pred, 'PA'(Sh,Args,Goal)), !,
        '$meta_call'(Goal).
calln(Pred, Args) :-
        Pred = 'PA'(_Sh,H,_B),
        functor(H,'',N),
        functor(Args,_,N), !, % Predicate abstraction OK, argument unif. failed
        fail.
calln(Pred, Args) :-
        functor(Args,_,N),
        throw(error(type_error(pred(N),Pred), call/n-1)).

% ---------------------------------------------------------------------------
:- export('$meta_call'/1).
:- '$props'('$meta_call'/1, [impnat=intrinsic]).

% ---------------------------------------------------------------------------

:- export('$term_to_meta'/2).
% note: a program may use term_to_meta without rt_exp
:- pred '$term_to_meta'/2 # "Transforms a normal term to a meta-term.".
'$term_to_meta'(X, '$:'(X)).
