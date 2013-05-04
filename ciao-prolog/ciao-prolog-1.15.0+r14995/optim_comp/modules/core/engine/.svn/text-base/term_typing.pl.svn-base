:- module(term_typing, [], [pure, assertions, isomodes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).
:- use_module(engine(arithmetic)).

:- doc(title,"Extra-logical properties for typing").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"This library contains traditional Prolog predicates
        for testing types.  They depend on the state of instantiation of
        their arguments, thus being of extra-logical nature.").

:- '$native_include_c_source'(.(term_typing)).

:- export(ground/1).
:- true prop ground(X) + native
	# "@var{X} is currently ground (it contains no variables).".
:- true success ground(X) => gnd(X).
:- true comp ground(@X) + ( sideff(free), native ).
% TODO: add support for ground in cyclic terms
:- '$props'(ground/1, [impnat=cbool(cground)]).

:- export(atom/1).
:- true prop atom(X) + native
	# "@var{X} is currently instantiated to an atom.".
:- true success atom(X) => atm(X).
:- true comp atom(@X) + ( sideff(free), native ).
:- '$props'(atom/1, [impnat=cblt(bu1_atom,m(0,0))]).

:- export(atomic/1).
:- true prop atomic(X) + native
	# "@var{X} is currently instantiated to an atom or a number.".
:- true comp atomic(@X) + ( sideff(free), native ).
:- '$props'(atomic/1, [impnat=cblt(bu1_atomic,m(0,0))]).

:- export(float/1).
:- true prop float(X) + native
	# "@var{X} is currently instantiated to a float.".
:- true success float(X) => flt(X).
:- true comp float(@X) + ( sideff(free), native ).
:- '$props'(float/1, [impnat=cblt(bu1_float,m(0,0))]).

:- export(integer/1).
:- true prop integer(X) + native
	# "@var{X} is currently instantiated to an integer.".
:- true success integer(X) => int(X).
:- true comp integer(@X) + ( sideff(free), native ).
:- '$props'(integer/1, [impnat=cblt(bu1_integer,m(0,0))]).

:- export(nonvar/1).
:- true prop nonvar(X) + native(not_free(X))
   # "@var{X} is currently a term which is not a free variable.".
:- true comp nonvar(@X) + ( sideff(free), native ).
:- '$props'(nonvar/1, [impnat=cblt(bu1_nonvar,m(0,0))]).

:- export(number/1).
:- true prop number(X) + native
	# "@var{X} is currently instantiated to a number.".
:- true success number(X) => num(X).
:- true comp number(@X) + ( sideff(free), native ).
:- '$props'(number/1, [impnat=cblt(bu1_number,m(0,0))]).

:- export(var/1).
:- true prop var(X) + native(free(X))
   # "@var{X} is a free variable.".
:- true comp var(@X) + ( native, sideff(free) ).
:- '$props'(var/1, [impnat=cblt(bu1_var,m(0,0))]).

:- export(type/2).
:- true prop type(X,Y) + native
   # "@var{X} is internally of type @var{Y} (@tt{var}, @tt{attv}, @tt{float},
      @tt{integer}, @tt{structure}, @tt{atom} or @tt{list}).".
:- true success type(X,Y) => atm(Y).
:- true comp type/2 + ( sideff(free), native ).
:- '$props'(type/2, [impnat=cfun(fu1_type,no)]).
