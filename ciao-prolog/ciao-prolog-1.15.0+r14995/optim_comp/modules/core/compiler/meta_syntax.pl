:- module(meta_syntax, [], [assertions, pure]).

:- doc(title, "Syntactic manipulation of (qualified) program terms").

:- doc(module, "This module defines a series of predicates to
   manipulate special program syntax. The abstraction of those
   definitions in this module allows a better integration of language
   extensions (e.g., funcional syntax will work whatever is the syntax
   to module qualify or unqualify a term) and simplies future changes
   in syntax. Those predicates are used in the language front-end and
   several language extensions.").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_compare)).
:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(data_facts)).
:- use_module(engine(exceptions)).

% (Use 'complang mini' to minimize dependencies at the runtime)
:- use_package(compiler(complang_mini)).

% Escaped reserved functors to compile special syntaxes.

% Note: It also avoids a in functional notation, escaping
%   X = ~f with X = ^(~f)
% only works in heads, while we want to use them in predicate bodies.
%
%       (this bug is solved in optim_comp, but those predicates
%        are still handy)

:- public funcall/2.
funcall(X, ^(~(X))).

:- public mcall/3.
mcall(X, Y, ^(X.Y)).

:- public square_index/3.
% Square brackets notation for indices "Obj[Field]"
square_index(X, Obj, Field) :-
	nonvar(X), X = '\6\postfix_block'(Obj, Field0),
	nonvar(Field0), Field0 = [Field].

:- public anon_curly_block/2.
% C is an anonymous curly block ("{ c1. c2. ... }")
% Note: "{}" is the empty block
anon_curly_block(C, Sentences) :-
	curly_to_list(C, Sentences).

:- public nested_decl/4.
nested_decl(Decl, Name, Keyword, Sentences) :- % keyword(name) { ... }
	nonvar(Decl),
	Decl = '\6\postfix_block'(Decl0, Bl),
	functor(Decl0, Keyword, 1),
	arg(1, Decl0, Name),
	!,
	curly_to_list(Bl, Sentences).
nested_decl(Decl, Name, Keyword, Sentences) :-
	functor(Decl, Keyword, 1),
	arg(1, Decl, Arg),
	% TODO: make '<' optional?
	( Arg = (Name < '\6\postfix_block'(Parent, Bl)) -> % keyword name < parent { ... }
	    Extends = sentence((:- extends(Parent)), [], [], 0, 0),
	    Sentences = [Extends|Sentences0]
	; Arg = '\6\postfix_block'(Name, Bl) -> % keyword name { ... }
	    Sentences = Sentences0
	; fail
	),
	curly_to_list(Bl, Sentences0).

:- public named_curly_block/3.
% C is a named curly block ("name { c1. c2. ... }")
% Note: "name {}" a named empty block
named_curly_block(C, Name, Sentences) :-
        C = '\6\postfix_block'(Name, Def),
	nonvar(Def),
	curly_to_list(Def, Sentences).

curly_to_list(({}), []).
curly_to_list('\6\curly_block'(Sentences), Sentences).

% ---------------------------------------------------------------------------
% Term and goal qualifications

% :- doc(qual/1, "An optional term qualifier.").
% :- regtype qual := 
%      qual_none   # "No qualifier"
%    | qual_m(M)   # "The qualifier is the module/class @var{M}"
%    | qual_o(Obj) # "Qualified with an object @var{Obj} of unknown type".

:- public get_qualifier/3.
% :- pred get_qualifier/3 :: term * qual * term 
%    # "Split a term into a qualifier and an unqualified term".
get_qualifier(QX, Qual, X) :-
	( var(QX) -> Qual = qual_none, X = QX % a variable, no qualifier
	; QX = ~mcall(Obj, X0) -> Qual = qual_o(Obj), X = X0 % Obj.X0 (object)
	; QX = ':'(M,X0) -> Qual = qual_m(M), X = X0 % M:X0 (class/module)
	; Qual = qual_none, X = QX % no qualifier
	).

:- public apply_qual/3.
% :- fun apply_qual/3 :: qual * term -> term 
%    # "Add a module qualification to a goal or term".
apply_qual(Qual, X) := X :- var(X), !,
	( Qual = qual_none -> true
	; throw(cannot_qualify_var) % TODO: avoid direct use of throw/1; add a helper 'error' predicate (a runtime check)
	).
apply_qual(Qual, X) :=
	( Qual = qual_none ? X
	| Qual = qual_o(Obj) ? ~mcall(Obj, X)
	| Qual = qual_m(M) ? ':'(M,X)
	).

% ---------------------------------------------------------------------------
% Functor/predicate symbols

% :- regtype sym := 
%      sympred(F,A) # "The predicate or function symbol F/A"
%    | symmod(M)    # "A module symbol M".

:- public decomp_goal/3.
% :- pred decomp_goal(Goal, Sym, Args) :: term * sym * list(term) 
% # "@var{Goal} is a term with predicate/function symbol @var{Sym} and arguments @var{Args}".
decomp_goal(Goal, Sym, Args) :- atom(Goal), !,
	Sym = sympred(Goal, 0),
	Args = [].
decomp_goal(Goal, Sym, Args) :-
	functor(Goal, N, A0),
	Goal =.. [_|Args],
	Sym = sympred(N, A0).

:- public sym_to_spec/2.
% :- fun sym_to_spec/2 :: sym -> predspec 
%    # "From symbol to (predicate) specifier".
sym_to_spec(sympred(N,A)) := N/A.

