:- module(term_basic, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title,"Basic term manipulation").
:- doc(author, "Jose F. Morales").

:- doc(bug, "[Incomplete version of engine(term_basic) for pl2js]").

:- use_module(library(arithpreds)). % TODO: include it? (at least for improlog-like code)

% ---------------------------------------------------------------------------

:- export((=)/2).
A = B :- A = B. % (predicate for built-in (=)/2)

:- export('C'/3). % builtin for DCGs
'C'(X, Y, Z) :- 'C'(X, Y, Z).

% ---------------------------------------------------------------------------

% TODO: implement functor/3, arg/3, atomic/1, etc. (see examples/testsuite/boyer.pl)

% ---------------------------------------------------------------------------
:- doc(section, "Variables in a term").

:- doc(bug, "This is similar to library(terms_vars). However, it is
   better to include it here. This would also be the place to write
   copy_term/?").

:- use_package(mutables). % var_nb_mut_dict/0

:- export(term_vars/2).
% TODO: it does not go inside non-arguments (is that right?)
term_vars(X) := Vars :-
	Seen = ~mutables_rt.var_nb_mut_dict,
	term_vars_(X, Seen, Vars, []).

term_vars_(X, Seen, Vars, Vars0) :- var(X), !,
	( Seen.in(X) -> Vars = Vars0
	; % note: it does not really matter what is put in the Seen dictionary
          Seen.set(X, 1), Vars = [X|Vars0]
	).
term_vars_(X, Seen, Vars, Vars0) :- nonvar(X), !,
	( ~X.'$arity' = 0 ->
	    Vars = Vars0
        ; term_vars__args(1, X, Seen, Vars, Vars0)
        ).
term_vars_(_X, _Seen, Vars, Vars0) :- Vars = Vars0.

term_vars__args(I, X, Seen, Vars, Vars0) :-
        ( I > ~X.'$arity' -> Vars = Vars0
        ; term_vars_(~X.'$arg'(I), Seen, Vars, Vars1),
	  term_vars__args(I + 1, X, Seen, Vars1, Vars0)
        ).

% ---------------------------------------------------------------------------

:- export(term_match/2).
% Two terms are unifiable (but do not unify them)
term_match(A, B) :- \+ \+ A = B.

