:- module(dynamic, [
        asserta/1, asserta/2, assertz/1, assertz/2, assert/1, assert/2,
        retract/1, retractall/1, abolish/1,
        clause/2, clause/3, wellformed_body/3
        ],[pure,assertions,isomodes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_compare)).
:- use_module(engine(exceptions)).
:- use_module(engine(data_facts)).
:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(system_info)).
:- use_module(engine(internals)).

:- doc(title,"Dynamic predicates").

:- doc(author, "The CLIP Group").

:- doc(module,"This module implements the assert/retract family of
   predicates to manipulate dynamic predicates.

   The predicates defined in this module allow modification of the
   program as it is actually running.  Clauses can be added to the
   program (@em{asserted}) or removed from the program (@em{retracted}).
   For these predicates, the argument which corresponds to the clause
   head must be instantiated to an atom or a compound term. The argument
   corresponding to the clause must be instantiated either to a term
   @tt{Head :- Body} or, if the body part is empty, to @tt{Head}. An
   empty body part is represented as @tt{true}.  Note that using this
   library is very detrimental to global analysis, and that for most
   uses the predicates listed in @ref{Fast/concurrent update of facts}
   suffice.").

% TODO: does not work! documenting a predicate defined in other module...
/*
:- doc(doinclude, dynamic/1).
:- true decl dynamic(Predicates) : sequence_or_list(predname) + iso
        # "Defines each predicate in @var{Predicates} as a
          @concept{dynamic predicate}.  If a predicate is defined
          dynamic in a file, it must be defined dynamic in every file
          containing clauses for that predicate. The directive should
          precede all clauses of the affected predicates.  This
          directive is defined as a prefix operator in the compiler.".
:- doc(dynamic(F/A), "The predicate named @var{F} with arity
@var{A} is made @concept{dynamic} in the current module at runtime
(useful for predicate names generated on-the-fly).  If the predicate
functor name @var{F} is uninstatiated, a new, unique, predicate name
is generated at runtime.").
:- true pred dynamic(+Spec).
*/

:- true pred asserta(+Clause) + (iso, native)
# "The current instance of @var{Clause} is interpreted as a clause and is
   added to the current program.  The predicate concerned must be dynamic.
   The new clause becomes the @em{first} clause for the predicate concerned.
   Any uninstantiated variables in @var{Clause} will be replaced by new
   private variables.".
%% along with copies of any subgoals blocked on these variables.

:- meta_predicate asserta(primitive(clause)).
asserta(Clause) :-
        dynamic_clauses(Clause, Head, Body, asserta/1),
	'$asserta'(Head, Body).

:- true pred asserta(+Clause,-Ref) + native
# "Like @tt{asserta/1}. @var{Ref} is a unique identifier of the asserted
   clause.".

:- meta_predicate asserta(primitive(clause), ?).
asserta(Clause, Ref) :-
        dynamic_clauses(Clause, Head, Body, asserta/2),
	'$asserta_ref'(Head, Body, Ref).

:- true pred assertz(+Clause) + (iso, native)
# "Like @tt{asserta/1}, except that the new clause becomes the @em{last}
   clause for the predicate concerned.".

:- meta_predicate assertz(primitive(clause)).
assertz(Clause) :-
        dynamic_clauses(Clause, Head, Body, assertz/1),
	'$assertz'(Head, Body).

:- true pred assertz(+Clause,-Ref) + native
# "Like @tt{assertz/1}. @var{Ref} is a unique identifier of the asserted
   clause.".

:- meta_predicate assertz(primitive(clause), ?).
assertz(Clause, Ref) :-
        dynamic_clauses(Clause, Head, Body, assertz/2),
	'$assertz_ref'(Head, Body, Ref).

:- true pred assert(+Clause) + native
# "Identical to @tt{assertz/1}. Included for compatibility.".

:- meta_predicate assert(primitive(clause)).
assert(Clause) :-
        dynamic_clauses(Clause, Head, Body, assert/1),
	'$assertz'(Head, Body).

:- true pred assert(+Clause,-Ref) + native
# "Identical to @tt{assertz/2}. Included for compatibility.".

:- meta_predicate assert(primitive(clause), ?).
assert(Clause, Ref) :-
        dynamic_clauses(Clause, Head, Body, assert/2),
	'$assertz_ref'(Head, Body, Ref).

dynamic_clauses(CL, Head, Body, Goal) :-
	( canonical_clause(CL, Head, Body0),
	  wellformed_body(Body0, +, Body) ->
	    '$check_dynamic'(Head, Goal)
	; throw(error(type_error(clause, CL), Goal-1))
	).

canonical_clause((H :- B), H, B) :- !,
	functor(H, F, _),
	atom(F).
canonical_clause(H, H, 'basiccontrol:true') :-
	functor(H, F, _),
	atom(F).

:- doc(wellformed_body(BodyIn,Env,BodyOut),
   "@var{BodyIn} is a well-formed clause body. @var{BodyOut} is its
    counterpart with no single-variable meta-goals (i.e., with @tt{call(X)}
    for @tt{X}). @tt{Env} denotes if global cuts are admissible in
    @tt{BodyIn} (@tt{+} if they are, @tt{-} if they are not)."). 

wellformed_body(B, _, call(B)) :- var(B), !.
wellformed_body(!, Env, !) :- !, Env = + .
wellformed_body((A,B), Env, (A1,B1)) :- !,
        wellformed_body(A, Env, A1),
	wellformed_body(B, Env, B1).
wellformed_body((A->B), Env, (A1->B1)) :- !,
        wellformed_body(A, -, A1),
	wellformed_body(B, Env, B1).
wellformed_body((A;B), Env, (A1;B1)) :- !,
        wellformed_body(A, Env, A1),
	wellformed_body(B, Env, B1).
wellformed_body((\+ A), _, (\+ A1)) :- !,
        wellformed_body(A, -, A1).
wellformed_body(if(A,B,C), Env, if(A1,B1,C1)) :- !,
        wellformed_body(A, -, A1),
        wellformed_body(B, Env, B1),
        wellformed_body(C, Env, C1).
wellformed_body(L^A, Env, L^A1) :- !,
        wellformed_body(A, Env, A1).
wellformed_body(Goal, _, Goal) :-
	functor(Goal, F, _),
	atom(F).

% TODO: does not work! documenting a predicate defined in other module...
/*
:- doc(doinclude, data/1).
:- true pred data(+Spec).
:- doc(data(F/A), "The predicate named @var{F} with arity @var{A}
is made @concept{data} in the current module at runtime (useful for
predicate names generated on-the-fly).  If the predicate functor name
@var{F} is uninstatiated, a new, unique, predicate name is generated
at runtime. ").
*/

:- true pred retract(+Clause) + (iso, native)
# "The first clause in the program that matches @var{Clause} is erased.
   The predicate concerned must be dynamic. 

   The predicate @tt{retract/1}
   may be used in a non-determinate fashion, i.e., it will successively
   retract clauses matching the argument through backtracking. If reactivated
   by backtracking, invocations of the predicate whose clauses are being
   retracted will proceed unaffected by the retracts. This is also true
   for invocations of @tt{clause} for the same predicate. The space occupied
   by a retracted clause will be recovered when instances of the clause are
   no longer in use.".

:- meta_predicate retract(primitive(clause)).
retract(CL) :-
	canonical_clause(CL, Head, Body), 
	'$check_dynamic'(Head, retract/1),
	'$erase_nb'(Head, Body).

:- true pred retractall(+Head) + native
# "Erase all clauses whose head matches @var{Head}, where @var{Head} must
   be instantiated to an atom or a compound term.  The predicate concerned
   must be dynamic.  The predicate definition is retained.".

:- meta_predicate retractall(primitive(fact)).
retractall(H) :-
	nonvar(H), % TODO: is that possible with meta_predicate decl?
        retractall_(H).

retractall_(Head) :-
	'$check_dynamic'(Head, retractall/1),
	'$erase_nb'(Head, _Body),
	fail.
retractall_(_).

:- true pred abolish(+Spec) + (iso, native)
# "Erase all clauses of the predicate specified by the predicate spec
   @var{Spec}. The predicate definition itself is also erased (the
   predicate is deemed undefined after execution of the abolish). The
   predicates concerned must all be user defined.".

:- meta_predicate abolish(primitive(spec)).
abolish(F/A) :-
        functor(Head, F, A), !,
	abolish_data_of(Head),
	'$abolish'(Head).

:- multifile do_on_abolish/1.

:- doc(do_on_abolish(Head),"A hook predicate which will be called
	when the definition of the predicate of @var{Head} is abolished.").

abolish_data_of(Head) :-
        do_on_abolish(Head),
        fail.
abolish_data_of(_).

:- true pred clause(+Head,?Body) + (iso, native)
# "The clause '@var{Head} @tt{:-} @var{Body}' exists in the current
   program. The predicate concerned must be dynamic.".

:- meta_predicate clause(primitive(fact),?).
clause(Head, Body) :-
	nonvar(Head),
	'$check_dynamic'(Head, clause/2),
	'$current_nb'(Head, Body).

:- doc(clause(Head,Body,Ref),"Like @tt{clause(Head,Body)}, plus the
   clause is uniquely identified by @var{Ref}.").

:- true comp clause/3 + native.
:- true pred clause(+Head,?Body,?Ref)
# "@var{Head} must be instantiated to an atom or a compound term.".
:- true pred clause(?Head,?Body,+Ref)
# "@var{Ref} must be instantiated to a valid identifier.".

:- meta_predicate clause(primitive(fact),?,?).
clause(Head, Body, Ref) :-
	% TODO: fix! head cannot be a variable because of the meta expansion... should meta expansions be moded?
	'$check_dynamic'(Head, clause/3),
	'$current_nb_ref'(Head, Body, Ref).

% TODO: does not work! documenting a predicate defined in other module...
/*
:- true pred current_predicate(?Spec) + (iso, native)
        # "A predicate in the current module is named @var{Spec}.".

:- true pred current_predicate(?Spec,?Module) + native
        # "A predicate in @var{Module} is named @var{Spec}. @var{Module}
           never is an engine module.".
*/
:- use_module(engine(rt_exp), ['$check_dynamic'/2]).
:- reexport(engine(rt_exp), [current_predicate/1, current_predicate/2,
	data/1, dynamic/1]).
