:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"FOL-based Checker").

:- doc(author, "Jose F. Morales").

:- doc(bug, "Alpha state - just an experiment").
:- doc(bug, "Define the core assertion language").
:- doc(bug, "Define the Ciao assertion language in terms of the core
   assertion language?").
:- doc(bug, "Define the different program languages/semantics").
:- doc(bug, "Can I help the prover?").
:- doc(bug, "Read the output of the external prover").
:- doc(bug, "Use SMT solvers?").

:- doc(module,"
   This package allows the verification of assertions using external
   tools for theorem proving.

   @begin{alert}
   This package is a proof-of-concept, and it is in a very ALPHA
   state. This documentation is just a stub.

   It is currently limited to a very simple (@math{Pre @Rightarrow
   Post}) assertion on a subset of Prolog using constants as data).
   It could be arbitrarily extended to support more complex semantics
   (selected by the user) and provers.
   @end{alert}

   This package translates predicates and assertions into axioms (the
   theory that defines the program), and conjectures to be proved. The
   logic of the formulae is FOL. The external prover is Eprover (using
   the TPTP format and FOF clauses). An example program is provided in
   @tt{tests/t2.pl}.

   @section{Program Language}

   We can classify the language of formulas as follow:

   @begin{itemize}
   @item Unrestricted FOL formulas?

   @item Horn clauses: a disjunction of literals with at most one
     positive literal (called @emph{head}) and any number of negative
     literals (forming the @emph{body}).  Usually written as 
     @math{p @leftarrow q_1 @wedge @ldots @wedge q_n}, or as Prolog syntax:

     @begin{verbatim}
     p :- q1, ..., qN.
     @end{verbatim}
   @end{itemize}

   W.r.t. the literals, they can be:
   @begin{itemize}
   @item Propositional: literals cannot have arguments.
     @begin{verbatim}
     p :- q, r.
     @end{verbatim}
   @item Predicate: literals can have arguments (quantifiers?)
     @begin{verbatim}
     p(s(X)) :- p(X).
     @end{verbatim}
   @end{itemize}

   W.r.t. the domain, it can be (combination of?):
   @begin{itemize}
   @item Herbrand.
   @item Some constraint system.
   @item Equational?
   @end{itemize}

   The resolution strategy can be one of:
   @begin{itemize}
   @item SLD (Selective Linear Definite): sound and (refutation?) complete for Horn clauses.
   @item SLDNF?
   @item Prolog = SLD + depth-first (not complete if the search
     contains infinite branches and the search strategy seaches these
     in preference to finite branches).
   @end{itemize}

   @subsection{Closed World Assumption (Clark's completion)}
   
   @begin{itemize}
   @item @math{S} follows logically from @math{Comp(S)}
   @item The completion of a set of definite clauses
     is always consistent (it always has a model).
   @item for atomic statements @math{A}, @math{S @vDash A} iff
     @math{Comp(S) @vDash A} (it adds no positive information)
   @end{itemize}

   @section{Assertion Language}

   The language of assertions can be:
   @begin{itemize}
   @item CNF (conjunctive normal form)
   @item Model: satisfability.
   @item Operational (derivation trees and substitutils)
   @item Low-level Operational (choice points, term derefs, etc.)
   @item Some syntactic sugar.
   @end{itemize}

   Translation to FOL clauses is different for each case.

   @subsection{Assertions for Validity of Propositional Subset}

   Resolution computes a minimal model (it is sound and complete for
   this subset).

   The properties that can be verified are:

   @begin{verbatim}
   :- pred a + succeed.
   :- pred a + not_succeed.
   :- pred a + fail.
   :- pred a + not_terminate.
   :- prove Formula.
   @end{verbatim}

   @subsection{Assertions for Datalog-like Subset}

   Horn clauses predicate (only atoms) (datalog minus negation?)

   @begin{verbatim}
   p(a) :- q(b).
   @end{verbatim}

   @subsection{Assertions for Predicate Subset}

   Horn clauses predicate (terms)

   @begin{verbatim}
   p(t(X,Y)) :- q(r(X)).
   @end{verbatim}

   An assertion like:

   @begin{verbatim}
   :- pred foo(X) :: list(int, X). % a1
   @end{verbatim}

   is equivalent to:
   @begin{verbatim}
   fof(a1, conjecture, ![X] : (foo(X) => list(int, X))).
   @end{verbatim}
").

