
:- set_prolog_flag(read_assertions, yes).

:- prop hmtyped(X) # "@var{X} is well-typed w.r.t. Hindley-Milner.".
hmtyped(_). % (computational property)

:- prop hmtype(X) # "@var{X} defines a Hindley-Milner type.".
hmtype(_). % (computational property)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Predefined types:	{{{
%
%  The types list(T), pair(A,B), boolean and integer are predefined by the type
%  checker.

:- prop list/2 + hmtype.
%:- hm_type list(T) 	---> [] ; [T|list(T)].
% note: exactly like basic_props:list/2
list([],_).
list([X|Xs], T) :- T(X), list(Xs, T).

:- prop pair/3 + hmtype.
%:- hm_type pair(A,B) 	---> A - B.
pair(Ax-Bx,A,B) :- A(Ax), B(Bx).

:- prop boolean/1 + hmtype.
%:- hm_type boolean	---> true ; false.
boolean(true).
boolean(false).

:- prop cmp/1 + hmtype.
%:- hm_type cmp		---> (=) ; (>) ; (<).
cmp(=).
cmp(>).
cmp(<).
% }}}

% TODO: I generalized this type definition from code. Is it really a GADT? --JFMC
:- prop arithexp/2 + hmtype.
arithexp(random, T) :- T = float. % NOTE: only supported by Yap
arithexp((-Exp), T) :- arithexp(Exp, T).
arithexp((\Exp), T) :- arithexp(Exp, T), T = integer.
arithexp(abs(Exp), T) :- arithexp(Exp, T).
arithexp(log(Exp), T) :- arithexp(Exp, T), T = float.
% explicit conversion from integer to float
arithexp(integer(Exp), T) :- float(Exp), T = integer.
arithexp(sign(Exp), T) :- arithexp(Exp, T).
arithexp((Exp1+Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T).
arithexp((Exp1-Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T).
arithexp((Exp1*Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T).
arithexp((Exp1/Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T), T = float.
arithexp((Exp1//Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T), T = integer.
arithexp((Exp1**Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T), T = float.
arithexp((Exp1 mod Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T), T = integer.
arithexp(min(Exp1,Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T).
arithexp(max(Exp1,Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T).
arithexp((Exp1 >> Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T), T = integer.
arithexp((Exp1 << Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T), T = integer.
arithexp((Exp1 /\ Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T), T = integer.
arithexp((Exp1 \/ Exp2), T) :- arithexp(Exp1, T), arithexp(Exp2, T), T = integer.
arithexp(Exp, T) :- T(Exp). % TODO: strange case! like an alias type

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% Predefined signatures:	{{{
% (extracted from hardwired code by JFMC)
% (notes by JFMC)

% control predicates
:- trust_hm_pred ((any, any)).
:- trust_hm_pred ((any ; any)).
:- trust_hm_pred ((any -> any)).
:- trust_hm_pred (once(any)).
:- trust_hm_pred (findall(A,any,list(A))).

:- trust_hm_pred true.
:- trust_hm_pred fail.
:- trust_hm_pred (!).
:- trust_hm_pred abort.
%
:- trust_hm_pred display(any).
:- trust_hm_pred nl.
:- trust_hm_pred writeln(_A).
:- trust_hm_pred format(any,any).
%
:- trust_hm_pred var(_A).
:- trust_hm_pred nonvar(_A).
:- trust_hm_pred ground(_A).
%
:- trust_hm_pred atom_chars(atm,list(atm)).
:- trust_hm_pred atom_concat(atm,atm,atm).
:- trust_hm_pred atom_length(atm,integer).
:- trust_hm_pred concat_atom(list(atm),atm).
:- trust_hm_pred get_char(atm).
%
:- trust_hm_pred throw(_A).
:- trust_hm_pred catch(any,_A,any).

:- trust_hm_pred type_to_any(_A,any).
% TODO: needs dependent types?
% :- trust pred any_to_type(A,B,Type) :: any(A), call(Type,B) + hmtyped.

% This means that whenever two variables are unified, their type must
% be the same. 
:- trust_hm_pred (A = A).

:- trust_hm_pred (A \= A).
:- trust_hm_pred (A == A).
:- trust_hm_pred (A \== A).
:- trust_hm_pred (A @< A).
:- trust_hm_pred (A @> A).
:- trust_hm_pred (A @>= A).
:- trust_hm_pred (A @=< A).
:- trust_hm_pred copy_term(A,A).
:- trust_hm_pred compare(cmp,A,A).

% TODO: numeric/1 is a type class! generalize
% TODO: Those assertions are too conservative, but may be useful.
%       It would make sense to make them optional.
:- trust_hm_pred extra(numeric(A), (A > A)).
:- trust_hm_pred extra(numeric(A), (A < A)).
:- trust_hm_pred extra(numeric(A), (A =< A)).
:- trust_hm_pred extra(numeric(A), (A >= A)).
:- trust_hm_pred extra(numeric(A), (A =:= A)).
:- trust_hm_pred extra(numeric(A), (A =\= A)).

% These two are strange... they are tests! they should not report errors
:- trust_hm_pred integer(integer).
:- trust_hm_pred float(float).

% :- pred write(_).
% :- pred writeln(_).

% }}}


