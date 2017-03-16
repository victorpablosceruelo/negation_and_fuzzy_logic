:- module(_, [], [assertions, pure]).
%:- module(_, [], [compiler(complang)]).

:- doc(title, "Some useful operations and maps on lists").
:- doc(author, "Jose F. Morales").

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_compare)).
:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(data_facts)).

% (Use 'complang mini' to minimize dependencies at the runtime)
:- use_package(compiler(complang_mini)).
:- use_module(compiler(compiler_object__rt)).

% ---------------------------------------------------------------------------

:- public repeat_n/3.
% Repeat @var{I} times @var{X}
repeat_n(I, _) := [] :- I =< 0, !.
repeat_n(I, X) := [X|~repeat_n(I1, X)] :- I1 is I - 1.

% ---------------------------------------------------------------------------

:- doc(section, "Some maps between lists and monoidal data structures").
% Monoidal data structures have a binary constructor and an empty
% element.

% TODO: Share the definition of the following code (just the constructor changes)

% Flatten (_+...+_) as a list
:- public sum_to_list/2.
sum_to_list(A, Xs) :- items :: accum(Xs), sum_to_list__2(A).
{
:- fluid items :: accum.
sum_to_list__2(A) :- var(A), !, items.add(A).
sum_to_list__2(A+B) :- !, sum_to_list__2(A), sum_to_list__2(B).
sum_to_list__2(A) :- items.add(A).
}.

% Flatten (_*...*_) as a list
:- public prod_to_list/2.
prod_to_list(A, Xs) :- items :: accum(Xs), prod_to_list__2(A).
{
:- fluid items :: accum.
prod_to_list__2(A) :- var(A), !, items.add(A).
prod_to_list__2(A*B) :- !, prod_to_list__2(A), prod_to_list__2(B).
prod_to_list__2(A) :- items.add(A).
}.

% Flatten (_,...,_) as a list
:- public conj_to_list/2.
conj_to_list(A, Xs) :- items :: accum(Xs), conj_to_list__2(A).
{
:- fluid items :: accum.
conj_to_list__2(A) :- var(A), !, items.add(A).
conj_to_list__2((A, B)) :- !, conj_to_list__2(A), conj_to_list__2(B).
conj_to_list__2(A) :- items.add(A).
}.

% Like conj_to_list, but removes 'true' elements
:- public conj_to_slist/2.
conj_to_slist(A, Xs) :- items :: accum(Xs), conj_to_slist__2(A).
{
:- fluid items :: accum.
conj_to_slist__2(A) :- var(A), !, items.add(A).
conj_to_slist__2((A, B)) :- !, conj_to_slist__2(A), conj_to_slist__2(B).
conj_to_slist__2(true) :- !.
conj_to_slist__2(A) :- items.add(A).
}.

% Flatten (_;...;_) as a list
:- public disj_to_list/2.
disj_to_list(A, Xs) :- items :: accum(Xs), disj_to_list__2(A).
{
:- fluid items :: accum.
disj_to_list__2(A) :- var(A), !, items.add(A).
disj_to_list__2((A ; B)) :- !, disj_to_list__2(A), disj_to_list__2(B).
disj_to_list__2(A) :- items.add(A).
}.

% Flatten (_|...|_) as a list
:- public fundisj_to_list/2.
fundisj_to_list(A, Xs) :- items :: accum(Xs), fundisj_to_list__2(A).
{
:- fluid items :: accum.
fundisj_to_list__2(A) :- var(A), !, items.add(A).
fundisj_to_list__2(^'|'(A,B)) :- !, fundisj_to_list__2(A), fundisj_to_list__2(B).
fundisj_to_list__2(A) :- items.add(A).
}.

% Build (_,_,...,_) from [_,_,...,_].
% [] is 'true'
:- public list_to_conj/2.
list_to_conj([], A) :- !, A = true.
list_to_conj([A], A) :- !.
list_to_conj([A|As], (A,Bs)) :-
	list_to_conj(As, Bs).

% Build (_;_;...;_) from [_,_,...,_]
% [] is 'fail'
:- public list_to_disj/2.
list_to_disj([], A) :- !, A = fail.
list_to_disj([A], A) :- !.
list_to_disj([A|As], (A ; Bs)) :-
	list_to_disj(As, Bs).

% % Build (_|_|...|_) from [_,_,...,_]
% % [] is not accepted
% %
% % TODO: In functional notation, the base case for translation of []
% %       could be something like ~empty, where "empty(_) :- fail".
%
% :- public list_to_fundisj/2.
% list_to_fundisj([A], A) :- !.
% list_to_fundisj([A|As], ^'|'(A,Bs)) :- !,
% 	list_to_fundisj(As, Bs).

% Build (_+_+...+_) from [_,_,...,_]
% [] is not accepted
:- public list_to_sum/2.
list_to_sum([A], A) :- !.
list_to_sum([A|As], A+Bs) :- list_to_sum(As, Bs).

% ===========================================================================
% (Module-expanded versions of the previous predicates)
% TODO: define parametrically, probably using '$unfold' for efficiency
% TODO: add an extension to write 'expanded module names in an abstract way'
%       (e.g., ~q('basiccontrol','true')

% from (module-expanded) conjunction (any tree with ',' nodes) to list
:- public mconj_to_slist/2.
mconj_to_slist(A, Xs) :- items :: accum(Xs), mconj_to_slist__2(A).
{
:- fluid items :: accum.
mconj_to_slist__2('basiccontrol:,'(A, B)) :- !,
	mconj_to_slist__2(A), mconj_to_slist__2(B).
mconj_to_slist__2('basiccontrol:true') :- !.
mconj_to_slist__2(A) :- items.add(A).
}.

% from list to (module-expanded) conjunction (a right-node recursive tree)
:- public list_to_mconj/2.
list_to_mconj([], 'basiccontrol:true') :- !.
list_to_mconj([A], A) :- !.
list_to_mconj([A|As], 'basiccontrol:,'(A,Bs)) :-
	list_to_mconj(As, Bs).

% ===========================================================================

:- use_module(compiler(meta_syntax), [mcall/3]).

% TODO: bug: cannot use 'list' as name of ctxvar (conflicts with list/1 definition)
% Note: 'dotpath' is only left-associative [e.g., (((_._)._)._)]
:- public dotpath_to_list/2.
dotpath_to_list(A) := L :- lst :: accum(L), dotpath_to_list__2(A).
{
:- fluid lst :: accum.
dotpath_to_list__2(A) :- var(A), !, lst.add(A).
dotpath_to_list__2(X) :- X = ~mcall(A, B), !, dotpath_to_list__2(A), lst.add(B).
dotpath_to_list__2(A) :- lst.add(A).
}.

:- public list_to_dotpath/2.
list_to_dotpath([A|As]) := L :-
	p :: m_any <- A,
	list_to_dotpath__2(As),
	L = ~p.
{
:- fluid p :: m_any.
list_to_dotpath__2([]).
list_to_dotpath__2([A|As]) :- P = ~p, p <- ~mcall(P, A), list_to_dotpath__2(As).
}.

