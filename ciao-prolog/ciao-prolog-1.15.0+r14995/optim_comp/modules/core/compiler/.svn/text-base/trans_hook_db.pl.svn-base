% Definition of operations for translation hooks
%
% (this file is included in both rt_exp.pl, semantic_translation.pl,
% toplevel.pl, and compiler/frontend.pl)
%
% Open definitions that this code requires:
%   {
%     :- fluid m :: any # "module"
%     :- pred get_translation_hook(Kind, KVs).
%     :- pred add_translation_hook(Kind, KVs).
%     :- pred set_translation_hook(Kind, KVs).
%     :- pred del_translation_hook(Kind).
%   }.
%   :- pred mark_trans_hook(Kind).
%   :- pred unmark_trans_hook.
%
% TODO: Values of translation hooks are a single lists of key-value,
%       except 'goal', that contains multiple solutions of elements
%       key-value. Study if keeping this asymmetry is really worth.

% Valid priority for a translation
% TODO: allow symbolic priorities?
%check_priority(default_priority) :- !.
check_priority(Prior) :- number(Prior), !.

%:- public add_trans_hook/4.
:- meta_predicate add_trans_hook(+, +, primitive(spec), +).
add_trans_hook(M, Kind, T/A, Prior) :- Kind = goal, !, % TODO: hardwired, use 'type'
	check_priority(Prior),
	m :: any <- M,
        atom(T),
	% TODO: why different than in create_trans/4?
        functor(Tr, T, A),
        ( A = 3 -> arg(3, Tr, M) ; true),
	%
        add_prior_translation_hook(Kind, Tr, Prior),
	mark_trans_hook(Kind).
add_trans_hook(M, Kind, T/A, Prior) :-
	check_priority(Prior),
	m :: any <- M,
        atom(T),
        create_trans(A, T, M, Tr),
	insert_prior_translation_hook(Kind, Tr, Prior),
	mark_trans_hook(Kind).

%:- public del_trans_hook/1.
del_trans_hook(M) :-
	m :: any <- M,
	unmark_trans_hook,
        del_translation_hook(_). % delete all hooks

% ---------------------------------------------------------------------------

:- use_module(library(aggregates), [findall/3]).

{
:- fluid m :: any.

% Insert a translation hook with priority in the right place
% (obtained nondeterministically)
add_prior_translation_hook(Kind, Tr, Prior) :-
	findall(K-V, get_translation_hook(Kind, (K-V)), KVs0),
	pqueue_insert(KVs0, Prior, Tr, KVs),
	del_translation_hook(Kind),
	add_translation_hooks(KVs, Kind).
 
add_translation_hooks([], _).
add_translation_hooks([Prior-Tr|KVs], Kind) :-
	add_translation_hook(Kind, (Prior-Tr)),
	add_translation_hooks(KVs, Kind).

% Insert a translation hook with priority (as a list)
insert_prior_translation_hook(Kind, Tr, Prior) :-
        ( get_translation_hook(Kind, KVs0) -> true ; KVs0 = [] ),
	KVs = ~pqueue_insert(KVs0, Prior, Tr),
        set_translation_hook(Kind, KVs).
}.

% ---------------------------------------------------------------------------
% Priority queues (for storing transformations in order)
% TODO: share code

% Insert in order
pqueue_insert(A, K, V, B) :-
	% TODO: allow symbolic priorities?
%	( K0 = default_priority -> K = 500 ; K = K0 ),
%	display(user_error, pi0(A, K, V)), nl(user_error),
	pqueue_insert0(A, K, V, B).
%	display(user_error, pi1(B)), nl(user_error).
	
pqueue_insert0([], K, V, KVs) :- !, KVs = [K-V].
pqueue_insert0(KVs0, K, V, KVs) :-
	KVs0 = [KV0|KVs1], KV0 = K0-_,
	( K < K0 ->
	    KVs = [K-V|KVs0]
	; KVs = [KV0|RestKVs],
          pqueue_insert0(KVs1, K, V, RestKVs)
	).

% Obtain all the values of the priority queue
pqueue_values([], []).
pqueue_values([_-V|Xs], [V|Vs]) :-
	pqueue_values(Xs, Vs).


