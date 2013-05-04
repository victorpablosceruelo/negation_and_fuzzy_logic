% A test for context variables
%
% Author: Jose F. Morales

:- module(_, [main/0], [compiler(complang)]).

% creates a node
node(Name, N) :-
	N = ~'$mut__init'(node(Name, no, [])).

% adds a directed arc
arc(From, To) :-
	node(Name, Seen, ToList) = ~'$mut__value'(From),
	% TODO: this should be From = ~graph.alloc(...)
	'$mut__assign'(From, node(Name, Seen, [To|ToList])).

% A queue
% TODO: this should be a class with deforested instances
% TODO: implement deforest_instance (avoids structures)
:- class accumlist {
    :- attr head :: single.
    :- attr tail :: pair.

    :- constructor new/0.
    new :- 
        '$self' <- accumlist(_, _),
	~head = ~tail.

    myadd(X) :- tail.add(X). % also ~tail = [X|T], tail <- T.

    :- constant close/0.
    close :- ~tail = [].
}.

% TODO: this should be a code block (like an object, but the
%       constructor does everything)
spanlist(From) := Names :-
	push_pair(st, class(accumlist), _) '$ctx_on' (
	  st.new,
	  spanlist__from(From),
	  st.close,
	  Names = st.head
        ).

:- '$begin_context'(spanlist).
:- '$usectx'(pair(st, class(accumlist))).
% obtains in pair(names) the names of all nodes reachable from From
spanlist__from(From) :-
	node(Name, Seen, ToList) = ~'$mut__value'(From),
	( Seen = no ->
	    '$mut__assign'(From, node(Name, yes, ToList)),
	    st.myadd(Name),
	    spanlist__list(ToList)
	; true
	).

spanlist__list([]).
spanlist__list([X|Xs]) :- spanlist__from(X), spanlist__list(Xs).
:- '$end'.

main :-
	node(a, A), node(b, B), node(c, C), node(d, D), node(e, E),
	arc(A, B), arc(B, A), arc(A, C), arc(C, B), arc(D, A), arc(D, B), arc(C, E),
	Names = ~spanlist(B),
	display(span_list_from_b_is(Names)), nl.


% NOTE: Obsolete documentation, ctx_store has been replaced by mutables
%
% This example creates a graph and follows its nodes using the
% '$ctx_store__*' family of predicates and hipair(...) context
% variables. The semantic of those predicates is pure, and equivalent
% to replacing, looking up or inserting (Key-Value) pairs in a list.
% 
% The hipair(...) context forbids directly reading or writing the
% Key-Value table, and thus access predicates can be implemented using
% attributed variables.
%
% So, although the program does not contain impure features, the node
% traversal is very efficient (and the program is supposed to scale
% very well).
%
% Bug: currently no '$ctx_store__*' predicates (except
% '$ctx_store__init'/2) can be called with an unbound Id, and it is
% not possible to recover the key list. On the other hand, lost keys
% are automatically recovered on garbage-collection.

