:- module(compiler_object__rt, [], [pure]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(arithmetic)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(data_facts)).
:- use_module(library(prolog_sys), [new_atom/1]).
:- use_module(engine(rt_exp), ['$define_predicate'/2]).
:- use_module(engine(internals), 
	['$current_clauses'/2,
	 '$erase_nb_root'/3]).

:- use_package(compiler(complang_mini)).

/* OLD VERSION THAT DOES NOT USE EMPTY_GC_BIN
% allocated predicates
:- data '$pred_id'/3.
% free predicates
:- data '$free_id'/3.

% obtain a fresh predicate Root
:- export('$pred__new'/2).
'$pred__new'(Arity, Root) :-
	( retract_fact('$free_id'(Arity, Id, Root)) -> 
            % reuse a unused predicate
	    true
	; % allocate a new one
	  new_atom(Id),
	  functor(Functor, Id, Arity),
	  '$define_predicate'(Id/Arity, 2'01), % TODO: see dynamic.pl
	  '$current_clauses'(Functor, Root)
	),
	% store as used
        asserta_fact('$pred_id'(Root, Id, Arity)).

% free a predicate Root
:- export('$pred__free'/1).
'$pred__free'(Root) :-
        % remove from the allocated predicate list
        retract_fact('$pred_id'(Root, Id, Arity)), !,
	% deallocate a predicate
	( functor(Fact3, '$junk', Arity),
	  '$erase_nb_root'(Fact3, 'basiccontrol:true', Root),
	  fail
	; true
	),
	% add to the unallocated predicate list
        asserta_fact('$free_id'(Arity, Id, Root)).
*/

% allocated predicates
:- data '$pred_id'/3.
% free predicates
:- data '$free_id'/2.

:- use_module(engine(internals), ['$abolish'/1]).
:- use_module(engine(internals), ['$empty_gcdef_bin'/0]).

% TODO: implement in C?
% TODO: is correct the use of empty_gcdef_bin here? it is also used in toplevel

% obtain a fresh predicate Root
:- export('$pred__new'/2).
'$pred__new'(Arity, Root) :-
	( retract_fact('$free_id'(Arity, Id)) -> 
            % reuse a unused id
	    true
	; % get a new one
	  new_atom(Id)
	),
	functor(Functor, Id, Arity),
	'$define_predicate'(Id/Arity, 2'01), % TODO: see dynamic.pl
	'$current_clauses'(Functor, Root),
	% store as used
        asserta_fact('$pred_id'(Root, Id, Arity)).

% free a predicate Root
:- export('$pred__free'/1).
'$pred__free'(Root) :-
        % remove from the allocated predicate list
        retract_fact('$pred_id'(Root, Id, Arity)), !,
	% deallocate a predicate
	functor(Functor, Id, Arity),
	'$abolish'(Functor),
	'$empty_gcdef_bin',
	% add to the unallocated predicate list
        asserta_fact('$free_id'(Arity, Id)).

% ---------------------------------------------------------------------------

:- use_module(engine(attributes)).

:- export('$mut__init'/2).
'$mut__init'(Val, Id) :-
	attributes:attach_attribute_weak(Id, Val).

:- export('$mut__value'/2).
'$mut__value'(Id, Val) :-
	attributes:get_attribute(Id, Val).

:- export('$mut__assign'/2).
'$mut__assign'(Id, Val) :-
	attributes:detach_attribute(Id),
	attributes:attach_attribute_weak(Id, Val).

% ---------------------------------------------------------------------------

:- use_module(engine(rt_exp), ['$oo_attrtype_v'/3, '$oo_attrtype_i'/4]).

:- multifile('$meta_itypes'/1).

'$meta_itype'(This, I, l(IType)) :-
	'$oo_attrtype_v'(This, I, IType), !.
'$meta_itype'(This, I, i(N/A)) :-
	'$oo_attrtype_i'(This, I, N, A), !.
%'$meta_itype'(This, I, IType) :-
%	functor(This, N, A),
%	functor(ITypes, N, A),
%	'$meta_itypes'(ITypes), !,
%	arg(I, ITypes, IType).

% Create a new instance
:- export('$inst_new'/2).
'$inst_new'(N/A, This) :-
	functor(This, N, A),
	inst_new__2(1, A, This).

inst_new__2(I, A, _) :- I > A, !.
inst_new__2(I, A, This) :-
	'$meta_itype'(This, I, IType),
	% Create new predicates
	( IType = i(_/Arity) ->
	    '$pred__new'(Arity, Root),
	    arg(I, This, Root)
	; IType = l(_) ->
	    true
	),
	I1 is I + 1,
	inst_new__2(I1, A, This).

% Destroy an instance
:- export('$inst_destroy'/1).
'$inst_destroy'(This) :-
	functor(This, _, A),
	inst_destroy__2(1, A, This).

inst_destroy__2(I, A, _) :- I > A, !.
inst_destroy__2(I, A, This) :-
	'$meta_itype'(This, I, IType),
	% Destroy the predicate
	( IType = i(_) ->
	    arg(I, This, Root),
	    '$pred__free'(Root)
	; IType = l(_) ->
	    true
	),
	I1 is I + 1,
	inst_destroy__2(I1, A, This).

% ---------------------------------------------------------------------------

:- export('\6\obj_attr_unify'/3).
:- '$props'('\6\obj_attr_unify'/3, [impnat=cbool(prolog_ooget2)]).

% ---------------------------------------------------------------------------
% IO

:- use_module(engine(streams_basic)).
:- use_module(engine(ql_inout)).
:- use_module(engine(internals), ['$assertz_root'/3, '$current_nb_root'/3]).
:- use_module(compiler(open_and_protect)).

% Read the instance from a file

% TODO: not very clean code...
% TODO: throw exceptions when read of write failed

%:- use_module(compiler(errlog)).
% TODO: improve efficiency...
:- export('$inst_read'/3).
'$inst_read'(Name, N/A, This) :-
	% Create empty
	'$inst_new'(N/A, This),
	% Read attrs
	( inst_read__2(Name, This) ->
	    true
	; '$inst_destroy'(This),
%	  errlog:trace(['cannot read ', Name]),
	  fail
	).

inst_read__2(Name, This) :-
	'$open'(Name, r, Stream),
	'$qread_begin'(Stream),
	( '$qread'(X),
%	  errlog:trace(['reitem ', X]),
	  inst_read__items(X, 1, This) ->
	    true
	; '$qread_end',
	  close(Stream),
	  fail
	),
	'$qread_end',
	close(Stream).

%:- use_module(engine(io_basic)).
inst_read__items(X, I, This) :-
%	display(f(I, X)), nl,
	'$meta_itype'(This, I, IType),
	arg(I, This, Y),
	inst_read__items__2(IType, Y, X, I, This).

inst_read__items__2(IType, Y, X, I, This) :-
	IType = l(N),
	!,
	functor(X, N, 1),
	arg(1, X, Y),
	% Next
        ( '$qread'(X2) ->
%	errlog:trace(['reitem2 ', X2]),
	    I1 is I + 1, % nothing more
	    inst_read__items(X2, I1, This)
        ; true % finish
        ).
inst_read__items__2(IType, Root, X, I, This) :-
	IType = i(N/A),
	!,
%	display(user_error, a(IType, X, I, N, A)), nl(user_error),
% TODO: remove, debug test
%	functor(X, N, 1),
%	display(user_error, b(IType, X, I)), nl(user_error),
	( functor(X, N, A) ->
%	    display(user_error, f(X, Root)), nl(user_error),
	    '$assertz_root'(X, 'basiccontrol:true', Root),
	    % Next
            ( '$qread'(X2) ->
%	    errlog:trace(['reitem3 ', X2]),
	        inst_read__items(X2, I, This)
            ; true % finish
            )
	; I1 is I + 1, % nothing to read, move to next attr
	  inst_read__items(X, I1, This)
	).

% Write the instance to a file

:- use_module(engine(prolog_flags)).
:- use_module(engine(exceptions)).

% TODO: improve efficiency...
% TODO: on error, old fileerrors flag value is lost
:- export('$inst_write'/2).
'$inst_write'(Name, This) :-
	prolog_flag(fileerrors, OldFE, off),
	( open_and_protect(Name, Stream, Ref) ->
	    '$qwrite_begin'(0, Stream), % TODO: improve, 0 means "use abscurr"
	    functor(This, _, A),
	    inst_write__2(1, A, This),
	    '$qwrite_end',
	    close(Stream),
	    end_protect(Ref)
	; throw('$inst_write'(Name)) % TODO: improve exception
	),
	set_prolog_flag(fileerrors, OldFE).

inst_write__2(I, A, _) :- I > A, !.
inst_write__2(I, A, This) :-
	'$meta_itype'(This, I, IType),
	( IType = i(_) ->
	    arg(I, This, Root),
	    % write all the attr facts
            ( '$current_nb_root'(X, 'basiccontrol:true', Root),
%	      errlog:trace(['itemwri ', I, ' ', X]),
	      '$qwrite'(X),
	      fail
	    ; true
	    )
	; IType = l(N) ->
	    % write the attr
	    arg(I, This, Y),
	    functor(X, N, 1),
	    arg(1, X, Y),
%	    errlog:trace(['itemwrl ', I, ' ', X]),
	    '$qwrite'(X)
	; fail
	),
	I1 is I + 1,
	inst_write__2(I1, A, This).

% ---------------------------------------------------------------------------

% The class of anything (except m_any)
% TODO: Equivalent to the term/1 type?
% TODO: Should this be an interface? or an abstract class?
% TODO: This class is added automatically if not imported; fix: import class
:- public class any {
    :- '$statemodel'(single).
    :- '$raw_state'.
    % TODO: missing instance_of__/1
}.

% The class of anything that can be replaced (using _ <- _)
% TODO: This should be an interface? or abstract class?
:- public class m_any {
    :- '$statemodel'(pair).
    :- '$raw_state'.
    % TODO: missing instance_of__/1
}.

% A m_term for integers
:- public class m_int {
    :- '$statemodel'(pair).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- public inc/1.
    :- '$props'(inc/1, [unfold([c])]).
    inc(X) :-
	'$ctx_value'('$self', Value0),
	Value is Value0 + X,
	'$ctx_set'('$self', Value).

    :- public dec/1.
    :- '$props'(dec/1, [unfold([c])]).
    dec(X) :-
	'$ctx_value'('$self', Value0),
	Value is Value0 - X,
	'$ctx_set'('$self', Value).
}.

% This is a list with a reference to the last element (aka difference lists)
% TODO: Only the last element is represented. We could represent the head and the last
% everywhere and simplify programs by deforesting the structure and slicing arguments.
:- public class accum {
    :- '$statemodel'(pair).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    % Adds an element to the end of list
    % (by instantiating the end with a new list constructor)
    :- public add/1.
    :- '$props'(add/1, [unfold([c])]).
    add(X) :-
        '$ctx_value'('$self', [X|Xs]),
	'$ctx_set'('$self', Xs).
}.

% This is a list accumulator.
:- public class revaccum {
    :- '$statemodel'(pair).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    % Adds an element to the beginning of list
    :- public insert0/1.
    :- '$props'(insert0/1, [unfold([c])]).
    insert0(X) :-
	'$ctx_value'('$self', Xs),
	'$ctx_set'('$self', [X|Xs]).
}.

% Dictionary class wrapper
:- public class m_dic {
    :- '$statemodel'(pair).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- public constant is_empty/0.
    :- '$props'(is_empty/0, [unfold([])]).
    is_empty :- var(~self).

    :- public replace/2.
    :- '$props'(replace/2, [unfold([c,c])]).
    replace(K, V) :- self <- ~dic_replace(~self, K, V).

    :- public constant lookup/2.
    :- '$props'(lookup/2, [unfold([c, c])]).
    lookup(K, V) :- dic_lookup(~self, K, V).

    :- public constant lookup/3.
    :- '$props'(lookup/3, [unfold([c, c, c])]).
    lookup(K, V, Status) :- dic_lookup(~self, K, V, Status).

    :- public constant get/2.
    :- '$props'(get/2, [unfold([c, c])]).
    get(K, V) :- dic_get(~self, K, V).
}.

% Dictionary class wrapper (use-only; based on partially instantiated structures)
:- public class u_dic {
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- public constant is_empty/0.
    :- '$props'(is_empty/0, [unfold([])]).
    is_empty :- var(~self).

    :- public constant lookup/2.
    :- '$props'(lookup/2, [unfold([c, c])]).
    lookup(K, V) :- dic_lookup(~self, K, V).

    :- public constant lookup/3.
    :- '$props'(lookup/3, [unfold([c, c, c])]).
    lookup(K, V, Status) :- dic_lookup(~self, K, V, Status).

    :- public constant get/2.
    :- '$props'(get/2, [unfold([c, c])]).
    get(K, V) :- dic_get(~self, K, V).
}.


