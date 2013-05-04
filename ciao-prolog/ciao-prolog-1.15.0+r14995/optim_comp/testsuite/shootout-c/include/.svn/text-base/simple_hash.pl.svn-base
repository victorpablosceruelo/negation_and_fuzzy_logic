% A hashtable map (from cstring to intmach)
%
% Based on C code $Id: simple_hash.h,v 1.1.1.1 2004-05-19 18:09:07 bfulgham Exp $

:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_string))).

:- '$improlog_begin'.

:- pred ht_num_primes/1 + lowentrymacrocons(intmach, 'ht_num_primes').
ht_num_primes := ~'$ccons'('(sizeof(ht_prime_list) / sizeof(uint32))', intmach).

% TODO: missing 'static'
:- globalvar(ht_prime_list/1) + lowentry(ref0(array(ref0(uint32))), 'ht_prime_list').
ht_prime_list__(T) :- T = ~'$array_elems'(~'$array'(ref0(uint32), [
    ~'$ccons'('53ul', uint32),         ~'$ccons'('97ul', uint32),
    ~'$ccons'('193ul', uint32),        ~'$ccons'('389ul', uint32),
    ~'$ccons'('769ul', uint32),        ~'$ccons'('1543ul', uint32),
    ~'$ccons'('3079ul', uint32),       ~'$ccons'('6151ul', uint32),
    ~'$ccons'('12289ul', uint32),      ~'$ccons'('24593ul', uint32),
    ~'$ccons'('49157ul', uint32),      ~'$ccons'('98317ul', uint32),
    ~'$ccons'('196613ul', uint32),     ~'$ccons'('393241ul', uint32),
    ~'$ccons'('786433ul', uint32),     ~'$ccons'('1572869ul', uint32),
    ~'$ccons'('3145739ul', uint32),    ~'$ccons'('6291469ul', uint32),
    ~'$ccons'('12582917ul', uint32),   ~'$ccons'('25165843ul', uint32),
    ~'$ccons'('50331653ul', uint32),   ~'$ccons'('100663319ul', uint32),
    ~'$ccons'('201326611ul', uint32),  ~'$ccons'('402653189ul', uint32),
    ~'$ccons'('805306457ul', uint32),  ~'$ccons'('1610612741ul', uint32),
    ~'$ccons'('3221225473ul', uint32), ~'$ccons'('4294967291ul', uint32)
])).

:- lowtype(ht_node).
:- class ht_node {
  :- struct.
  :- mut key :: cstring.
  :- mut val :: intmach.
  :- mut next :: ref1(ht_node).
}.

:- lowtype(ht_ht).
:- class ht_ht {
  :- struct.
  :- mut size :: intmach.
  % struct ht_node **tbl, % but it is an array of r1(ht_node)...
  :- mut tbl :: ref1(array(ref0(mut(ref1(ht_node))))).
  :- mut iter_index :: intmach.
  :- mut iter_next :: ref1(ht_node). % struct ht_node *iter_next,
  :- mut items :: intmach.
  :- if('$use_opt'(ht_debug)).
    :- mut collisions :: intmach.
  :- endif.
}.

:- pred ht_val/2 + lowentryfun([ref1(ht_node)], intmach, 'ht_val') + prop(foreign__inline).
ht_val(Node, Result) :-
	Result = @Node.val.

:- pred ht_key/2 + lowentryfun([ref1(ht_node)], cstring, 'ht_key') + prop(foreign__inline).
ht_key(Node, Result) :-
	Result = @Node.key.

:- pred ht_hashcode/3 + lowentryfun([ref1(ht_ht), cstring], intmach, 'ht_hashcode') + prop(foreign__inline).
% TODO: alternatives in functional notation:
%   ~ht_hashcode(Ht, Key) = Result :- ??
%   ht_hashcode(Ht, Key) = Result :- ??
ht_hashcode(Ht, Key, Result) :-
	Val = ~initmut(uint32, ~'$ccons'(0, uint32)),
	% TODO: in the original C implementation, the variable for Key was reused as a moving pointer
	KeyP = ~initmut(cstring, Key),
	'$while'(@(@KeyP) \== ~'$ccons'(0, char), (
	  Val <- ~'$trust_typed'(5 * ~'$trust_typed'(@Val, intmach) + ~'$trust_typed'(@(@KeyP), intmach), uint32),
	  KeyP <- ~'$mut_move'(@KeyP, 1)
        )),
	Result = ~'$trust_typed'(~'$trust_typed'(@Val, intmach) mod @Ht.size, intmach).

:- pred ht_node_create/2 + lowentryfun([cstring], ref1(ht_node), 'ht_node_create').
ht_node_create(Key, Result) :-
	Node = ~'$alloc'(malloc, ht_node),
	( Node == ~'$null_ref1'(ht_node) ->
	    perror("malloc ht_node"),
	    exit(1)
	; true
	),
	Newkey = ~strdup(Key),
	( Newkey == ~'$ccons'('NULL', cstring) ->
	    perror("strdup newkey"),
	    exit(1)
	; true
	),
	Node.key <- Newkey,
	Node.val <- 0,
	Node.next <- ~'$null_ref1'(ht_node),
	Result = Node.

:- pred ht_create/2 + lowentryfun([intmach], ref1(ht_ht), 'ht_create').
ht_create(Size, Result) :-
	Ht = ~'$alloc'(malloc, ht_ht),
	I = ~initmut(intmach, 0),
	% TODO: use unsigned ints for size!!! do not use that trust_type to intmach
	'$while'(~'$trust_typed'((~ht_prime_list)[@I], intmach) < Size, I <- @I + 1),
	Ht.size <- ~'$trust_typed'((~ht_prime_list)[@I], intmach),
	Ht.tbl <- ~'$alloc'(malloc, array(ref0(mut(ref1(ht_node))), @Ht.size)),
	% TODO: memory is automatically set to 0 if we use calloc!!! change? use memset?
	'$for_each'(J, ~intrange(@Ht.size), (
	  Ht.tbl[@J] <- ~'$null_ref1'(ht_node)
        )),
	Ht.iter_index <- 0,
	Ht.iter_next <- ~'$null_ref1'(ht_node),
	Ht.items <- 0,
	( '$use_opt'(ht_debug) ->
	    Ht.collisions <- 0
	; true
	),
	Result = Ht.

:- pred ht_destroy/1 + lowentry(det, [ref1(ht_ht)], 'ht_destroy').
ht_destroy(Ht) :-
	( '$use_opt'(ht_debug) ->
	    Chain_len = ~mut(intmach), 
	    Max_chain_len = ~initmut(intmach, 0),
	    Density = ~initmut(intmach, 0),
	    fprintf3(~stderr, " HT: size            %d\n", @Ht.size),
	    fprintf3(~stderr, " HT: items           %d\n", @Ht.items),
	    fprintf3(~stderr, " HT: collisions      %d\n", @Ht.collisions)
	; true
	),
	'$for_each'(I, ~intrange(@Ht.size), (
	  Next = ~initmut(ref1(ht_node), @Ht.tbl[@I]),
	  ( '$use_opt'(ht_debug) ->
	      ( @Next \== ~'$null_ref1'(ht_node) ->
		  Density <- @Density + 1
	      ; true
	      ),
	      Chain_len <- 0
	  ; true
	  ),
	  '$while'(@Next \== ~'$null_ref1'(ht_node), (
	    Cur = @Next,
	    Next <- @Next.next,
	    '$dealloc'(malloc, @Cur.key),
	    '$dealloc'(malloc, Cur),
	    ( '$use_opt'(ht_debug) ->
	        Chain_len <- @Chain_len + 1
	    ; true
	    )
	  )),
	  ( '$use_opt'(ht_debug) ->
	      ( @Chain_len > @Max_chain_len ->
		  Max_chain_len <- @Chain_len
	      ; true
	      )
	  ; true
	  )
        )),
	'$dealloc'(malloc, @Ht.tbl),
	'$dealloc'(malloc, Ht),
	( '$use_opt'(ht_debug) ->
	    fprintf3(~stderr, " HT: density         %d\n", @Density),
	    fprintf3(~stderr, " HT: max chain len   %d\n", @Max_chain_len)
	; true
	).

:- pred ht_find/3 + lowentryfun([ref1(ht_ht), cstring], ref1(ht_node), 'ht_find') + prop(foreign__inline).
ht_find(Ht, Key, Result) :-
	Hash_code = ~ht_hashcode(Ht, Key),
	Node = ~initmut(ref1(ht_node), @Ht.tbl[Hash_code]),
	ht_find__2(Ht, Key, Node, Result).

:- pred ht_find__2/4 + prop(subpr).
% TODO: this could be written with an iteration pattern... $first_that(Node, ~strcmp(Key, @Node.Key) == 0, ~nodes(Ht), Result)
ht_find__2(Ht, Key, Node, Result) :-
	( @Node \== ~'$null_ref1'(ht_node) ->
	    ( ~strcmp(Key, @Node.key) == 0 ->
	        Result = @Node % key found
	    ; Node <- @Node.next,
	      ht_find__2(Ht, Key, Node, Result) % loop
	      % TODO: it should be possible to reuse input variables and write ht_find_new__2(Ht, Key, @Node.next, Result) (i.e. Node does not need to be a mutable)
	    )
	; Result = ~'$null_ref1'(ht_node)
	).

:- pred ht_find_new/3 + lowentryfun([ref1(ht_ht), cstring], ref1(ht_node), 'ht_find_new') + prop(foreign__inline).
ht_find_new(Ht, Key, Result) :-
	Hash_code = ~ht_hashcode(Ht, Key),
	% TODO: then, define maybe_ht_node := r1(ht_node) | null.
	% TODO: the next line should be Prev <- ~'$typed'(maybe_ht_node, null)
	% TODO: or maybe... use maybe
	% TODO: extend t_dep definition to support single null values (use that for ERRORTAG in tagged/1 ... and put assertions everywhere)
	Prev = ~initmut(ref1(ht_node), ~'$null_ref1'(ht_node)),
	Node = ~initmut(ref1(ht_node), @Ht.tbl[Hash_code]),
	ht_find_new__2(Ht, Key, Prev, Node, Hash_code, Result).

:- pred ht_find_new__2/6 + prop(subpr).
ht_find_new__2(Ht, Key, Prev, Node, Hash_code, Result) :-
	% TODO: with maybe_ht_node, it could be written directly as Node \== null
	( @Node \== ~'$null_ref1'(ht_node) ->
	    ( ~strcmp(Key, @Node.key) == 0 ->
	        Result = @Node % key found
	    ; Prev <- @Node,
	      Node <- @Node.next,
	      ( '$use_opt'(ht_debug) ->
		  Ht.collisions <- @Ht.collisions + 1
	      ; true
	      ),
	      ht_find_new__2(Ht, Key, Prev, Node, Hash_code, Result) % loop
	    )
	; % key not found, add new node
	  Ht.items <- @Ht.items + 1,
	  ( @Prev \== ~'$null_ref1'(ht_node) ->
	      NewNode = ~ht_node_create(Key),
	      Prev.next <- NewNode,
	      Result = NewNode
	  ; NewNode = ~ht_node_create(Key),
	    Ht.tbl[Hash_code] <- NewNode,
	    Result = NewNode
	  )
	).

% Hash Table iterator data/functions
:- pred ht_next/2 + lowentryfun([ref1(ht_ht)], ref1(ht_node), 'ht_next') + prop(foreign__inline).
ht_next(Ht, Result) :-
	Node = @Ht.iter_next,
	( Node \== ~'$null_ref1'(ht_node) ->
	    Ht.iter_next <- @Node.next,
	    Result = Node
	; ht_next__2(Ht, Result)
	).	

:- pred ht_next__2/2 + prop(subpr).
ht_next__2(Ht, Result) :-
	( @Ht.iter_index < @Ht.size ->
	    Index = @Ht.iter_index,
	    Ht.iter_index <- Index + 1,
	    ( @Ht.tbl[Index] \== ~'$null_ref1'(ht_node) ->
		Ht.iter_next <- @Ht.tbl[Index].next,
		Result = @Ht.tbl[Index]
	    ; ht_next__2(Ht, Result)
	    )
	; Result = ~'$null_ref1'(ht_node)
	).

:- pred ht_first/2 + lowentryfun([ref1(ht_ht)], ref1(ht_node), 'ht_first') + prop(foreign__inline).
ht_first(Ht, Result) :-
	Ht.iter_index <- 0,
	Ht.iter_next <- ~'$null_ref1'(ht_node),
	Result = ~ht_next(Ht).

:- pred ht_count/2 + lowentryfun([ref1(ht_ht)], intmach, 'ht_count') + prop(foreign__inline).
ht_count(Ht, Result) :-
	Result = @Ht.items.
% TODO: can I rewrite it as ht_count(Ht) := @Ht.items. ?

:- '$improlog_end'.
