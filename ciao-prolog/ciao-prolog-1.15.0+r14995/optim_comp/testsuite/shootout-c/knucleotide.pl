:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_ctype))).
:- include(.(include(c_string))).

:- '$native_weak_inline'(include('engine/knucleotide.native.h')).

:- include(.(include(simple_hash))).

:- '$improlog_begin'.

:- pred hash_table_size/3 + lowentryfun([intmach, intmach], intmach, 'hash_table_size').
hash_table_size(Fl0, Buflen, Size) :-
	Fl = ~initmut(intmach, Fl0), % TODO: original code reused Fl and changed its value, is that as efficient as the original code?
	Maxsize1 = Buflen - @Fl,
	Maxsize2 = ~initmut(intmach, 4),
	hash_table_size__2(Fl, Maxsize1, Maxsize2),
	( Maxsize1 < @Maxsize2 ->
	    Size = Maxsize1
	; Size = @Maxsize2
	).

:- pred hash_table_size__2/3 + prop(subpr).
hash_table_size__2(Fl, Maxsize1, Maxsize2) :-
	Fl <- @Fl - 1,
	( @Fl > 0, @Maxsize2 < Maxsize1 ->
            Maxsize2 <- @Maxsize2 * 4,
	    hash_table_size__2(Fl, Maxsize1, Maxsize2)
	; true
	).

:- pred generate_frequencies/4 + lowentryfun([intmach, ref1(array(ref0(mut(char)))), intmach], ref1(ht_ht), 'generate_frequencies').
generate_frequencies(Fl, Buffer, Buflen, Result) :-
	( Fl > Buflen ->
	    Result = ~'$null_ref1'(ht_ht)
	; Ht = ~ht_create(~hash_table_size(Fl, Buflen)),
	  '$for_each'(I, ~intrange(Buflen - Fl + 1), (
	    Reader = ~'$subarray'(Buffer, @I),
	    Nulled = @Reader[Fl],
	    Reader[Fl] <- ~'$ccons'(0x00, char), % TODO: add a property like nullended? substring with memory reuse? 
	    % TODO: document, check and improve: in for_each expansion, variables common only to the loop code are not shared between iterations
	    % TODO: add missing existential variable notation to force local variables in loops:
	    % e.g. Sum<-0,'$for_each'(I, [1,2,3,4], local I2 in (I2 = @I * @I, Sum <- @Sum + I2))
	    % e.g. Sum<-0,'$for_each'(I, [1,2,3,4], exists I2 in (I2 = @I * @I, Sum <- @Sum + I2))
	    Node = ~ht_find_new(Ht, ~'$trust_typed'(Reader, cstring)),
	    Node.val <- @Node.val + 1,
	    Reader[Fl] <- Nulled
	  )),
	  Result = Ht
	).

:- lowtype(ssorter).
:- class ssorter {
  :- struct.
  :- mut string :: cstring.
  :- mut num :: intmach.
}.

:- pred write_frequencies/3 + lowentry(det, [intmach, ref1(array(ref0(mut(char)))), intmach], 'write_frequencies').
write_frequencies(Fl, Buffer, Buflen) :-
	Tmp = ~newmut(ssorter), % TODO: add annotations...
	%
	Ht = ~generate_frequencies(Fl, Buffer, Buflen),
	%
	Total = ~initmut(intmach, 0),
	Size = ~initmut(intmach, 0),
        Nd = ~initmut(ref1(ht_node), ~ht_first(Ht)),
	% TODO: use maybe(ht_node) as type... use @Nd = yes(Nd2) instead, or define ht_node as a sum type that includes nil
	'$while'(@Nd \== ~'$null_ref1'(ht_node), (
          Total <- @Total + @Nd.val,
	  Size <- @Size + 1,
	  Nd <- ~ht_next(Ht)
        )),
	%
	S = ~'$alloc'(malloc, array(ref0(mut(ssorter)), @Size)),
	I0 = ~initmut(intmach, 0),
        Nd <- ~ht_first(Ht),
	'$while'(@Nd \== ~'$null_ref1'(ht_node), (
          S[@I0].string <- @Nd.key,
          S[@I0].num <- @Nd.val,
	  I0 <- @I0 + 1,
	  Nd <- ~ht_next(Ht)
        )),
	'$for_each'(I, ~intrange(@Size - 1), (
	  '$for_each'(J, ~intrange2(@I + 1, @Size), (
             ( @S[@I].num < @S[@J].num ->
	         Tmp <- @S[@I],
		 S[@I] <- @S[@J],
		 S[@J] <- @Tmp
	     ; true
	     )
          ))
        )),
	'$for_each'(I2, ~intrange(@Size), (
          printf3("%s %.3f\n", @S[@I2].string, 100.0 * ~'$cast'(@S[@I2].num, flt64) / ~'$trust_typed'(@Total, flt64))
        )),
	printf1("\n"),
	ht_destroy(Ht),
	'$dealloc'(malloc, S).

:- pred write_count/3 + lowentry(det, [cstring, ref1(array(ref0(mut(char)))), intmach], 'write_count').
write_count(SearchFor, Buffer, Buflen) :-
	Ht = ~generate_frequencies(~strlen(SearchFor), Buffer, Buflen),
	printf3("%d\t%s\n", @ (~ht_find_new(Ht, SearchFor)).val, SearchFor),
        ht_destroy(Ht).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	Line = ~'$alloc'(malloc, array(ref0(mut(char)), 256)),
	( Line == ~'$null_ref1'(array(ref0(mut(char)))) ->
	    exit(-1) % TODO: throw excetions
	; true
	),
	%
	Nothree = ~initmut(intmach, 1),
	'$while'((@Nothree \== 0, ~fgets(~'$trust_typed'(Line, mut(char)), 255, ~stdin) \== ~'$ccons'('NULL', mut(char))), (
	  ( @Line[0] == ~'$ccons'(0'>, char), @Line[1] == ~'$ccons'(0'T, char), @Line[2] == ~'$ccons'(0'H, char) ->
              Nothree <- 0
	  ; true
	  )
        )),
        '$dealloc'(malloc, Line),
	%
	Buflen = ~initmut(intmach, 10240),
	Buffer = ~initmut(ref1(array(ref0(mut(char)))), ~'$alloc'(malloc, array(ref0(mut(char)), @Buflen + 1))),
	( @Buffer == ~'$null_ref1'(array(ref0(mut(char)))) ->
	    exit(-1) % TODO: throw excetions
	; true
	),
	%
	Seqlen = ~initmut(intmach, 0),
	X = ~initmut(ref1(array(ref0(mut(char)))), @Buffer),
	read_loop(X, Buffer, Buflen, Seqlen),
	%
        % TODO: allow definitions such as?
        %  T.toupper := C :- char(T), !, C = ...
        % (if we overwrite +/3, why couldn't we do the same for #./3?)
        '$for_each'(I, ~intrange(@Seqlen), (
          Buffer[@I] <- ~toupper(@Buffer[@I])
        )),
	write_frequencies(1, @Buffer, @Seqlen),
	write_frequencies(2, @Buffer, @Seqlen),
	write_count("GGT", @Buffer, @Seqlen),
	write_count("GGTA", @Buffer, @Seqlen),
	write_count("GGTATT", @Buffer, @Seqlen),
	write_count("GGTATTTTAATT", @Buffer, @Seqlen),
	write_count("GGTATTTTAATTTATAGT", @Buffer, @Seqlen),
	'$dealloc'(malloc, @Buffer).

:- pred read_loop/4 + prop(subpr).
read_loop(X, Buffer, Buflen, Seqlen) :-
	R = ~fgets(~'$trust_typed'(@X, mut(char)), 255, ~stdin),
	( R \== ~'$ccons'('NULL', mut(char)) ->
	    Linelen = ~initmut(intmach, ~strlen(~'$trust_typed'(@X, cstring))),
	    ( @Linelen \== 0 ->
	        ( @X[@Linelen - 1] == ~'$ccons'(0'\n, char) -> Linelen <- @Linelen - 1 ; true ),
		C = @X[0],
		( C == ~'$ccons'(0'>, char) ->
		    true % end loop
		; C \== ~'$ccons'(0';, char) ->
		    Seqlen <- @Seqlen + @Linelen,
		    ( @Seqlen + 512 >= @Buflen ->
		        Buflen <- @Buflen + 10240,
			% TODO: realloc could be view as a internal pointer relocation, but here it appears at the program level: all references to the old Tmp will be incorrect; this operation is only safe if we prove that there is not any alive reference to the old content of Tmp.
			% TODO: note that '$subarray' is a dangerous operation if pointers are relocated!!
			Tmp = ~'$resize'(malloc, @Buffer, (@Buflen + 1)),
			( Tmp == ~'$null_ref1'(array(ref0(mut(char)))) ->
			    exit(-1) % TODO: throw exception
			; true
			),
			Buffer <- Tmp,
			X <- ~'$subarray'(@Buffer, @Seqlen)
		    ; X <- ~'$subarray'(@X, @Linelen)
		    ),
		    X[0] <- ~'$ccons'(0, char),
		    read_loop(X, Buffer, Buflen, Seqlen) % continue with next line
		; read_loop(X, Buffer, Buflen, Seqlen) % continue with next line
		)
	    ; read_loop(X, Buffer, Buflen, Seqlen) % continue with next line
	    )
       ; true % end loop
       ).

:- '$improlog_end'.
