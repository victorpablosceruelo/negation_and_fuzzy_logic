:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_ctype))).
:- include(.(include(c_string))).

:- '$native_weak_inline'(include('engine/revcomp.pl-2.native.h')).

:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'malloc.h').
:- lowinclude_foreign(predef_h, 'string.h').
:- lowinclude_foreign(predef_h, 'limits.h').

%:- globalvar(iubpairs/1) + lowentry(ref0(array(ref0(mut(array(mut(char)))))), 'iubpairs') + prop(foreign__static).
:- globalvar(iubpairs/1) + lowentry(ref0(array(ref0(array(ref0(char))))), 'iubpairs') + prop(foreign__static).
% TODO: Rewrite internal 'lowinst' support: just evaluate the goal at compile time and emit a C structure that holds the same result
% TODO: simplify array def... take type from elements, check that array is homogeneous
% TODO: define as a global variable?
% TODO: something like :- global(iubpairs/1) + type(...) 
iubpairs__(T) :-
	T = ~'$array_elems'(~'$array'(ref0(array(ref0(char), 2)), [
	  ~'$array_elems'(~'$array_r0'(char, [0'A, 0'T])),
	  ~'$array_elems'(~'$array_r0'(char, [0'C, 0'G])),
	  ~'$array_elems'(~'$array_r0'(char, [0'B, 0'V])),
	  ~'$array_elems'(~'$array_r0'(char, [0'D, 0'H])),
	  ~'$array_elems'(~'$array_r0'(char, [0'K, 0'M])),
	  ~'$array_elems'(~'$array_r0'(char, [0'R, 0'Y])),
	  ~'$array_elems'(~'$array_r0'(char, [0, 0]))
        ])).

:- globalvar(iubComplement/1) + lowentry(ref0(array(ref0(mut(ref0(char))))), 'iubComplement') + prop(foreign__static).
iubComplement__(T) :-
	T = ~'$uninit'(ref0(array(ref0(mut(ref0(char))), '1+UCHAR_MAX'))).

:- pred buildIubComplement/0 + lowentry(det, [], 'buildIubComplement') + prop(foreign__static).
buildIubComplement :-
	'$for_each'(I, ~intrangeclosed(0, ~'$ccons'('UCHAR_MAX', intmach)), (
          (~iubComplement)[@I] <- ~'$trust_typed'(@I, ref0(char))
	)),
	I <- 0,
	% TODO: allow comparison of rev0(char), rev1(char), etc.
	'$while'((~iubpairs)[@I][0] \== ~'$ccons'(0, ref0(char)), (
	  (~iubComplement)[~'$cast'((~iubpairs)[@I][0], intmach)] <- (~iubpairs)[@I][1],
    	  (~iubComplement)[~'$cast'((~iubpairs)[@I][1], intmach)] <- (~iubpairs)[@I][0],
    	  (~iubComplement)[~tolower((~iubpairs)[@I][0])] <- (~iubpairs)[@I][1],
    	  (~iubComplement)[~tolower((~iubpairs)[@I][1])] <- (~iubpairs)[@I][0],
	  I <- @I + 1
        )).

% TODO: do not use $cast, and define it only to convert between numeric types (with bigger precession), define a $cast_trunc to allow castings to types with less precission.
:- pred inPlaceReverse/2 + lowentry(det, [ref1(array(ref0(mut(ref0(char))))), intmach], 'inPlaceReverse') + prop(foreign__static).
inPlaceReverse(Strand, Len0) :-
	I = ~initmut(intmach, 0),
	Len = ~initmut(intmach, Len0 - 1),
	'$while'(@I < @Len, (
    	  C = @Strand[@I],
    	  Strand[@I] <- @((~iubComplement)[~'$cast'(@Strand[@Len], intmach)]),
    	  Strand[@Len] <- @((~iubComplement)[~'$cast'(C, intmach)]),
	  I <- @I + 1,
	  Len <- @Len - 1
        )),
	( @I == @Len ->
	    Strand[@I] <- @((~iubComplement)[~'$cast'(@Strand[@I], intmach)])
	; true
	).

:- pred process/2 + lowentry(det, [ref1(array(ref0(mut(ref0(char))))), intmach], 'process') + prop(foreign__static).
process(Strand, Len0) :-
	inPlaceReverse(Strand, Len0),
        % trick to print from 60 to 60 characters
	S = ~initmut(ref1(array(ref0(mut(ref0(char))))), Strand),
	Len = ~initmut(intmach, Len0),
	'$while'(@Len > 60, (
	  % save character at position S+60, put a 0 there and print
	  C = @S[60],
	  S[60] <- ~'$ccons'(0, ref0(char)),
	  puts(@S),
	  % recover nulled character, move S forward and continue with loop
	  S[60] <- C,
	  S <- ~'$subarray'(@S, 60),
	  Len <- @Len - 60
        )),
	% print rest of characters
        S[@Len] <- ~'$ccons'(0, ref0(char)),
	puts(@S).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	% TODO: buffer was a local variable defined as "static char buffer[1024]". That is difficult to express here... maybe using a predicate abstraction? or a global variable (defined externally) whose root is only initially visible from a given predicate
	Buffer = ~'$alloc'(alloca, array(ref0(mut(ref0(char))), 1024)),
	Inp = ~initmut(ref1(array(ref0(mut(ref0(char))))), ~'$alloc'(malloc, array(ref0(mut(ref0(char))), 129))),
	MLen = ~initmut(intmach, 128),
	SLen = ~initmut(intmach, 0),
	%
	buildIubComplement,
	%
	loop(Buffer, Inp, MLen, SLen),
	%
        ( @SLen > 0 ->
	    process(@Inp, @SLen),
	    '$dealloc'(malloc, @Inp)
	; true
	).

:- pred loop/4 + prop(subpr).
loop(Buffer, Inp, MLen, SLen) :-
	R = ~fgets(~'$trust_typed'(Buffer, mut(char)), 1023, ~'$ccons'(stdin, ref1('FILE'))),
	( R \== ~'$ccons'('NULL', mut(char)) ->
	    ( @Buffer[0] == ~'$ccons'(0'>, ref0(char)) ->
	        ( @SLen > 0 ->
		    process(@Inp, @SLen),
		    SLen <- 0
		; true
		),
		printf2("%s", Buffer)
	    ; L = ~initmut(intmach, ~strlen(~'$trust_typed'(Buffer, cstring))),
	      '$while'((@L > 0, \+ isalpha(@Buffer[@L - 1])), L <- @L - 1),
	      '$while'(@SLen + @L > @MLen, (
    	    	MLen <- @MLen + @MLen,
		Inp <- ~'$resize'(malloc, @Inp, @MLen + 1)
	      )),
	      memcpy(~'$trust_typed'(~'$subarray'(@Inp, @SLen), cptr), ~'$trust_typed'(Buffer, cptr), @L),
	      SLen <- @SLen + @L
	    ),
	    loop(Buffer, Inp, MLen, SLen)
	; true
	).

:- '$improlog_end'.
