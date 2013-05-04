:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_string))).

:- '$native_weak_inline'(include('engine/fasta.native.h')).

:- '$improlog_begin'.

:- pred im/1 + lowentrymacrocons(intmach, 'IM').
im := 139968.
:- pred ia/1 + lowentrymacrocons(intmach, 'IA').
ia := 3877.
:- pred ic/1 + lowentrymacrocons(intmach, 'IC').
ic := 29573.

% TODO: that global variable was a local STATIC variable in gen_random/2, fix it!
%    static long last = 42;
:- globalvar(gen_random__last/1) + lowentry(mut(intmach), 'gen_random__last').
gen_random__last__(T) :- T <- ~'$typed'(intmach, 42). % initializer

:- pred gen_random/2 + lowentryfun([flt64], flt64, 'gen_random').
gen_random(Max, R) :-
	~gen_random__last <- (@(~gen_random__last) * ~ia + ~ic) mod ~im,
	R = Max * ~'$trust_typed'(@(~gen_random__last), flt64) / ~'$trust_typed'(~im, flt64).

:- lowtype(aminoacids).
:- class aminoacids {
  :- struct.
  :- attr c :: ref0(char).
  :- mut p :: flt64.
}.

% Weighted selection from alphabet
:- pred makeCumulative/2 + lowentry(det, [ref1(array(ref0(aminoacids))), intmach], 'makeCumulative').
makeCumulative(Genelist, Count) :-
	Cp = ~initmut(flt64, 0.0),
	'$for_each'(I, ~intrange(Count), (
          Cp <- @Cp + @Genelist[@I].p,
	  Genelist[@I].p <- @Cp
        )).

% TODO: missing 'const' in first argument, prototype was: char selectRandom (const struct aminoacids * genelist, int count)
:- pred selectRandom/3 + lowentryfun([ref1(array(ref0(aminoacids))), intmach], char, 'selectRandom').
selectRandom(Genelist, Count, Result) :-
	R = ~gen_random(1.0),
	( R < @Genelist[0].p ->
	   Result = Genelist[0].c
	; % TODO: is this a dicotomic search?
	  Lo = ~initmut(intmach, 0),
	  Hi = ~initmut(intmach, Count-1),
	  '$while'(@Hi > @Lo + 1, (
            I = (@Hi + @Lo) / 2,
	    ( R < @Genelist[I].p ->
	        Hi <- I
	    ; Lo <- I
	    )
          )),
	  Result = Genelist[@Hi].c
	).

% Generate and write FASTA format

:- pred line_length/1 + lowentrymacrocons(intmach, 'LINE_LENGTH').
line_length := 60.

% TODO: missing 'const' in argument declarations, prototype was: void makeRandomFasta (const char * id, const char * desc, const struct aminoacids * genelist, int count, int n)
:- pred makeRandomFasta/5 + lowentry(det, [cstring, cstring, ref1(array(ref0(aminoacids))), intmach, intmach], 'makeRandomFasta').
makeRandomFasta(Id, Desc, Genelist, Count, N) :-
	%
	printf3(">%s %s\n", Id, Desc),
	%
	% TODO: it was a local variable defined as: char pick[LINE_LENGTH+1];
	Pick = ~'$alloc'(alloca, array(ref0(mut(ref0(char))), ~line_length + 1)),
	%
	Todo = ~initmut(intmach, N),
	'$while'(@Todo > 0, (
	  M = ~newmut(intmach),
	  ( @Todo < ~line_length ->
	      M <- @Todo
	  ; M <- ~line_length
	  ),
	  '$for_each'(I, ~intrange(@M), (
	    Pick[@I] <- ~selectRandom(Genelist, Count)
	  )),
	  Pick[@M] <- ~'$trust_typed'(0, char), % TODO: add some property like... nullended(Pick)
	  puts(Pick),
	  Todo <- @Todo - ~line_length
        )).

% TODO: missing 'const' in argument declarations, prototype was: makeRepeatFasta (const char * id, const char * desc, const char *s, int n)
:- pred makeRepeatFasta/4 + lowentry(det, [cstring, cstring, ref1(array(ref0(mut(char)))), intmach], 'makeRepeatFasta').
makeRepeatFasta(Id, Desc, S, N) :-
	KN = ~strlen(~'$trust_typed'(S, cstring)),

	SS = ~'$alloc'(malloc, array(ref0(mut(ref0(char))), KN + 1)),
	memcpy(~'$trust_typed'(SS, cptr), ~'$trust_typed'(S, cptr), KN + 1),

	printf3(">%s %s\n", Id, Desc),

	Todo = ~initmut(intmach, N),
	K = ~initmut(intmach, 0),
	'$while'(@Todo > 0, (
	  M = ~newmut(intmach),
	  ( @Todo < ~line_length ->
	      M <- @Todo
	  ; M <- ~line_length
	  ),

	  '$while'(@M >= KN - @K, (
             printf2("%s", ~'$subarray'(S, @K)),
	     M <- @M - (KN - @K),
	     K <- 0
	  )),			   

	  SS[@K + @M] <- ~'$trust_typed'(0, char),
	  puts(~'$subarray'(SS, @K)),
	  SS[@K + @M] <- @S[@M + @K],
	  K <- @K + @M,
	  Todo <- @Todo - ~line_length
        )),
	free(SS).

/* Main -- define alphabets, make 3 fragments */

:- globalvar(iub/1) + lowentry(ref0(array(ref0(aminoacids))), 'iub').
iub__(T) :-
% TODO: WRONG! use struct initializers
	T = ~'$array_elems'(~'$array'(ref0(aminoacids), [
	  ~'$array_elems'(~'$array_r0'(char, [0'a, 0.27])),
	  ~'$array_elems'(~'$array_r0'(char, [0'c, 0.12])),
	  ~'$array_elems'(~'$array_r0'(char, [0'g, 0.12])),
	  ~'$array_elems'(~'$array_r0'(char, [0't, 0.27])),

	  ~'$array_elems'(~'$array_r0'(char, [0'B, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'D, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'H, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'K, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'M, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'N, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'R, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'S, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'V, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'W, 0.02])),
	  ~'$array_elems'(~'$array_r0'(char, [0'Y, 0.02]))
        ])).
% TODO: improve!
:- pred iub_len/1 + lowentrymacrocons(intmach, 'IUB_LEN').
iub_len := ~'$ccons'('(sizeof(iub) / sizeof(aminoacids_t))', intmach).

:- globalvar(homosapiens/1) + lowentry(ref0(array(ref0(aminoacids))), 'homosapiens').
homosapiens__(T) :-
% TODO: WRONG! use struct initializers
	T = ~'$array_elems'(~'$array'(ref0(aminoacids), [
	  ~'$array_elems'(~'$array_r0'(char, [0'a, 0.3029549426680])),
	  ~'$array_elems'(~'$array_r0'(char, [0'c, 0.1979883004921])),
	  ~'$array_elems'(~'$array_r0'(char, [0'g, 0.1975473066391])),
	  ~'$array_elems'(~'$array_r0'(char, [0't, 0.3015094502008]))
        ])).
:- pred homosapiens_len/1 + lowentrymacrocons(intmach, 'HOMOSAPIENS_LEN').
homosapiens_len := ~'$ccons'('(sizeof(homosapiens) / sizeof(aminoacids_t))', intmach).

:- globalvar(alu/1) + lowentry(ref1(array(ref0(mut(char)))), 'alu').
alu__(T) :-
	T = ~'$trust_typed'(~'$cstring'(
	  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"||
	  "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"||
	  "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"||
	  "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"||
	  "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"||
	  "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"||
	  "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"
        ), ref1(array(ref0(mut(char))))).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	makeCumulative(~'$to_ref1'(~iub), ~iub_len),
	makeCumulative(~'$to_ref1'(~homosapiens), ~homosapiens_len),
	%
	makeRepeatFasta("ONE", "Homo sapiens alu", ~alu, N*2),
	makeRandomFasta("TWO", "IUB ambiguity codes", ~'$to_ref1'(~iub), ~iub_len, N*3),
	makeRandomFasta("THREE", "Homo sapiens frequency", ~'$to_ref1'(~homosapiens), ~homosapiens_len, N*5).

:- '$improlog_end'.
