:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_string))).
:- include(.(include(c_pcre))).

:- '$native_weak_inline'(define('__USE_STRING_INLINES', 1)). % TODO: this global option may also be read in included files!
:- '$native_weak_inline'(include('engine/regexdna.pl-2.native.h')).

:- '$improlog_begin'.

% TODO: 'struct' is a hint for the type analyzer, but it could also be a check-assertion... the user wants it to be a structure (no matter how complicated the definition is)
:- lowtype(fbuf).
:- class fbuf {
  :- struct.
  :- mut buf :: mut(char).
  :- mut size :: size.
  :- mut len :: size.
}.

:- pred fb_init/1 + lowentry(det, [ref1(fbuf)], 'fb_init') + prop(foreign__static).
fb_init(B) :-
	B.buf <- ~'$ccons'('NULL', mut(char)),
	B.len <- ~'$ccons'(0, size),
	B.size <- ~'$ccons'(0, size).

:- pred fb_need/3 + lowentryfun([ref1(fbuf), size], mut(char), 'fb_need') + prop(foreign__static).
fb_need(B, Need0, Ret) :-
	Need = ~'$trust_typed'(~'$trust_typed'(Need0, intmach) + ~'$trust_typed'(@B.len, intmach), size),
	( ~'$trust_typed'(Need, intmach) > ~'$trust_typed'(@B.size, intmach) ->
	    ( @B.size == ~'$ccons'(0, size) ->
	        B.size <- Need
	    ; '$while'(Need > @B.size, (
	 	 B.size <- ~'$trust_typed'(~'$trust_typed'(@B.size, intmach) + ~'$trust_typed'(@B.size, intmach), size)
	       ))
% TODO: allow nicer syntax like:
%	      double(B.size) while Need > @B.size
%	      while Need > @B.size do double(B.size)
 	    ),
% TODO: make it work with mut(char)... or use arrays for strings instead, mut(char) is a kludge
	    B.buf <- ~'$trust_typed'(
              ~'$resize'(malloc, ~'$trust_typed'(@B.buf, ref1(array(ref0(mut(char))))), ~'$trust_typed'(@B.size, intmach)),
              mut(char)),
	    ( @B.buf == ~'$ccons'('NULL', mut(char)) ->
	        exit(1) % TODO: throw exception instead
	    ; true
	    )
        ; true
	),
	Ret = ~'$mut_move'(@B.buf, ~'$trust_typed'(@B.len, intmach)).

:- pred fb_minread/1 + lowentrymacrocons(size, 'FB_MINREAD').
fb_minread := ~'$trust_typed'(3<<16, size).

% Read all of a stdio stream into dst buffer.
:- pred fb_readall/3 + lowentryfun([ref1(fbuf), ref1('FILE')], size, 'fb_readall') + prop(foreign__static).
fb_readall(Dst, Fp, Size) :-
	Dp = ~initmut(mut(char), ~fb_need(Dst, ~fb_minread)),
	fb_readall__2(Dp, Dst, Fp),
	( ~ferror(Fp) \== 0 ->
	    exit(1) % TODO: use throw
	; true
	),
	Size = @Dst.len.

:- pred fb_readall__2/3 + prop(subpr).
fb_readall__2(Dp, Dst, Fp) :-
	N = ~fread(@Dp, 1, ~'$trust_typed'(@Dst.size, intmach) - ~'$trust_typed'(@Dst.len, intmach), Fp),
	( N > 0 ->
	    Dp <- ~fb_need(Dst, ~fb_minread),
	    Dst.len <- ~'$trust_typed'(~'$trust_typed'(@Dst.len, intmach) + N, size),
	    fb_readall__2(Dp, Dst, Fp)
	; true
	).

% Substitute pattern p with replacement r, copying from src to dst buffer.
% TODO: const is missing in prototype: static size_t fb_subst(fbuf_t *dst, fbuf_t *src, const char *p, const char *r)
% TODO: use other notation for ref1(fbuf): ref1&fbuf (a 1-link reference to a fbuf)
:- pred fb_subst/5 + lowentryfun([ref1(fbuf), ref1(fbuf), constcstring, constcstring], size, 'fb_subst') + prop(foreign__static).
fb_subst(Dst, Src, P, R, Len) :-
	M = ~array(ref0(mut(intmach)), 3),
	Re_e = ~newmut(constcstring), % TODO: it was 'const char *re_e', add const!
	Re_eo = ~newmut(intmach), % unused output value of pcre_compile
	Re = ~pcre_compile(P, ~pcre_caseless, Re_e, Re_eo, ~'$ccons'('NULL', mut(char))),
	( Re == ~'$ccons'('NULL', ref1(pcre)) ->
	    exit(1)
	; true
	),
	Re_ex = ~pcre_study(Re, 0, Re_e),
	%
	Dst.len <- ~'$ccons'(0, size), Rlen = ~strlen(~'$trust_typed'(R, cstring)), Pos = ~initmut(intmach, 0),
	fb_subst__2(Re, Re_ex, Dst, Src, R, Rlen, Pos, M),
        Clen = ~'$trust_typed'(@Src.len, intmach) - @Pos,
	Dp = ~fb_need(Dst, ~'$trust_typed'(Clen, size)),
	Dst.len <- ~'$trust_typed'(~'$trust_typed'(@Dst.len, intmach) + Clen, size),
	memcpy(Dp, ~'$trust_typed'(~'$mut_move'(@Src.buf, @Pos), cptr), Clen),
	Len = @Dst.len.

:- pred fb_subst__2/8 + prop(subpr).
fb_subst__2(Re, Re_ex, Dst, Src, R, Rlen, Pos, M) :-
	Val = ~pcre_exec(Re, Re_ex, @Src.buf, @Src.len, @Pos, 0, M, 3),
	( Val >= 0 ->
           Clen = @M[0] - @Pos,
           Dp = ~fb_need(Dst, ~'$trust_typed'(Clen + Rlen, size)),
           Dst.len <- ~'$trust_typed'(~'$trust_typed'(@Dst.len, intmach) + (Clen + Rlen), size),
           memcpy(Dp, ~'$trust_typed'(~'$mut_move'(@Src.buf, @Pos), cptr), Clen),
           memcpy(~'$mut_move'(Dp, Clen), ~'$trust_typed'(R, cptr), Rlen),
           Pos <- @M[1],
	   fb_subst__2(Re, Re_ex, Dst, Src, R, Rlen, Pos, M)
	; true
	).

% Count all matches with pattern p in src buffer.
% TODO: missing const, prototype was: static int fb_countmatches(fbuf_t *src, const char *p)
:- pred fb_countmatches/3 + lowentryfun([ref1(fbuf), constcstring], intmach, 'fb_countmatches') + prop(foreign__static).
fb_countmatches(Src, P, Result) :-
	M = ~array(ref0(mut(intmach)), 3),
	Re_e = ~newmut(constcstring),
	Re_eo = ~newmut(intmach),
	Re = ~pcre_compile(P, ~pcre_caseless, Re_e, Re_eo, ~'$ccons'('NULL', mut(char))),
	( Re == ~'$ccons'('NULL', ref1(pcre)) ->
	    exit(1)
	; true
	),
	Re_ex = ~pcre_study(Re, 0, Re_e),
	%
	Count = ~initmut(intmach, 0), Pos = ~initmut(intmach, 0),
	fb_countmatches__2(Re, Re_ex, Src, Pos, M, Count),
	Result = @Count.

:- pred fb_countmatches__2/6 + prop(subpr).
fb_countmatches__2(Re, Re_ex, Src, Pos, M, Count) :-
	Val = ~pcre_exec(Re, Re_ex, @Src.buf, @Src.len, @Pos, 0, M, 3),
	( Val >= 0 ->
	    Count <- @Count + 1,
	    Pos <- @M[1],
	    fb_countmatches__2(Re, Re_ex, Src, Pos, M, Count)
	; true
	).

:- globalvar(variants/1) + lowentry(ref0(array(ref1(constcstring))), 'variants') + prop(foreign__static).
variants__(T) :-
	T = ~'$array_elems'(~'$array'(ref1(constcstring), [
	  ~'$cstring'("agggtaaa|tttaccct"), ~'$cstring'("[cgt]gggtaaa|tttaccc[acg]"),
	  ~'$cstring'("a[act]ggtaaa|tttacc[agt]t"), ~'$cstring'("ag[act]gtaaa|tttac[agt]ct"),
	  ~'$cstring'("agg[act]taaa|ttta[agt]cct"), ~'$cstring'("aggg[acg]aaa|ttt[cgt]ccct"),
	  ~'$cstring'("agggt[cgt]aa|tt[acg]accct"), ~'$cstring'("agggta[cgt]a|t[acg]taccct"),
	  ~'$cstring'("agggtaa[cgt]|[acg]ttaccct"), ~'$ccons'('NULL', mut(char))
        ])).

:- globalvar(subst/1) + lowentry(ref0(array(ref1(constcstring))), 'subst') + prop(foreign__static).
subst__(T) :-
	T = ~'$array_elems'(~'$array'(ref1(constcstring), [
          ~'$cstring'("B"), ~'$cstring'("(c|g|t)"), ~'$cstring'("D"), ~'$cstring'("(a|g|t)"), ~'$cstring'("H"), ~'$cstring'("(a|c|t)"), ~'$cstring'("K"), ~'$cstring'("(g|t)"),
          ~'$cstring'("M"), ~'$cstring'("(a|c)"),   ~'$cstring'("N"), ~'$cstring'("(a|c|g|t)"), ~'$cstring'("R"), ~'$cstring'("(a|g)"),   ~'$cstring'("S"), ~'$cstring'("(c|g)"),
          ~'$cstring'("V"), ~'$cstring'("(a|c|g)"), ~'$cstring'("W"), ~'$cstring'("(a|t)"),     ~'$cstring'("Y"), ~'$cstring'("(c|t)"),   ~'$ccons'('NULL', mut(char))
        ])).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	Seq = ~array(ref0(fbuf), 2),
	fb_init(~'$to_ref1'(Seq[0])),
	fb_init(~'$to_ref1'(Seq[1])),
	% TODO: from mut(fbuf) to ref1(fbuf)??
	Ilen = ~fb_readall(~'$to_ref1'(Seq[0]), ~stdin),
	Clen = ~fb_subst(~'$to_ref1'(Seq[1]), ~'$to_ref1'(Seq[0]), ~'$trust_typed'(~'$cstring'(">.*|\n"), constcstring), ~'$trust_typed'(~'$cstring'(""), constcstring)),
	%
	% TODO: this trust type should not be necessary
	% TODO: mut(mut(char)) in this case should be ref0(string)
	PP = ~initmut(ref1(array(ref0(mut(constcstring)))), ~'$trust_typed'(~variants, ref1(array(ref0(mut(constcstring)))))), % TODO: missing 'const', it was: const char **pp;
	'$while'(@PP[0] \== ~'$ccons'('NULL', constcstring), (
          printf3("%s %d\n", @PP[0], ~fb_countmatches(~'$to_ref1'(Seq[1]), @PP[0])),
	  PP <- ~'$subarray'(@PP, 1)
	)),							  
	%
	Slen = ~initmut(size, ~'$ccons'(0, size)),
	Flip = ~initmut(intmach, 1),
	% TODO: this trust type should not be necessary
	PP <- ~'$trust_typed'(~subst, ref1(array(ref0(mut(constcstring))))),
	'$while'(@PP[0] \== ~'$ccons'('NULL', constcstring), (
          Slen <- ~fb_subst(~'$to_ref1'(Seq[1 - @Flip]), ~'$to_ref1'(Seq[@Flip]), @PP[0], @PP[1]),
	  PP <- ~'$subarray'(@PP, 2),
	  Flip <- 1 - @Flip
	)),
	printf4("\n%zu\n%zu\n%zu\n", Ilen, Clen, @Slen).

:- '$improlog_end'.
