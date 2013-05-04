:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).

:- '$native_weak_inline'(include('engine/fannkuch.native.h')).
:- '$native_weak_inline'(include('malloc.h')).

:- '$improlog_begin'.

:- pred fannkuch/2 + lowentryfun([intmach], intmach, 'fannkuch') + prop(foreign__static).
fannkuch(N, Result) :- N < 1, !, Result = 0.
fannkuch(N, Result) :-
	N1 = ~initmut(intmach, N - 1),
	%
	Perm = ~'$alloc'(malloc, array(ref0(mut(intmach)), N)),
	Perm1 = ~'$alloc'(malloc, array(ref0(mut(intmach)), N)),
	Count = ~'$alloc'(malloc, array(ref0(mut(intmach)), N)),
	%
	% initial (trivial) permu
	'$for_each'(I, ~intrange(N), Perm1[@I] <- @I),
        %
	R = ~initmut(intmach, N), Printed = ~initmut(intmach, 0), FlipsMax = ~initmut(intmach, 0),
	Result0 = ~newmut(intmach),
	label0(Printed, Perm, Perm1, Count, R, N, N1, FlipsMax, Result0),
	Result = @Result0.

:- pred xch/2 + prop(unfold) + prop(propargs).
xch(X,Y) :- Temp = @X, X <- @Y, Y <- Temp.

:- pred label0/9 + prop(subpr).
label0(Printed, Perm, Perm1, Count, R, N, N1, FlipsMax, Result) :-
	% print 30 first permutations
	( @Printed < 30 ->
	    '$for_each'(I1, ~intrange2(0, N), printf2("%d", 1 + @Perm1[@I1])),
	    printf1("\n"),
	    Printed <- @Printed + 1
	; true
	),
	'$while'(@R \== 0, (
	  Count[@R - 1] <- @R,
	  R <- @R - 1
	)),
	%
	( \+ ( @Perm1[0] == 0 -> true % TODO: allow disjunction here (because of cut)
	     ; @Perm1[@N1] == @N1
	     ) ->
	    Flips = ~initmut(intmach, 0),
	    '$for_each'(I0, ~intrange2(1, N), ( % perm = perm1
		Perm[@I0] <- @Perm1[@I0]
	    )),
	    K = ~initmut(intmach, @Perm1[0]), % cache perm[0] in k
	    '$do_while'(( % k!=0 implies k>0
		I = ~initmut(intmach, 1),
		J = ~initmut(intmach, @K - 1),
		'$while'(@I < @J, (
		    xch(Perm[@I], Perm[@J]),
		    I <- @I + 1,
		    J <- @J - 1
		)),
		Flips <- @Flips + 1,
		% Now exchange k (caching perm[0]) and perm[k]... with care!
		% XCH(k, perm[k]) does NOT work because index K is also exchanged
		% TODO: fix unfold! do not copy the expressions (except when required)
		J <- @Perm[@K], Perm[@K] <- @K, K <- @J
	    ), (@K \== 0)),
	    ( @FlipsMax < @Flips ->
	        FlipsMax <- @Flips
	    ; true
	    )
	; true
	),
	label1(Printed, Perm, Perm1, Count, R, N, N1, FlipsMax, Result).

:- pred label1/9 + prop(subpr).
label1(Printed, Perm, Perm1, Count, R, N, N1, FlipsMax, Result) :-
	( @R == N ->
	    Result <- @FlipsMax % end loop
	; % rotate down perm[0..r] by one
	  Perm0 = @Perm1[0],
	  I = ~initmut(intmach, 0),
	  '$while'(@I < @R, (
	    K = ~initmut(intmach, @I + 1),
	    Perm1[@I] <- @Perm1[@K],
	    I <- @K
	  )),
	  Perm1[@R] <- Perm0,
	  %
	  Count[@R] <- @Count[@R] - 1,
	  ( @Count[@R] > 0 ->
	      label0(Printed, Perm, Perm1, Count, R, N, N1, FlipsMax, Result)
	  ; R <- @R + 1,
	    label1(Printed, Perm, Perm1, Count, R, N, N1, FlipsMax, Result)
          )
	).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	fannkuch(N, R),
	printf3("Pfannkuchen(%d) = %d\n", N, R).

:- '$improlog_end'.
