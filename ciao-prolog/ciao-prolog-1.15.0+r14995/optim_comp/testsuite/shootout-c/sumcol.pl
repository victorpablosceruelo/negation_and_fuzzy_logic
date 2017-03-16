:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).

:- '$native_weak_inline'(include('engine/sumcol.native.h')).
:- '$native_weak_inline'(include('malloc.h')).

:- '$improlog_begin'.

:- pred maxlinelen/1 + lowentrymacrocons(intmach, 'MAXLINELEN').
maxlinelen := 128.

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(_) :-
	% TODO: size is static, allocate without alloca, it was 'char line[MAXLINELEN]'
	% TODO: do not cast to mut(char)!
	Line = ~'$trust_typed'(~'$alloc'(alloca, array(ref0(mut(ref0(char))), ~maxlinelen)), mut(char)),
        %
	Sum = ~initmut(intmach, 0),
	sum_lines(Line, Sum),
	printf2("%d\n", @Sum).

:- pred sum_lines/2 + prop(subpr).
sum_lines(Line, Sum) :-
	Ret = ~fgets(Line, ~maxlinelen, ~stdin),
	( Ret \== ~'$ccons'('NULL', mut(char)) ->
	    Sum <- @Sum + ~atoi(Line),
	    sum_lines(Line, Sum)
	; true
	).

:- '$improlog_end'.

