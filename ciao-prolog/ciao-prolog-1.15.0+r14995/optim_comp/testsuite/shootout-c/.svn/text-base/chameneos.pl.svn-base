% TODO: slower than the C version... and system time much larger... check!!
% TODO: remove exprstat from switchdef generation, so that ; appears instead of ;; in generated code like:
%  switch(var1) {
%  case 1:
%    goto lab0;;

:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_pthread))).
:- include(.(include(c_semaphore))).

% TODO: uncommenting this like gives errors about incorrect 'extern' declarations (due to the foreignvar kludge)
:- '$native_weak_inline'(include('engine/chameneos.native.h')).

:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'semaphore.h').

:- lowtype(color).
:- type(color/1) + enum.
color(T) :- T = ~'$base'(intmach), T = (blue|red|yellow|faded).
:- enum_encode(color/1, [
	(blue, 0),
	(red, 1),
	(yellow, 2),
	(faded, 3)]).

:- lowtype(status).
:- type(status/1) + enum.
status(T) :- T = ~'$base'(intmach), T = (st1|st2|st3).
:- enum_encode(status/1, [
	(st1, 1),
	(st2, 2),
	(st3, 3)]).

:- globalvar(atMostTwo/1) + lowentry(ref0(mut(ref0(sem))), 'atMostTwo').
atMostTwo__(T) :- T = ~'$uninit'(ref0(mut(ref0(sem)))).
:- globalvar(mutex/1) + lowentry(ref0(mut(ref0(sem))), 'mutex').
mutex__(T) :- T = ~'$uninit'(ref0(mut(ref0(sem)))).
:- globalvar(semPriv/1) + lowentry(ref0(mut(ref0(sem))), 'semPriv').
semPriv__(T) :- T = ~'$uninit'(ref0(mut(ref0(sem)))).

:- globalvar(mpStatus/1) + lowentry(mut(status), 'mpStatus').
mpStatus__(T) :- T <- ~'$typed'(status, st1). % TODO: wrong, this is a mutable!

:- globalvar(aColor/1) + lowentry(ref0(mut(color)), 'aColor').
aColor__(T) :- T = ~'$uninit'(ref0(mut(color))).
:- globalvar(bColor/1) + lowentry(ref0(mut(color)), 'bColor').
bColor__(T) :- T = ~'$uninit'(ref0(mut(color))).

:- globalvar(meetingsLeft/1) + lowentry(ref0(mut(intmach)), 'meetingsLeft').
meetingsLeft__(T) :- T = ~'$uninit'(ref0(mut(intmach))).
:- globalvar(reports/1) + lowentry(ref0(array(ref0(mut(intmach)))), 'reports').
reports__(T) :- T = ~'$uninit'(ref0(array(ref0(mut(intmach)), 4))).

:- pred complementaryColor/3 + lowentryfun([color, color], color, 'complementaryColor').
complementaryColor(C1, C2, R) :-
	( C2 = faded ->
	    R = faded
	; C1 == C2 ->
	    R = C1
	; % TODO: use if-then-else and a property to tell to use a switch
          ( C1 = blue -> ( C2 = red -> R = yellow ; R = red )
          ; C1 = red -> ( C2 = blue -> R = yellow ; R = blue )
          ; C1 = yellow -> ( C2 = blue -> R = red ; R = blue )
          ; R = C1
	  )
	).

:- pred cooperation/3 + lowentryfun([intmach, color], color, 'Cooperation').
cooperation(Id, C, R) :-
	OtherColor = ~newmut(color),

	sem_wait(~'$to_ref1'(~atMostTwo)),
	sem_wait(~'$to_ref1'(~mutex)),
%	R = ~'$typed'(color, red).

	% TODO: add a property to ensure that it is compiled as a C switch
	% TODO: I changed 'status' type so that it is an enumerated and can be compiled as a switch
	MpStatus = @(~mpStatus), % TODO: without this temporal variable the next code is not properly indexed
        ( MpStatus = st1 ->
	    (~aColor) <- C,
	    (~mpStatus) <- st2,
	    sem_post(~'$to_ref1'(~mutex)),
	    sem_wait(~'$to_ref1'(~semPriv)),
	    OtherColor <- @(~bColor),
	    sem_post(~'$to_ref1'(~mutex)),
	    sem_post(~'$to_ref1'(~atMostTwo)),
	    sem_post(~'$to_ref1'(~atMostTwo))
        ; MpStatus = st2 ->
	    (~meetingsLeft) <- @(~meetingsLeft) - 1,
	    ( @(~meetingsLeft) > 0 ->
	        ~mpStatus <- st1
	    ; ~mpStatus <- st3
	    ),
	    ~bColor <- C,
	    OtherColor <- @(~aColor),
	    sem_post(~'$to_ref1'(~semPriv))
        ; MpStatus = st3 ->
	    OtherColor <- ~'$typed'(color, faded),
	    sem_post(~'$to_ref1'(~mutex)),
	    sem_post(~'$to_ref1'(~atMostTwo))
	),
	R = @OtherColor.

:- pred chameneosCode/1 + lowentry(det, [mut(char)], 'chameneosCode').
chameneosCode(Args) :-
	MyId = ~newmut(intmach),
	MyColorInt = ~newmut(intmach),
	sscanf4(Args, "%d %d", MyId, MyColorInt),
	MyColor = ~initmut(color, ~'$trust_typed'(@MyColorInt, color)), % TODO: this is not trivial... it should be a call to the decoding function!
	Meetings = ~initmut(intmach, 0),
	'$while'(@MyColor \== ~'$typed'(color, faded), (
          OtherColor = ~cooperation(@MyId, @MyColor),
	  MyColor <- ~complementaryColor(@MyColor, OtherColor),
	  Meetings <- @Meetings + 1
	)),			   
	(~reports)[@MyId] <- @Meetings - 1.

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	% TODO: improve! allow direct initialization 
%	TabColor <- ~'$array_elems'(~'$array'(color, [~'$typed'(color, blue), ~'$typed'(color, red), ~'$typed'(color, yellow), ~'$typed'(color, blue)])),
%	color tabColor[4] = { Blue, Red, Yellow, Blue },
	TabColor = ~array(ref0(mut(color)), 4),
	  TabColor[0] <- blue,
	  TabColor[1] <- red,
	  TabColor[2] <- yellow,
	  TabColor[3] <- blue,
	TabPid = ~array(ref0(mut(pthread)), 4),
	TheArgs = ~array(ref0(array(ref0(mut(char)), 32)), 4),
	sem_init(~'$to_ref1'(~atMostTwo), 0, 2),
	sem_init(~'$to_ref1'(~mutex), 0, 1),
	sem_init(~'$to_ref1'(~semPriv), 0, 0),

	~meetingsLeft <- N,

	'$for_each'(I, ~intrange(4), (
          sprintf4(@((~'$trust_typed'(TheArgs, array(ref0(mut(mut(char))))))[@I]), "%d %d", @I, @TabColor[@I]),
	  % TODO: fix ccons, casting of predicate abstraction			 
          pthread_create(TabPid[@I], ~'$ccons'('NULL', mut(pthread_attr)), ~'$ccons'('(void *(*)(void *)) chameneosCode', mut(char)), @((~'$trust_typed'(TheArgs, array(ref0(mut(mut(char))))))[@I]))
	)),
	% TODO: use scope for I...
	'$for_each'(I2, ~intrange(4), (
          pthread_join(@(TabPid[@I2]), ~'$ccons'('NULL', mut(mut(char))))
        )),

	Sum = ~initmut(intmach, @((~reports)[0])),
	'$for_each'(I3, ~intrange2(1,4), (
          Sum <- @Sum + @((~reports)[@I3])
        )),
	printf2("%d\n", @Sum).

:- '$improlog_end'.
