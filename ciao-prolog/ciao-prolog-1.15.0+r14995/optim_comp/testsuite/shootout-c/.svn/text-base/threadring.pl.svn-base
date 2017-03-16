% Note: GHC benchmarks that use concurrency may be faster than C
% versions because they use a STM (software transitional memory)
% concurrency control (see
% http://en.wikipedia.org/wiki/Software_transactional_memory)
% --Jose

:- module(_, [], ['$purest']).
:- include(.(include(common))).
:- include(.(include(c_stdio))).
:- include(.(include(c_stdlib))).
:- include(.(include(c_pthread))).
:- include(.(include(c_string))).

:- '$native_weak_inline'(include('engine/threadring.native.h')).

:- '$improlog_begin'.

:- foreign_include_in_header('limits.h').
:- foreign_include_in_header('pthread.h').

:- pred threads/1 + lowentrymacrocons(intmach, 'THREADS').
threads := 503.

:- lowtype(stack).
:- class stack {
  :- struct.
  % TODO: use real type constructors for arrays, evaluate size!!
  % TODO: IN THIS CASE, the size is externally defined and not known
  % at compile time!! (by ImProlog)
  :- attr x :: array(ref0(mut(char)), 'PTHREAD_STACK_MIN').
}.

:- globalvar(mutex/1) + lowentry(ref0(array(ref0(mut(pthread_mutex)))), 'mutex') + prop(foreign__static).
% TODO: use real type constructors for arrays, evaluate size!!
mutex__(T) :- T = ~'$uninit'(ref0(array(ref0(mut(pthread_mutex)), 'THREADS'))).

:- globalvar(data/1) + lowentry(ref0(array(ref0(mut(intmach)))), 'data') + prop(foreign__static).
% TODO: use real type constructors for arrays, evaluate size!!
data__(T) :- T = ~'$uninit'(ref0(array(ref0(mut(intmach)), 'THREADS'))).

:- globalvar(stacks/1) + lowentry(ref0(array(ref0(mut(stack)))), 'stacks') + prop(foreign__static).
% TODO: use real type constructors for arrays, evaluate size!!
stacks__(T) :- T = ~'$uninit'(ref0(array(ref0(mut(stack)), 'THREADS'))).

% TODO: note in the original C code:
% stacks must be defined staticaly, or my i386 box run of virtual memory for this
% process while creating thread +- #400
:- pred thread/2 + lowentryfun([mut(char)], mut(char), 'thread') + prop(foreign__static).
thread(Num0, Result) :-
	L = ~'$cast'(Num0, intmach),
	R = (L + 1) mod ~threads,
	thread_loop(L, R, Result).

:- pred thread_loop/3 + prop(subpr).
thread_loop(L, R, Result) :-
	pthread_mutex_lock((~mutex)[L]),
	Token = @((~data)[L]),
	( Token \== 0 ->
	    (~data)[R] <- Token - 1,
	    pthread_mutex_unlock((~mutex)[R]),
	    thread_loop(L, R, Result)
	; printf2("%i\n", L+1),
	  % TODO: it was a call to C exit function: exit(0);
	  exit(0),
          Result = ~'$ccons'('NULL', mut(char))
        ).

:- pred begin/1 + lowentry(det, [intmach], 'begin').
begin(N) :-
	Cthread = ~newmut(pthread),
	StackAttr = ~newmut(pthread_attr),

	(~data)[0] <- N,

	pthread_attr_init(StackAttr),
      
        '$for_each'(I, ~intrange(~threads), (
	  pthread_mutex_init((~mutex)[@I], ~'$ccons'('NULL', mut(char))),
	  pthread_mutex_lock((~mutex)[@I]),
	  %
          pthread_attr_setstack(StackAttr, (~stacks)[@I], ~'$ccons'('sizeof(struct stack)', intmach)),
	  pthread_create(Cthread, StackAttr, ~'$ccons'('(void *(*)(void *)) thread', mut(char)), ~'$cast'(@I, mut(char)))
	)),

	pthread_mutex_unlock((~mutex)[0]),
	pthread_join(@Cthread, ~'$ccons'('NULL', mut(mut(char)))).

:- '$improlog_end'.
