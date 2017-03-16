% interface to semaphore.h

%:- '$native_weak_inline'(include('semaphore.h')).
:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'semaphore.h').

:- foreigntype(sem, 'sem_t').

:- pred sem_wait/1 + foreign([ref1(mut(ref0(sem)))], det, 'sem_wait').
:- pred sem_post/1 + foreign([ref1(mut(ref0(sem)))], det, 'sem_post').
:- pred sem_init/3 + foreign([ref1(mut(ref0(sem))),intmach,intmach], det, 'sem_init').

:- '$improlog_end'.

