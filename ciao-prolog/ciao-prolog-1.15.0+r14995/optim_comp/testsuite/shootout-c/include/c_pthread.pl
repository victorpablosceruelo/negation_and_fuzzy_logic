% interface to pthread.h

%:- '$native_weak_inline'(include('pthread.h')).
:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'pthread.h').

:- foreigntype(pthread, 'pthread_t').

:- pred pthread_stack_min/1 + foreigncons(intmach, 'PTHREAD_STACK_MIN').

:- foreigntype(pthread_mutex, 'pthread_mutex_t').

:- foreigntype(pthread, 'pthread_t').
:- foreigntype(pthread_attr, 'pthread_attr_t').

:- pred pthread_attr_init/1 + foreign([mut(pthread_attr)], det, 'pthread_attr_init').
:- pred pthread_mutex_init/2 + foreign([mut(pthread_mutex),mut(char)], det, 'pthread_mutex_init').
:- pred pthread_mutex_lock/1 + foreign([mut(pthread_mutex)], det, 'pthread_mutex_lock').
:- pred pthread_mutex_unlock/1 + foreign([mut(pthread_mutex)], det, 'pthread_mutex_unlock').
:- pred pthread_attr_setstack/3 + foreign([mut(pthread_attr),mut(stack),intmach], det, 'pthread_attr_setstack').
:- pred pthread_create/4 + foreign([mut(pthread),mut(pthread_attr),mut(char),mut(char)], det, 'pthread_create').
% TODO: it is extern int pthread_join (pthread_t __th, void **__thread_return);
:- pred pthread_join/2 + foreign([pthread,mut(mut(char))], det, 'pthread_join').

:- '$improlog_end'.

