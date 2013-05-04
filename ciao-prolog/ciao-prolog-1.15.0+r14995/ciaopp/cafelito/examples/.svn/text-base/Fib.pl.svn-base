:- module( _fib, [fib/2], [assertions , nativeprops] ).


:- entry fib_1(N,Fib_1)
         : ( int(N), var(Fib_1) ).

:- trust success fib_1(N,Fib_1)
        => ( int(N), int(Fib_1) ).

fib_1(N_0,Fib_1_0) :-
        N_0=0,
        Fib_1_0 is 0 .
fib_1(N_0,Fib_1_0) :-
        N_0\==0,
        N_0=1,
        Fib_1_0 is 1 .
fib_1(N_0,Fib_1_0) :-
        N_0\==0,
        N_0\==1,
        Fib_1_aux_l_fib_1_0 is N_0-1,
        fib(Fib_1_aux_l_fib_1_0,Fib_1_aux_l_fib_0),
        Fib_1_aux_r_fib_1_0 is N_0-2,
        fib(Fib_1_aux_r_fib_1_0,Fib_1_aux_r_fib_0),
        Fib_1_aux_0 is Fib_1_aux_l_fib_0+Fib_1_aux_r_fib_0,
        Fib_1_0 is Fib_1_aux_0 .

:- entry fib(N,Fib)
         : ( int(N), var(Fib) ).

:- trust success fib(N,Fib)
        => ( int(N), int(Fib) ).

fib(N_0,Fib_0) :-
        fib_1(N_0,Fib_aux_fib_1_0),
        Fib_0 is Fib_aux_fib_1_0 .

