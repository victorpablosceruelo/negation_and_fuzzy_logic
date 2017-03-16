:- module(fast_unif,[unif/2,unif_int_var/2]).

:- use_package(assertions).

:- trust pred unif(A, B) : ground * var
   + equiv(unif_int_var(A,B)).

%:- trust pred unif(A, B) + equiv(A = B).

:- trust pred unif(A, B) + native(A = B).
:- impl_defined(unif/2).
:- impl_defined(unif_int_var/2).

