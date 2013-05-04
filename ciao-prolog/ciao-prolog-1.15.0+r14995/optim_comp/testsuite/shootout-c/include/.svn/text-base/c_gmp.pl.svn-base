% interface to gmp.h

%:- '$native_weak_inline'(include('gmp.h')).
:- '$improlog_begin'.
:- lowinclude_foreign(predef_h, 'gmp.h').

:- foreigntype(mpz, 'mpz_t').
% TODO: use better notation, define polymorphic +/3, etc. (at the source code, do not use built-in tables)
%   However, mpz_t is a mutable. So A <- @B + @C is mpz_add(A, B, C)
% TODO: mpz_t is a mutable indeed!!
%:- pred mpz_add/3 + foreign(det, [mut(mpz), mut(mpz), mut(mpz)]).
:- pred mpz_add/3 + foreign([mpz,mpz,mpz], det, 'mpz_add').
%:- pred mpz_init/1 + foreign(det, [mut(mpz)]).
:- pred mpz_init/1 + foreign([mpz], det, 'mpz_init').
%:- pred mpz_init_set_ui/2 + foreign(det, [mut(mpz), uintmach]).
:- pred mpz_init_set_ui/2 + foreign([mpz,uintmach], det, 'mpz_init_set_ui').
%:- pred mpz_mul_si/3 + foreign(det, [mut(mpz), mut(mpz), intmach]).
:- pred mpz_mul_si/3 + foreign([mpz,mpz,intmach], det, 'mpz_mul_si').
%:- pred mpz_mul_ui/3 + foreign(det, [mut(mpz), mut(mpz), uintmach]).
:- pred mpz_mul_ui/3 + foreign([mpz,mpz,uintmach], det, 'mpz_mul_ui').
%:- pred mpz_tdiv_q/3 + foreign(det, [mut(mpz), mut(mpz), mut(mpz)]).
:- pred mpz_tdiv_q/3 + foreign([mpz,mpz,mpz], det, 'mpz_tdiv_q').
%:- pred mpz_get_ui/2 + foreignfun(uintmach, mpz_get_ui).
:- pred mpz_get_ui/2 + foreignfun([ref0(mpz)], uintmach, 'mpz_get_ui').

:- '$improlog_end'.

