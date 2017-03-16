:- module(_, [], [assertions, nativeprops]).

:- doc(title, "Default resources.").

:- doc(author, "Edison Mera").

:- doc(module, "This module contains the implementation of all
	predefined resources, and the predicates that support it.").

:- reexport(res_wamcode(res_wamcode_res)).
:- reexport(res_nargs(res_nargs_res)).
:- reexport(res_giunif(res_giunif_res)).
:- reexport(res_gounif(res_gounif_res)).
:- reexport(res_viunif(res_viunif_res)).
:- reexport(res_vounif(res_vounif_res)).
:- reexport(res_arith(res_arith_res)).
:- reexport(res_arith(res_arith_each_res)).

