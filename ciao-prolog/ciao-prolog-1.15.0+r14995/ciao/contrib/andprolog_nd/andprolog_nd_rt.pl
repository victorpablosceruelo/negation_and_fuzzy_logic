:- module(andprolog_nd_rt, _, []).


:- reexport(library(assertions(native_props)), [indep/1, indep/2]).

:- reexport(library(andprolog_nd(det_rt_nd)),                          _).
:- reexport(library(andprolog_nd(nondet_move_top_rt_nd)),              _).
:- reexport(library(andprolog_nd(callh_rt_nd)),                        _).
:- reexport(library(andprolog_nd(agents_rt_nd)),                       _).

