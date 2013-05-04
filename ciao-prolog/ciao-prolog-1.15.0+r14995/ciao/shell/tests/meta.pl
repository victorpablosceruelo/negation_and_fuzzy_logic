:- module(meta, [meta_assign/2], []).

:- use_module(library(lists)).

:- meta_predicate meta_assign(?,fact).

meta_assign(X, X).
