%:- include(library('clpr/clpr')).
:- include(library('rfuzzy/rfops')).
:- use_module(library('rfuzzy/rfaggr')).
:- reexport(library('rfuzzy/rfaggr')).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms),[copy_args/3]).

:- load_compilation_module(library('rfuzzy/rfuzzy_tr')).
:- add_sentence_trans(trans_fuzzy_sent/3).
:- add_clause_trans(trans_fuzzy_cl/3).

:- aggr min.
:- aggr luka.
:- aggr prod.
:- aggr max.
:- aggr dluka.
:- aggr dprod.
:- aggr iprod.
:- aggr complement.


:- new_declaration(is_fuzzy/3,on).