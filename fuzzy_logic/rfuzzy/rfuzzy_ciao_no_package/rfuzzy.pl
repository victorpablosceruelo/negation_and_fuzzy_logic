%:- include(library('clpr/clpr')).
:- include(library('rfuzzy/rfops')).
:- use_module(library('rfuzzy/rfaggr')).
:- load_compilation_module(library('rfuzzy/rfuzzy_tr')).
:- add_sentence_trans(fuzzy_pred/3).
:- add_clause_trans(fuzzy_pred2/3).

:- aggr min.
:- aggr luka.
:- aggr prod.
:- aggr max.
:- aggr dluka.
:- aggr dprod.
:- aggr iprod.
:- aggr complement.


:- new_declaration(is_fuzzy/3,on).