:- package(dcg).

:- load_compilation_module(library(dcg_expansion)).
:- add_sentence_trans(dcg_translation/2, 310).
:- op(1200, xfx,[(-->)]).
