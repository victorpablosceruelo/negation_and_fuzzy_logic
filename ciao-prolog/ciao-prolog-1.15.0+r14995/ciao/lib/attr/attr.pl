:- package(attr).

:- use_module(library(attr(attr_rt)), [get_attr_local/2, put_attr_local/2, del_attr_local/1]).

:- load_compilation_module(library(attr(attr_tr))).
:- add_sentence_trans(attr_tr:sentence/3, 910).


