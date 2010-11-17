:- use_module(dist,[dist/2]).

:- meta_predicate stored_pred(goal,?). 


:- load_compilation_module(.('pack_tr')).

:- add_sentence_trans(dpred/3).

