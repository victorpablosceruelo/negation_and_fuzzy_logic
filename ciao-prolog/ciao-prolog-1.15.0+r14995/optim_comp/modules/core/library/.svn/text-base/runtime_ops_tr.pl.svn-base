:- module(runtime_ops_tr, [runtime_op/2], [pure, assertions]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).

runtime_op((:- Op), RuntimeOp) :-
        Op = op(_,_,_),
        RuntimeOp = [(:- Op), (:- initialization(Op))].
