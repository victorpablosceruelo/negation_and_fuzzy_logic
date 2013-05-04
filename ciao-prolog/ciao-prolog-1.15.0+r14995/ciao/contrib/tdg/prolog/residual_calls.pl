:- module(residual_calls,[res_call/1],assertions).

%:- use_module(prolog_interpreter,[main/4]).

:- trust comp res_call/1 + (memo,bind_ins,sideff(free)).
%res_call(main(A,B,C,D)) :- main(A,B,C,D).
res_call(main(_,_,_,_)).


