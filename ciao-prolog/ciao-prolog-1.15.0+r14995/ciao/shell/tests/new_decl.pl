:- use_package([]).

extract_pred(Decl, F, A) :-
        new_declaration(Decl, P, G),
        call(G),
        (P = F/A ; functor(P,F,A)),
        !.

new_declaration(pred(_,Y), P, (\+ assop(Y), Y=P ; arg(1,Y,P))).

assop(:=(_, _)).
assop(::(_, _)).
assop(=>(_, _)).
assop(++(_, _)).
assop(--(_, _)).
