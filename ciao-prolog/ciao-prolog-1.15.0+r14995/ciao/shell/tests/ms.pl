:- use_package([]).
:- use_module(engine(messages)).

main :- message_lns(error, 12, 13, ['Bad predicate specifier ',~(3/4),' in new_declaration directive']).
