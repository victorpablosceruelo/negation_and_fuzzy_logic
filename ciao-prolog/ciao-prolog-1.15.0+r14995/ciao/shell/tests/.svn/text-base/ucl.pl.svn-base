:- use_module(call_list).

:- meta_predicate test(list(goal)).

test(L) :- call_list([write(hello)|L]).

test :- test([nl]).
