:- module( _test_rem_use_cls, [p/2], [] ).


p(a,b).
p(a,g(A)) :-
        produce2_1(A),
        to_filter_1(g(A)).
p(f(B),A) :-
        produce1_1(B),
        to_filter_2(f(B)),
        produce2_1(A),
        to_filter_2(A).

produce2_1(b).
produce2_1(g(A)) :-
        produce2_1(A).

to_filter_1(g(a)).
to_filter_1(g(b)).
to_filter_1(g(c)).
to_filter_1(g(d)).
to_filter_1(g(f(A))) :-
        to_filter(A).
to_filter_1(g(g(A))) :-
        to_filter_2(A).

to_filter_2(a).
to_filter_2(b).
to_filter_2(c).
to_filter_2(d).
to_filter_2(f(A)) :-
        to_filter_2(A).
to_filter_2(g(A)) :-
        to_filter(A).

produce1_1(a).
produce1_1(f(A)) :-
        produce1_1(A).



