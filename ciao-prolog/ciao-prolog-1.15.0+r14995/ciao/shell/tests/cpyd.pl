:- use_module(m1).
:- use_module(m2).

put_m1_data(D) :- assertz_fact(data(D)).

get_m2_data(D) :- current_fact(m2:data(D)).

copy_data(Data) :-
        retract_fact(m1:Data),
        assertz_fact(m2:Data),
        fail.
copy_data(_).
