q1 :- findall((FLAT_NUMBER,[V]),not_very_expensive(FLAT_NUMBER, V),Ans),show(Ans).
q2 :- findall((FLAT_NUMBER,[V]),something_very_cheap(FLAT_NUMBER, V),Ans),show(Ans).
main :- q1,nl,q2,nl.
