q1 :- findall((HOUSE_CODE,[V]),not_very_expensive(HOUSE_CODE, V),Ans),show(Ans).
q2 :- findall((HOUSE_CODE,[V]),not_very_small(HOUSE_CODE, V),Ans),show(Ans).
main :- q1,nl,q2,nl.
