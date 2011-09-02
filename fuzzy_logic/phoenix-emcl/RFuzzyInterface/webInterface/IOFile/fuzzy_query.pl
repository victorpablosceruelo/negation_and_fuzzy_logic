% Query List 
not_very_expensive(HOUSE_CODE, V) :- expensive(HOUSE_CODE, V1), very_func(V1, V2), not_func(V2, V).
not_very_small(HOUSE_CODE, V) :- small(HOUSE_CODE, V1), very_func(V1, V2), not_func(V2, V).
