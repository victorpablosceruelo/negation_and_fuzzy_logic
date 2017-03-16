% Query List 
not_very_expensive(FLAT_NUMBER, V) :- expensive(FLAT_NUMBER, V1), very_func(V1, V2), not_func(V2, V).
something_very_cheap(FLAT_NUMBER, V) :- cheap(FLAT_NUMBER, V1), very_func(V1, V2), something_func(V2, V).
