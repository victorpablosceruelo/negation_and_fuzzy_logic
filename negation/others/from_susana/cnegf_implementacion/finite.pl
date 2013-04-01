:- use_module(pplib(database), [db_get/1,trust_complexity/9]).

finite_solutions(F/A):-
	functor(Head0,F,A),
	db_get(trust_nonfail(Head0, _InTypes, _OuTypes, FailInfo, _CoverInfo)),
        trust_complexity(F/A, _Mode, _Measure, _Mutex, 
                        _Solution_Det, _Size, _Relation, Time, _Domain),
        FailInfo == not_fails,
        Time \== inf.



