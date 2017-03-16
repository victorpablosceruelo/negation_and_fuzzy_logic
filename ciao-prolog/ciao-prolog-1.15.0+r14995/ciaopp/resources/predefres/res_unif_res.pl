:- module(_, _, [assertions]).

term_unif_size_(Head, Modes, CountingMode, AddVar, AddGround, Size0, Size) :-
	Head =.. [_FunctorName|Args],
	term_unif_size_list_(Args, Modes, CountingMode, AddVar, AddGround,
	    Size0, Size).

term_unif_size_list_([],         [],           _Mode, _AddVar, _AddGround,
	    Size,  Size) :- !.
term_unif_size_list_([Arg|Args], [Mode|Modes], Mode,  AddVar,  AddGround,
	    Size0, Size) :-
	!,
	term_size_(Arg, AddVar, AddGround, Size0, Size1),
	term_unif_size_list_(Args, Modes, Mode, AddVar, AddGround, Size1,
	    Size).
term_unif_size_list_([_Arg|Args], [_Mode|Modes], Mode, AddVar, AddGround,
	    Size0, Size) :-
	term_unif_size_list_(Args, Modes, Mode, AddVar, AddGround, Size0,
	    Size).

term_size_(Head, AddVar, _AddGround, Size0, Size) :-
	var(Head),
	!,
	Size is Size0 + AddVar.
term_size_(Head, AddVar, AddGround, Size0, Size) :-
	Head =.. [_FunctorName|Args],
	Size1 is Size0 + AddGround,
	term_size_list_(Args, AddVar, AddGround, Size1, Size).

term_size_list_([],     _,      _,         Size,  Size) :- !.
term_size_list_([A|As], AddVar, AddGround, Size0, Size) :-
	!,
	term_size_(A, AddVar, AddGround, Size0, Size1),
	term_size_list_(As, AddVar, AddGround, Size1, Size).
