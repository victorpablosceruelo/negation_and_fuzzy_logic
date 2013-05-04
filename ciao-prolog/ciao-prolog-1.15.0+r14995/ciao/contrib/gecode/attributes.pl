:- use_module(library(write),[write/1]).

:- multifile verify_attribute/2.
:- multifile combine_attributes/2.
:- multifile portray_attribute/2.


verify_attribute('$gecode_id'(Var,_), Int) :-
	integer(Int),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	Var .=. Int.

combine_attributes('$gecode_id'(Var1,_), '$gecode_id'(Var2,_)) :-
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	Var1 .=. Var2.

portray_attribute('$gecode_id'(Id,_),Var) :-
	write(Var), display(' in '),
	gecode_print_variable(Id).

