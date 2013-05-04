:- module(gecode_rt,
        [
	    gecode_print/0,
	    '$.=.'/2,
	    '$.>.'/2,
	    '$.<.'/2,
	    '$.=<.'/2,
	    '$.>=.'/2,
	    '$.<>.'/2,
	    '$add'/3,
	    '$sub'/3,
	    '$mul'/3,
 	    '$in'/2, 
 	    glb/2,
 	    lub/2,
 	    pitm/2,
 	    bounds/3,
  	    retrieve_list_of_values/2,
 	    labeling/1
	], 
	[assertions,basicmodes,regtypes,foreign_interface]).

 %% :- extra_compiler_opts(['-DDEBUG_ALL -DDEBUG_GMORE_GENERAL -DDEBUG_ADD_CONSTRAINT_ANSWER -DDEBUG_DIJKSTRA -DDEBUG_MALLOC']).
:- extra_compiler_opts(['-I/usr/local/include']).
:- extra_linker_opts(['-lstdc++ -lgecodedriver -lgecodesearch -lgecodeminimodel -lgecodeint']). 
:- use_module(library(odd)).
:- use_module(engine(attributes)).

:- use_module(library(write),[print/1]).
:- use_module(library(lists), [length/2]).

'$.=.'(X, Y) :-
	X = Y.

'$.<.'(X, Y) :-
	var(X), var(Y), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	get_attribute(Y,'$gecode_id'(Var2,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_less_variable(Var1,Var2).

'$.<.'(X, C) :-
	var(X), number(C), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_less_integer(Var1,C).

'$.<.'(C, Y) :-
	number(C), var(Y), !,
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_less_integer(Var1,C).

'$.<.'(C1, C2) :-
	number(C1), number(C2),
	C1 < C2.

'$.=<.'(X, Y) :-
	var(X), var(Y), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	get_attribute(Y,'$gecode_id'(Var2,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_less_equal_variable(Var1,Var2).

'$.=<.'(X, C) :-
	var(X), number(C), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_less_equal_integer(Var1,C).

'$.=<.'(C, Y) :-
	number(C), var(Y), !,
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_less_equal_integer(Var1,C).

'$.=<.'(C1, C2) :-
	number(C1), number(C2),
	C1 =< C2.

'$.>.'(X, Y) :-
	var(X), var(Y), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	get_attribute(Y,'$gecode_id'(Var2,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_greater_variable(Var1,Var2).

'$.>.'(X, C) :-
	var(X), number(C), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_greater_integer(Var1,C).

'$.>.'(C, Y) :-
	number(C), var(Y), !,
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_greater_integer(Var1,C).

'$.>.'(C1, C2) :-
	number(C1), number(C2),
	C1 > C2.

'$.>=.'(X, Y) :-
	var(X), var(Y), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	get_attribute(Y,'$gecode_id'(Var2,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_greater_equal_variable(Var1,Var2).

'$.>=.'(X, C) :-
	var(X), number(C), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_greater_equal_integer(Var1,C).

'$.>=.'(C, Y) :-
	number(C), var(Y), !,
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_greater_equal_integer(Var1,C).

'$.>=.'(C1, C2) :-
	number(C1), number(C2),
	C1 >= C2.

'$.<>.'(X, Y) :-
	var(X), var(Y), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	get_attribute(Y,'$gecode_id'(Var2,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_different_variable(Var1,Var2).

'$.<>.'(X, C) :-
	var(X), number(C), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_different_integer(Var1,C).

'$.<>.'(C, Y) :-
	number(C), var(Y), !,
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_different_integer(Var1,C).

'$.<>.'(C1, C2) :-
	number(C1), number(C2),
	\+ C1 == C2.

'$add'(X, Y, Z) :-
	var(X), var(Y), var(Z), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	get_attribute(Y,'$gecode_id'(Var2,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_add_variable(Var1,Var2,Res),
	attach_attribute(Z,'$gecode_id'(Res,Z)).
	
'$add'(Y, C, X) :-
	(number(C), var(Y)), !,
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_add_integer(Var1,C,Res),
	attach_attribute(X,'$gecode_id'(Res,X)).

'$add'(C, Y, X) :-
	(number(C), var(Y)), !, 
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_add_integer(Var1,C,Res),
	attach_attribute(X,'$gecode_id'(Res,X)).

'$add'(C1, C2, X) :-
	X is C1 + C2.

'$sub'(X, Y, Z) :-
	var(X), var(Y), var(Z),!,
	get_attribute(X,'$gecode_id'(Var1,_)),
	get_attribute(Y,'$gecode_id'(Var2,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_sub_variable(Var1,Var2,Res),
	attach_attribute(Z,'$gecode_id'(Res,Z)).

'$sub'(Y, C, X) :-
	(number(C), var(Y)), !, 
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_sub_integer(Var1,C,Res),
	attach_attribute(X,'$gecode_id'(Res,X)).

'$sub'(C, Y, X) :-
	(number(C), var(Y)), !, 
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_sub_integer(Var1,C,Res),
	attach_attribute(X,'$gecode_id'(Res,X)).

'$sub'(C1, C2, X) :-
	X is C1 - C2.

'$mul'(Y, Z, X):-
	var(X), var(Y), var(Z), !,
	get_attribute(X,'$gecode_id'(Var1,_)),
	get_attribute(Y,'$gecode_id'(Var2,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_mul_variable(Var1,Var2,Res),
	attach_attribute(Z,'$gecode_id'(Res,Z)).

'$mul'(Y, C, X) :-
	(number(C), var(Y)), !, 
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_mul_integer(Var1,C,Res),
	attach_attribute(X,'$gecode_id'(Res,X)).

'$mul'(C, Y, X) :-
	(number(C), var(Y)), !, 
	get_attribute(Y,'$gecode_id'(Var1,_)),
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_mul_integer(Var1,C,Res),
	attach_attribute(X,'$gecode_id'(Res,X)).

'$mul'(C1, C2, C):-
	number(C1), 
	number(C2), !,
	C is C1 * C2.

'$in'(V, _) :- %%Bounds could be severals ranges
	get_attribute(V,_), !,
	display('ERROR: Repeated domain definition of var'), nl,
	fail.

'$in'(V, [[LB|UB]]) :- %%Bounds could be severals ranges
	gecode_get_space(PrevSpace,LastCP),
	undo(gecode_backtracking_space(PrevSpace,LastCP)),
	gecode_new_variable(LB,UB,Id),
	attach_attribute(V,'$gecode_id'(Id,V)).

glb(V,LB) :-
	get_attribute(V,'$gecode_id'(Id)),
	gecode_get_lb(Id,LB).

lub(V,UB) :-
	get_attribute(V,'$gecode_id'(Id)),
	gecode_get_ub(Id,UB).
	
bounds(V,LB,UB) :-
	get_attribute(V,'$gecode_id'(Id)),
	gecode_get_lb(Id,LB),
	gecode_get_ub(Id,UB).

pitm(V,P_ITM) :-
	bounds(V,LB,UB),
	Paux is (LB + UB)/2,
	P_ITM is integer(Paux).

retrieve_list_of_values(V,Value) :-
	get_attribute(V,'$gecode_id'(Id)),
	gecode_get_values(Id,Values),
	member(Value,Values).

labeling(V) :-
	labeling_aux(V,Ids),
	gecode_labeling(Ids).

labeling_aux([],[]).
labeling_aux([V|RV],[Id|RID]) :-
	get_attribute(V,'$gecode_id'(Id)),
	labeling_aux(RV,RID).

:- regtype dummy(T) # "@var{T} is the zero integer.".
dummy(0).

:- true pred initial + foreign_low(initial_cc)
	# "Creates an initial gecode space.".

:- initialization(initial).

:- true pred gecode_print + foreign_low(gecode_print_cc)
	# "Prints gecode space.".

:- true pred gecode_print_variable(+Var) :: dummy + foreign_low(gecode_print_variable_cc)
	# "Prints a constraint variable.".

:- true pred gecode_get_space(-Var, -Var) :: dummy * dummy + foreign_low(gecode_get_space_cc)
	# "Gets the current GeCode state.".

:- true pred gecode_backtracking_space(-Var, -Var) :: dummy * dummy + foreign_low(gecode_backtracking_space_cc)
	# "Backtracks over a previous GeCode state.".

:- true pred gecode_equal_integer(+Var, +Int) :: dummy * dummy + foreign_low(gecode_equal_integer_cc)
	# "Assigns an integer to a constraint variable.".

:- true pred gecode_equal_variable(+Var, +Var) :: dummy * dummy + foreign_low(gecode_equal_variable_cc)
	# "Assignation between constraint variables.".

:- true pred gecode_less_integer(+Var, +Int) :: dummy * dummy + foreign_low(gecode_less_integer_cc)
	# "Constrains a variable to be less than an integer.".

:- true pred gecode_less_variable(+Var, +Var) :: dummy * dummy + foreign_low(gecode_less_variable_cc)
	# "Constrains a variable to be less than another variable".

:- true pred gecode_less_equal_integer(+Var, +Int) :: dummy * dummy + foreign_low(gecode_less_equal_integer_cc)
	# "Constrains a variable to be less or equal than an integer.".

:- true pred gecode_less_equal_variable(+Var, +Var) :: dummy * dummy + foreign_low(gecode_less_equal_variable_cc)
	# "Constrains a variable to be less or equal than another variable".

:- true pred gecode_greater_integer(+Var, +Int) :: dummy * dummy + foreign_low(gecode_greater_integer_cc)
	# "Constrains a variable to be greater than an integer.".

:- true pred gecode_greater_variable(+Var, +Var) :: dummy * dummy + foreign_low(gecode_greater_variable_cc)
	# "Constrains a variable to be greater than another variable".

:- true pred gecode_greater_equal_integer(+Var, +Int) :: dummy * dummy + foreign_low(gecode_greater_equal_integer_cc)
	# "Constrains a variable to be greater or equal than an integer.".

:- true pred gecode_greater_equal_variable(+Var, +Var) :: dummy * dummy + foreign_low(gecode_greater_equal_variable_cc)
	# "Constrains a variable to be greater or equal than another variable".

:- true pred gecode_different_integer(+Var, +Int) :: dummy * dummy + foreign_low(gecode_different_integer_cc)
	# "Constrains a variable to be different than an integer.".

:- true pred gecode_different_variable(+Var, +Var) :: dummy * dummy + foreign_low(gecode_different_variable_cc)
	# "Constrains a variable to be different than another variable".

:- true pred gecode_add_integer(+Var, +Int, -Res) :: dummy * dummy * dummy + foreign_low(gecode_add_integer_cc)
	# "Adds an integer to a variable.".

:- true pred gecode_add_variable(+Var, +Var, -Res) :: dummy * dummy * dummy + foreign_low(gecode_add_variable_cc)
	# "Adds a variable to another variable.".

:- true pred gecode_sub_integer(+Var, +Int, -Res) :: dummy * dummy * dummy + foreign_low(gecode_sub_integer_cc)
	# "Substracts an integer from a variable.".

:- true pred gecode_sub_variable(+Var, +Var, -Res) :: dummy * dummy * dummy + foreign_low(gecode_sub_variable_cc)
	# "Substracts a variable from another variable.".

:- true pred gecode_mul_integer(+Var, +Int, -Res) :: dummy * dummy * dummy + foreign_low(gecode_mul_integer_cc)
	# "Multiplies an integer and a variable.".

:- true pred gecode_mul_variable(+Var, +Var, -Res) :: dummy * dummy * dummy + foreign_low(gecode_mul_variable_cc)
	# "Multiplies two variables.".

:- true pred gecode_new_variable(+LB, +UB,-Id) :: dummy * dummy * dummy + foreign_low(gecode_new_variable_cc)
	# "Creates a new constraint variable.".

:- true pred gecode_get_lb(+Var, -LB) :: dummy * dummy + foreign_low(gecode_get_lb_cc)
	# "Gets the lower bound of a constraint variable.".

:- true pred gecode_get_ub(+Var, -UB) :: dummy * dummy + foreign_low(gecode_get_ub_cc)
	# "Gets the upper bound of a constraint variable.".

:- true pred gecode_get_values(+Var, -Values) :: dummy * dummy + foreign_low(gecode_get_values_cc)
	# "Returns all the possible values of a constraint variable.".

:- true pred gecode_labeling(+Var) + foreign_low(gecode_labeling_cc)
	# "Returns all the possible values of a constraint variable.".

:- use_foreign_source(['gecode.cc']).

