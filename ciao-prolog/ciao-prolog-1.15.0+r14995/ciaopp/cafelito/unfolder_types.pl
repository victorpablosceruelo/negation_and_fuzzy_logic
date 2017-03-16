:- module(_,_,[assertions,regtypes]).

:- reexport(flattener_types,[
	                    locator/1,
			    assertion/1,
			    assign_operator/1,
			    binary_operator/1,
			    unary_operator/1,
			    variable/1,
			    type_/1,
	                    identifier/1,
			    literal/1,

			    modifiers/1

	                 ]).

:- regtype module/1
# "".

module(Mod) :-
   Mod = module(Loc,Id,Path,Preds),
   locator(Loc),
   atom(Id),
   sourcename(Path),
   list(Preds,predicate).


:- regtype predicate/1
# "".

predicate(Pred):-
   Pred = pred(Loc,Mods,Var,(Pars1,Pars2),Asserts,Clauses),
   locator(Loc),
   modifiers(Mods),
   variable(Var),
   list(Pars1,variable),
   list(Pars2,variable),
   list(Asserts,assertion),
   list(Clauses,clause).



:- regtype clause/1
# "".

clause(Cls):-
   Cls = clause(Loc,Var_Ver,(Pars1,Pars2),Block_Stats),
   locator(Loc),
   variable_version(Var_Ver),
   list(Pars1,variable_version),
   list(Pars2,variable_version),
   list(Block_Stats,statement).


:- regtype statement/1
# "".

statement(Stat):-
   local_declaration(Stat),!.
statement(Stat):-
   method_call(Stat),!.	
statement(Stat):-
   assignment(Stat),!.	
statement(Stat):-
   binary_operation(Stat),!.
statement(skip):-!.
statement(cut):-!.


:- regtype local_declaration/1
# "".
 
local_declaration(Local_Decl):-
   Local_Decl = declaration(Loc,Var_Ver),
   locator(Loc),
   variable_version(Var_Ver).



:- regtype method_call/1
# "".

method_call(Call):-
   Call = call(Loc,Var_Ver,Params),
   locator(Loc),
   variable_version(Var_Ver),
   Params = (R,W_Input,W_Output),
   list(R,variable_literal),
   list(W_Input,variable_literal),
   list(W_Output,variable_literal).


assignment(Assign):-
   Assign = assignment(Loc,Var_Ver,Var_Lit),
   variable_literal(Var_Lit),!,
   locator(Loc),
   variable_version(Var_Ver).
assignment(Assign):-
   Assign = assignment(Loc,Var_Ver,Bin_Op),
   binary_operation(Bin_Op),!,
   locator(Loc),
   variable_version(Var_Ver).
assignment(Assign):-
   Assign = assignment(Loc,Var_Ver,Call),
   method_call(Call),!,
   locator(Loc),
   variable_version(Var_Ver).


:- regtype binary_operation/1
# "".

binary_operation(Bin_Op):-
   Bin_Op = binary_op(Bin_Opr,Expr1,Expr2),
   binary_operator(Bin_Opr),
   variable_literal(Expr1),
   variable_literal(Expr2).


:- regtype unary_operation/1
# "".

unary_operation(Un_Op):-
   Un_Op = unary_op(Un_Opr,Expr),
   unary_operator(Un_Opr),
   variable_literal(Expr).


:- regtype variable_literal/1
# "".

variable_literal(Var_Lit):-
   variable_version(Var_Lit),!.	
variable_literal(Var_Lit):-
   literal(Var_Lit).


:- regtype variable_version/1
# "".

variable_version(Var_Ver):-
   Var_Ver = var(Type,Id,Ver),
   variable(var(Type,Id)),
   nnegint(Ver).

   