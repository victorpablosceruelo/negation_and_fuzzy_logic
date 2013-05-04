:- module(_,_,[assertions,regtypes]).

:- reexport(parser_types,[
	                    locator/1,
			    assertion/1,
			    assign_operator/1,
			    binary_operator/1,
			    unary_operator/1,
			    variable/1,
			    type_/1,
	                    identifier/1,
			    literal/1
	                 ]).

:- regtype module/1
# "A module is a module".

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

:- regtype modifiers/1
# "".

modifiers(Mod):-
   Mod = (Access,Iterative,Real),
   access(Access),
   iterative(Iterative),
   real(Real).


:- regtype access/1
# "".

access(entry).
access(non_entry).


:- regtype iterative/1
# "".

iterative(iterative).
iterative(simple).

:- regtype iterative/1
# "".

real(real).
real(virtual).



:- regtype clause/1
# "".

clause(Cls):-
   Cls = clause(Loc,Var,(Pars1,Pars2),Block_Stats),
   locator(Loc),
   variable(Var),
   list(Pars1,variable),
   list(Pars2,variable),
   list(Block_Stats,block_statement).
   

:- regtype block_statement/1
# "".

block_statement(Block_Stat):-
   local_declaration(Block_Stat),!.	
block_statement(Block_Stat):-
   statement(Block_Stat).


:- regtype local_declaration/1
# "".
 
local_declaration(Local_Decl):-
   Local_Decl = declaration(Loc,Var,Expr),
   locator(Loc),
   variable(Var),
   expression(Expr).


:- regtype statement/1
# "".

statement(Stat):-
   return(Stat),!.
statement(Stat):-
   expression(Stat),!.	
statement(skip):-!.
statement(cut) :-!.


:- regtype return/1
# "".

return(Return):-
   Return = return(Loc,Expr),
   locator(Loc),
   expression(Expr),!.
return(Return):-
   Return = return(Loc,void),
   locator(Loc).


:- regtype expression/1
# "".

expression(Expr):-
   method_call(Expr),!.
expression(Expr):-
  variable(Expr) ,!.
expression(Expr):-
  literal(Expr) ,!.
expression(Expr):-
   binary_operation(Expr),!.
expression(Expr):-
   unary_operation(Expr),!.
expression(Expr):-
   assignment(Expr),!.


:- regtype method_call/1
# "".

method_call(Call):-
   Call = call(Loc,Var,(Input,Output)),
   locator(Loc),
   variable(Var),
   list(Input,variable),!,
   list(Output,variable).
method_call(Call):-
   Call = call(Loc,Var,Exprs),
   locator(Loc),
   variable(Var),
   list(Exprs,expression).



:- regtype binary_operation/1
# "".

binary_operation(Bin_Op):-
   Bin_Op = binary_op(Bin_Opr,Expr1,Expr2),
   binary_operator(Bin_Opr),
   expression(Expr1),
   expression(Expr2).


:- regtype unary_operation/1
# "".

unary_operation(Un_Op):-
   Un_Op = unary_op(Un_Opr,Expr),
   unary_operator(Un_Opr),
   expression(Expr).


:- regtype assignment/1
# "".

assignment(Assign):-
   Assign = assignment(Loc,Assig_Op,Var,Expr),
   locator(Loc),
   assign_operator(Assig_Op),
   (Var = var(Type,return(Name)) ->
      variable(var(Type,Name));
      variable(Var)
   ),
   expression(Expr).
