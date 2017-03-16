:- module(_,_,[assertions,regtypes]).


:- regtype class/1
# "".

class(Class):-
   Class = class(Loc,Id,Methods),
   locator(Loc),
   identifier(Id),
   list(Methods,method).


:- regtype locator/1
# "".

locator((L1,L2)):-
  !,
  line(L1),
  line(L2).
locator(L):-
  !,
  line(L).


:- regtype line/1
# "".
line(L):-
  nnegint(L),!.
line(na).


:- regtype modifier/1
# "".

modifier(public).
modifier(private).


:- regtype method/1
# "".

method(Method):-
   Method = method(Loc,Mod,Var,Params,Asserts,Block),
   locator(Loc),
   modifier(Mod),
   variable(Var),
   list(Params,variable),
   list(Asserts,assertion),
   block(Block).


:- regtype assertion/1
# "".
assertion(Assert):-
   Assert = (Loc,Status,Type,Call,Succ,Comp,Dict),	
   locator(Loc),
   status(Status),
   type(Type),
   term(Call),
   term(Succ),
   term(Comp),
   varnames(Dict).


:- regtype method/1
# "".

status(check).
status(trust).



:- regtype type/1
# "".

type(entry).
type(calls).
type(success).
type(pred).
type(comp).

:- regtype varnames/1
# "".

varnames(Dict):-
   list(Dict,atom_var_pair).	



:- regtype atom_var_pair/1
# "".

atom_var_pair(Pair):-
   Pair = '='(Atom,Var),
   atom(Atom),
   var(Var).


:- regtype block/1
# "".

block(Block):-
   Block = block(Loc,Block_Stats),
   locator(Loc),
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
   block(Stat),!.
statement(Stat):-
   if(Stat),!.
statement(Stat):-
   while(Stat),!.
statement(Stat):-
   return(Stat),!.
statement(Stat):-
   expression(Stat),!.	
statement(skip).



:- regtype if/1
# "".

if(If):-
   If = if((Loc1,Loc2),Expr,Stat1,Stat2),
   locator(Loc1),
   locator(Loc2),
   expression(Expr),
   statement(Stat1),
   statement(Stat2).


:- regtype while/1
# "".

while(While):-
   While = while(Loc,Expr,Stat),
   locator(Loc),
   expression(Expr),
   statement(Stat).


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
   variable(Var),
   expression(Expr).


:- regtype assign_operator/1
# "".

assign_operator('=').
assign_operator('+').
assign_operator('-').
assign_operator('*').
assign_operator('/').
assign_operator('%').


:- regtype binary_operator/1
# "".

binary_operator('+').
binary_operator('-').
binary_operator('/').
binary_operator('*').
binary_operator('%').
binary_operator('==').
binary_operator('!=').
binary_operator('<=').
binary_operator('>=').
binary_operator('<').
binary_operator('>').


:- regtype unary_operator/1
# "".

unary_operator('!').


:- regtype type_/1
# "".

type_(byte).
type_(short).
type_(int).
type_(long).
type_(float).
type_(double).
type_(void).

:- regtype variable/1
# "".

variable(Var):-
   Var = var(Type,Id),
   type_(Type),!,
   identifier(Id).
variable(Var):-
   Var = var(Type,Id),
   var(Type),!,
   identifier(Id).


:- regtype identifier/1
# "".

identifier(Id):-
  atom(Id).


	
:- regtype literal/1
# "".

literal(Lit):-
   Lit = literal(Type,Num),
   type_(Type),
   num(Num).

