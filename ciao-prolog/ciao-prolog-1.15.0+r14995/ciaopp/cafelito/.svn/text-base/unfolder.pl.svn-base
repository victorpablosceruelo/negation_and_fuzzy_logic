%:- module(_,[ast_unfold/2],[fsyntax,hiord,assertions]).
:- module(_,_,[fsyntax,hiord,assertions]).

:- use_module(library(messages)).
:- use_module(library(lists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).

:- reexport(  cafelito(flattener),[parse/2,ast_flatten/2]).
:- use_module(cafelito(unfolder_types)).

:- op(200,xfy,['--']).

:- op(200,xfy,[::]).
:- fun_eval :: /2.
   A :: B := ~append(A,B):-!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Testing predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ast_unfold(Java_File,New_AST):-
    ast_flatten(Java_File,Flattened_AST),!,
    optional_message("Doing expression unfolding... ",[]),
    (unfold_class(Flattened_AST,New_AST) ->
        statistics(runtime,[_,T1]),
        optional_message("done in ~3D msec.",[T1],[]),
	debug_message("Unfolded tree:"),
	debug_message("~w",[New_AST]);
	
        throw(unfolding_bug)
    ).
ast_unfold(_,_):-
   throw(flattening_bug).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AST transformations, step 2
%
% -Expressions are unfolded (they are no longer recursive)
%  preserving left-to rigth eval
%
%- This top-down process generates auxiliar variables of
%  types that are build bottom-up and then checked against
%  declarations and parameters
% 
% -Every compound assigment is transformed to a simple version
%  The equivalence is semantically correct but introduces overhead
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unfold_class(Module,New_Module):-
   Module = module(Line,Id,File,Methods),!,
   dictionary_init(Methods,Dict),
   unfold_preds(Methods,Dict,New_Methods),
   New_Module = module(Line,Id,File,New_Methods),
   (compat(New_Module,unfolder_types:module) ->
        true;
	throw(internal_type_error(expected(unfolder_types:module)))       
   ).



unfold_preds([],_,[]).
unfold_preds([M|RM],Dict,New_Module_Body):-
   unfold_pred(M,Dict,New_M),
   unfold_preds(RM,Dict,New_RM),
   New_Module_Body = [New_M | New_RM].

unfold_pred(Pred,Dict,New_Pred):-
  Pred     = pred(L,M,Pred_Def,(R,W),Asserts,Cls),
  unfold_clauses(Cls,Dict,New_Cls),
  generate_pred_sig(Pred_Def,(R,W),Sign),
  New_Pred = pred(L,M,Pred_Def,Sign,Asserts,New_Cls).

unfold_clauses([],_,[]).
unfold_clauses([C|RC],Dict,New_Pred_Body):-
   unfold_clause(C,Dict,New_C),
   unfold_clauses(RC,Dict,New_RC),
   New_Pred_Body = [New_C | New_RC].

unfold_clause(AST,Dict,New_AST):-
   AST      = clause(Line,Var,Pars,Statements),!,
   Var      = var(Type,Name),
   Pred_Var = var(Type,Name,0), 
   Pars     = (R,W),
   dictionary_put_all(R::W,Dict,Dict1),
   add_version(Pars,Version_Pars),
   unfold_statements(Statements,Dict1,Pred_Var,New_Statements,New_Dict),
   % magic: unification of return parameters with last versions
   Version_Pars = (Read,Write_Input,Write_Output),
   sublist(Write_Output,New_Dict),
   Arguments = (Read::Write_Input,Write_Output),
   New_AST   = clause(Line,Pred_Var,Arguments,New_Statements).

add_version_el(var(T,N),var(T,N,0)) :-!.
add_version([],[]).
add_version([Var|RVar],[Var_Ver|RVar_Ver]):-
   add_version_el(Var,Var_Ver),
   add_version(RVar,RVar_Ver).
add_version((R,W),(R_V,W_V,WF_V)):-
   add_version(R,R_V),
   add_version(W,W_V),
   add_variable_version(W,WF_V).

add_variable_version_el(var(T,N),var(T,N,_X)) :-!.
add_variable_version([],[]).
add_variable_version([Var|RVar],[Var_Ver|RVar_Ver]):-
   add_variable_version_el(Var,Var_Ver),
   add_variable_version(RVar,RVar_Ver).


unfold_statements([],D,_,[],D).
unfold_statements([S|RS],Dict,My_Method,New_Block_Body,New_Dict):-
   unfold_statement( S, Dict     ,My_Method,New_S ,Dict1),
   unfold_statements(RS,Dict1    ,My_Method,New_RS,New_Dict),
   New_Block_Body = New_S :: New_RS.

% declaration
unfold_statement(AST,Dict,_,New_AST,New_Dict):-
   AST      = declaration(Line,Variable,Expr),!,
   Variable = var(Type,Name),
   add_version_el(Variable,Variable_Vers),
   atom_concat(Name,'_aux',Prefix),
   % use the OLD dictionary
   unfold_expr(Expr,Dict,Prefix,New_Expr--Last,Dict1),
   type_compatible(Type,Last),
   dictionary_put(var(Type,Name),Dict1,New_Dict),
   New_Declaration = declaration(Line,Variable_Vers),
   New_Assignment  = assignment(Line,Variable_Vers,Last),
   New_AST = New_Expr :: [New_Declaration,New_Assignment].

% return, both versions (??)
unfold_statement(AST,Dict,_,New_AST,Dict):-
   AST     = return(_,void),!,
   New_AST = [AST].
unfold_statement(AST,Dict,My_Method,New_AST,New_Dict):-
   AST     = return(Line,Expr),!,
   Prefix  = '_return',
   unfold_expr(Expr,Dict,Prefix,New_Expr--Last,New_Dict),
   dictionary_get(var(Signature,My_Method,_),Dict),
   Signature = [Return_Type|_],
   type_compatible(Return_Type,Last),
   New_Return = return(Line,Last),
   New_AST = New_Expr :: [New_Return].

%%%%%%%%
%% FIXME 
%% THIS CALL HAS TO BE VOID ????
unfold_statement(AST,Dict,_,New_AST,New_Dict):-
   AST = call(_,Var,_),!,
   Var = var(_,Name),
   unfold_expr(AST,Dict,Name,New_AST--_,New_Dict).

unfold_statement(AST,Dict,_,New_AST,New_Dict):-
   AST = assignment(_,_,Var,_),
   Var = var(_,Name),
   unfold_expr(AST,Dict,Name,New_AST--_,New_Dict).

unfold_statement(skip,Dict,_,[skip],Dict):-!.

unfold_statement(cut,Dict,_, [cut],Dict):-!.

unfold_statement(AST,_,_,AST,_):-
   AST = var(_,_),!,	
   throw(malformed_adt1(AST)).	
unfold_statement(AST,_,_,AST,_):-
   AST = literal(_,_),!,	
   throw(malformed_adt1(AST)).	

% FIXME 
% kludge
% this is here just because of the absence of boolean types
unfold_statement(AST,Dict,_,New_AST,Dict):-
   AST     = binary_op(Op,_,_),	
   member(Op,['==','!=','<=','>=','<','>']),!,
   unfold_expr(AST,Dict,'cond',Expr_AST--_,Dict),
   append(Tmp,[assignment(_,_,Just_Bin_Op)],Expr_AST),
   append(Tmp,[Just_Bin_Op],New_AST).

unfold_statement(AST,Dict,_,New_AST,Dict):-
   AST     = binary_op(_,_,_),!,	
   unfold_expr(AST,Dict,'',Correct_AST--_,Dict),
   Correct_AST = [Decl,assignment(_,_,Just_Bin_Op)],
   New_AST = [Decl,Just_Bin_Op].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%               unfolding & type-checking of EXPRESSIONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% method calls, case 1: calls existing in the original program
unfold_expr(AST,Dict,Prefix,New_AST,New_Dict):-
   AST           = call(Line,Variable,Exprs),
   list(Exprs),!,
   Variable      = var(Return_Type,Method_Name),
   Method_Var    = var(Return_Type,Method_Name,0),
   atom_concat([Prefix,'_',Method_Name],Method_Prefix),
   Return_Var    = var(Return_Type,Method_Prefix,0),
   unfold_call_parameters(Exprs,Dict,Method_Name,Method_Prefix,
                          New_Exprs--Lasts,New_Dict),
   New_Call      = call(Line,Method_Var,(Lasts,[],[])),
   dictionary_get(var(Signature,Method_Name,_),Dict),
   Signature = [Return_Type|_],
   (Return_Type \== void ->
      New_Decl      = declaration(na,Return_Var), 
      New_Assign    = assignment(Line,Return_Var,New_Call),
      New_AST = (New_Exprs::[New_Decl,New_Assign])--Return_Var;

      New_AST = (New_Exprs::[New_Call])--New_Call
   ).
% calls, case 2: calls introduced by the flatten step 
% The difference is the existence of Input/Output variables
% no need to unfold: just retrieve old version and add one for set 'W'
unfold_expr(AST,Dict,Prefix,New_AST,New_Dict):-
   AST           = call(Line,Variable,Exprs),
   Exprs         = (Read,Write),
   Variable      = var(Return_Type,Method_Name),
   atom_concat([Prefix,'_',Method_Name],Method_Prefix),
   Method_Var    = var(Return_Type,Method_Name,0),
   Return_Var    = var(Return_Type,Method_Prefix,0),
   dictionary_add_version_all(Read,Dict,Read_Vers),
   dictionary_add_version_all(Write,Dict,Write_Vers),
   dictionary_increase_version_all(Write_Vers,Dict,New_Dict),
   dictionary_get_last_version_all(Write_Vers,New_Dict,Write_Vers_Plus_1),
   Params  = (Read_Vers,Write_Vers,Write_Vers_Plus_1),
   New_Call      = call(Line,Method_Var,Params),
   dictionary_get(var(Signature,Method_Name,_),Dict),
   Signature = [Return_Type|_],
   (Return_Type \== void ->
      New_Decl      = declaration(na,Return_Var), 
      New_Assign    = assignment(Line,Return_Var,New_Call),
      New_AST = ([New_Decl,New_Assign])--Return_Var;

      New_AST = [New_Call]--New_Call
   ).
   
% variables
unfold_expr(AST,Dict,_,New_AST,Dict):-
   AST = var(Type,Name),!,
   dictionary_get(var(Type,Name,Version),Dict),
   Version_Variable = var(Type,Name,Version),
   New_AST = []--Version_Variable.
% literals
unfold_expr(AST,Dict,_,New_AST,Dict):-
   AST     = literal(_,_),!,
   New_AST = []--AST.
% binary operations
unfold_expr(AST,Dict,Prefix,New_AST,New_Dict):-
   AST      = binary_op(Op,L,R),
   atom_concat([Prefix,'_l'],Prefix_L),
   atom_concat([Prefix,'_r'],Prefix_R),
   unfold_expr(L,Dict ,Prefix_L,New_L--Last_L,Dict1),
   unfold_expr(R,Dict1,Prefix_R,New_R--Last_R,New_Dict),
   type_casting(Op,Last_L,Last_R,Resulting_Type),
   New_Binary_Op = binary_op(Op,Last_L,Last_R),
   New_Variable  = var(Resulting_Type,Prefix,0), 
   % New_Decl      = declaration(na,New_Variable),
   New_Assign    = assignment(na,New_Variable,New_Binary_Op),
   New_AST = (New_L :: New_R :: [New_Assign])--New_Variable.

% assignment Expr1 = Expr2
unfold_expr(AST,Dict,_Prefix,New_AST,New_Dict):-
   AST = assignment(L,'=',Var,Expr),!,
   Var = var(Type,X),
   (X = return(Name) ->
       true;

       dictionary_get(var(Type,X,_),Dict),
       Name = X
   ),
   atom_concat([Name , '_aux'],New_Prefix),
   unfold_expr(Expr,Dict,New_Prefix,New_Expr--Last,Dict1),
   type_compatible(Type,Last),
   (X = return(Name) ->
       Dict2 = Dict1;
       dictionary_increase_version(Name,Dict1,Dict2)       
   ),
   dictionary_get(var(_,Name,Last_Version),Dict2),
   Var_Last_Version = var(Type,Name,Last_Version),
   New_Assignment = assignment(L,Var_Last_Version,Last),
   New_AST        = (New_Expr :: [New_Assignment])--Var_Last_Version,
   New_Dict = Dict2.
% assignment Expr1 op= Expr2
unfold_expr(AST,Dict,Prefix,New_AST,New_Dict):-
   AST        = assignment(L,Op,Var,R),!,
   Binary_Op  = binary_op(Op,Var,R),
   New_Assignment   = assignment(L,'=',Var,Binary_Op),
   unfold_expr(New_Assignment,Dict,Prefix,New_AST,New_Dict).


unfold_call_parameters(AST,Dict,Method_Name,Prefix,New_AST,New_Dict):-
   dictionary_get(var(Sig,Method_Name,_),Dict),
   Sig = [_Return_Type|Params_Types],
   unfold_call_parameters_(AST,Params_Types,Dict,Prefix,1,New_AST,New_Dict).

unfold_call_parameters_([],[],Dict,_,_,[]--[],Dict).
unfold_call_parameters_([Expr|R],[Param|R_Param],
	                Dict,Prefix,Number,New_AST,New_Dict):-
   atom_number(No,Number),
   atom_concat([Prefix,'_',No],New_Prefix),
   unfold_expr(Expr,Dict,New_Prefix,New_Expr--Last,Dict1),
   type_compatible(Param,Last),
   Number1 is Number + 1,
   unfold_call_parameters_(R,R_Param,Dict1,Prefix,Number1,New_R--Lasts,New_Dict),
   New_AST = (New_Expr :: New_R)--[Last|Lasts].


generate_output_names([],[]).
generate_output_names([V|RV],[NV|RNV]):-
   V  = var(Type,Name),
   atom_concat([Name,'_F'],New_Name),
   NV = var(Type,New_Name),
   generate_output_names(RV,RNV).

generate_pred_sig(Pred_Def,(R,W),Sig):-
   generate_output_names(W,WO),
   Pred_Def = var(Type,_),
   (Type = void ->
       Sig = (R::W,WO);
       Sig = (R::W,WO:: [Pred_Def])
   ).


type_casting(_,_,_,int).

% true if rhs is included in lhs
type_compatible(var(T1,_,_),var(T2,_,_)):-
   !,type_compatible(T1,T2).	
type_compatible(var(T1,_,_),literal(T2,_)):-
   !,type_compatible(T1,T2).	
type_compatible(literal(T1,_),var(T2,_,_)):-
   !,type_compatible(T1,T2).
type_compatible(literal(T1,_),literal(T2,_)):-
   !,type_compatible(T1,T2).
type_compatible(var(T1,_,_),T2):-
   !,type_compatible(T1,T2).	
type_compatible(T1,var(T2,_,_)):-
   !,type_compatible(T1,T2).	
type_compatible(literal(T1,_),T2):-
   !,type_compatible(T1,T2).	
type_compatible(T1,literal(T2,_)):-
   !,type_compatible(T1,T2).	

type_compatible(X,X)         :-!.
type_compatible(X,Y):-
   type_compatible_(X,C),
   type_compatible(C,Y).

type_compatible_(double,float):-!.
type_compatible_(float,long)  :-!.
type_compatible_(long,int)    :-!.
type_compatible_(int,short)   :-!.
type_compatible_(short,byte)  :-!.

% a Dict is just a list of (Id,Type,Version) for typing all variables
% it's initialized with the global variables: methods
dictionary_init(Methods,Dict):-
   dictionary_init_(Methods,[],Dict).	

dictionary_init_([],D,D).
dictionary_init_([P|RP],Dict,DictF):-
   P = pred(_,_,Variable,(R,W),_,_),
   Variable = var(Return_Type,Name_Method),
   get_types(R::W,Params_Types),
   Signature = [Return_Type|Params_Types],
   dictionary_put(var(Signature,Name_Method),Dict,New_Dict),
   dictionary_init_(RP,New_Dict,DictF).

get_types([],[]).
get_types([var(T,_)|R],[T|NR]):-
	get_types(R,NR).

dictionary_put(var(Type,Id,Version),Dict,New_Dict):-
    !,	
    (member(var(_,Id,_),Dict) ->
       New_Dict = Dict;
       New_Dict = [var(Type,Id,Version)|Dict]
    ).
dictionary_put(var(Type,Id),Dict,New_Dict):-
    dictionary_put(var(Type,Id,0),Dict,New_Dict).

dictionary_put_all([],Dict,Dict).
dictionary_put_all([P|RP],Dict,DictF):-
   dictionary_put(P,Dict,New_Dict),
   dictionary_put_all(RP,New_Dict,DictF).

dictionary_remove(Id,Dict,New_Dict):-
   delete_non_ground(Dict,var(_,Id,_),New_Dict).	

dictionary_get(Var,Dict):-
   member(Var,Dict),!.
dictionary_get(var(Type,Id,_),_):-
   var(Type),!,	
   throw(inexistent_key_in_dict(Id)).
dictionary_get(var(Type,Id,Version),Dict):-
   dictionary_get(var(Actual_Type,Id,Version),Dict),
   throw(incompatible_type(Id,found(Actual_Type),required(Type))).

% FIXME
% this is inconsistent with the previous declaration
dictionary_get_last_version_all([],_,[]).
dictionary_get_last_version_all([P|RP],Dict,[VP|RVP]):-
   P = var(Type,Id,_),	
   dictionary_get(var(Type,Id,Version),Dict),
   VP = var(Type,Id,Version),
   dictionary_get_last_version_all(RP,Dict,RVP).

dictionary_add_version_all([],_,[]).
dictionary_add_version_all([P|RP],Dict,[VP|RVP]):-
   P = var(Type,Id),	
   dictionary_get(var(Type,Id,Version),Dict),
   VP = var(Type,Id,Version),
   dictionary_add_version_all(RP,Dict,RVP).

dictionary_increase_version(Id,Dict,New_Dict):-
   dictionary_get(var(Type,Id,Version),Dict),
   Version1 is Version + 1,
   dictionary_remove(Id,Dict,Dict1),
   dictionary_put(var(Type,Id,Version1),Dict1,New_Dict).

dictionary_increase_version_all([],Dict,Dict).
dictionary_increase_version_all([P|RP],Dict,New_Dict):-
   P = var(_,Id,_),	
   dictionary_increase_version(Id,Dict,Dict1),
   dictionary_increase_version_all(RP,Dict1,New_Dict).
