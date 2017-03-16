%:- module(_,[ast_flatten/2],[fsyntax,hiord,assertions]).
:- module(_,_,[fsyntax,hiord,assertions]).

:- use_module(library(messages)).
:- use_module(library(lists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).

:- reexport(  cafelito(parser),[parse/2]).
:- use_module(cafelito(parser_types)).
:- use_module(cafelito(flattener_types)).


:- op(200,xfy,[++]).
:- fun_eval ++ /2.
   A ++ B := ~union(A,B).

:- op(200,xfy,['minus']).
:- fun_eval 'minus' /2.
   A minus B := ~difference(A,B).

:- op(200,xfy,[::]).
:- fun_eval :: /2.
   A :: B := ~append(A,B):-!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Testing predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ast_flatten(Java_File,Flattened_AST):-
    parse(Java_File,AST),
    optional_message("Doing flattening... ",[]),
    statistics(runtime,_),
    flatten_class(AST,Java_File,Flattened_AST),!,
    statistics(runtime,[_,T]),
    optional_message("done in ~3D msec.",[T],[]),
    debug_message("Flattened tree:",[]),
    debug_message("~w",[Flattened_AST]).
ast_flatten(_,_):-
   throw(flattening_bug).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Unfolding of block constructions (if-then-else,block) to predicates  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred flatten_class(Class, Java_File, Module) 
	: (class(Class),atm(Java_File), var(Module)) => module(Module) 
 # "Transform the parser internal representation @var{Class} in order to
 eliminate nestings of blocks, ie, @var{Module} contains only atomic statements.
 At the same time we switch from a imperative representation to a logic one (use
 of clauses and predicates instead of blocks)".

flatten_class(Class,Java_File,Module):-
   (compat(Class,parser_types:class) ->
       true;
       throw(internal_type_error(expected(parser_types:class)))
   ),
   Class = class(Line,Id,Methods),!,
   flatten_methods(Methods,New_Methods),
   Module = module(Line,Id,Java_File,New_Methods),
   (compat(Module,flattener_types:module) ->
       true;
       throw(internal_type_error(expected(flattener_types:module)))
   ).


:- pred flatten_methods(Methods,Preds)
	: (list(Methods,method),var(Preds)) => list(Preds,clause)
# "Flatten every method in @var{Methods} into his (possibly multiple) predicates
 @var{Preds}".

flatten_methods([],[]).
flatten_methods([M|RM],Preds):-
   flatten_method(M,Pred),
   flatten_methods(RM,RPred),
   Preds = Pred :: RPred.


:- pred flatten_method(Method,Pred)
	: (method(Method),var(Pred)) => clause(Pred)
# "A method @var{Method} is equivalent (by itself) to a one-clause predicate @var{Pred}".

flatten_method(Method,Pred):-
   Method = method(Line,Mods,This_Method,Pars,Asserts,Block),
   flatten_block(Block,This_Method,New_Block,Predicates,Decl,(R,W)),
   % this unifies type variables  
   _ = Pars ++ Decl ++ R ++ W,
   modifier2entry(Mods,Access),
   Modifiers = (Access,simple,real),
   Clause    = clause(Line,This_Method,(Pars,[]),New_Block),
   Predicate = pred(Line,Modifiers,This_Method,(Pars,[]),Asserts,[Clause]),
   Pred = Predicates :: [Predicate].

flatten_block(AST,My_Method,New_AST,New_Predicates,Decl,(R,W)):-
   AST = block(_,St),!, 
   flatten_statements(St,My_Method,New_St,New_Predicates,(R,W)),
   declared_vars(St,Decl),
   New_AST = New_St.

flatten_statements(St,My_Method,New_St,New_Preds,(R,W)):-
   flatten_statements_(St,My_Method,(1,_),New_St,New_Preds,(R,W),_).

flatten_statements_([],_,(N,N),[],[],([],[]),false).
flatten_statements_([S|RS],My_Method,(N,NF),[NS|NRS],New_Preds,(R,W),Ret):-
   flatten_statement(S,My_Method,(N,N1),NS,Preds_S,(R1,W1),Ret1),
   flatten_statements_(RS,My_Method,(N1,NF),NRS,Preds_RS,(R2,W2),Ret2),
   New_Preds = Preds_S :: Preds_RS,
   R   = R1 ++ R2,
   W   = W1 ++ W2,
   or(Ret1,Ret2,Ret). 

flatten_statement(AST,_,(N,N),AST,[],(R,W),false):-
   AST = declaration(_,_,Expr),!,
   referenced_vars(Expr,(R,W)).


:- push_prolog_flag(discontiguous_warnings,off).

% block generates 1 predicate
flatten_statement(AST,My_Method,(N,NF),New_AST,New_Preds,(R,W),Ret):-
   AST = block(Line,St),!, 	
   My_Method = var(Method_Type,Method_Name),
   atom_number(No,N),
   atom_concat([Method_Name,'_', No],Pred_Name),
   (Ret = true ->
      Pred_Type = Method_Type;
      Pred_Type = void
   ),
   Pred_Def = var(Pred_Type,Pred_Name),
   flatten_statements_(St,Pred_Def,(N,N1),New_St,Preds,(Read,Write),Ret),
   declared_vars(St,Local_Vars),
   NF is N1 + 1,
   R  = Read minus Write minus Local_Vars,
   W =  Write minus Local_Vars, 
   Clause    = clause(Line,Pred_Def,(R,W),New_St),
   Modifiers = (non_entry,simple,virtual),
   Predicate = pred(Line,Modifiers,Pred_Def,(R,W),[],[Clause]),
   New_Preds = Preds :: [Predicate],
   Call_Parameters = (R, W),
   New_Call = call(na,Pred_Def,Call_Parameters),
   Return_Var = var(Method_Type,return(Method_Name)),
   (Pred_Type == void ->
       New_AST = New_Call;
       New_AST = assignment(na,'=',Return_Var,New_Call)
   ).

% if-then-else generates as many clauses as nested branches 
flatten_statement(AST,My_Method,(N1,NF),New_AST,New_Preds,(R,W),Ret):-
   AST = if(L,_Expr,_St1,_St2),!,
   % if they are block we need to flatten them
   My_Method = var(Method_Type,Method_Name),
   atom_number(No,N1),
   atom_concat([Method_Name,'_', No],Pred_Name),
   Pred_Def = var(Pred_Type,Pred_Name),
   flatten_and_simplify_branches(AST,Pred_Def,(N1,N2),New_Sts,Preds,(R,W),Ret),
   (Ret = true ->
      Pred_Type = Method_Type;
      Pred_Type = void
   ),
   L  = (LI,LE),
   build_clauses(New_Sts,LI,Pred_Def,(R,W),Clauses),
   LI = (LP1,LI2),
   LE = (_,LE2),
   (LE2 = na ->
     LP2 = LI2;
     LP2 = LE2
   ),
   Modifiers    = (non_entry,iterative,virtual),
   If_Else_Pred = pred((LP1,LP2),Modifiers,Pred_Def,(R,W),[],Clauses),
   New_Preds    = Preds :: [If_Else_Pred],
   Call_Parameters = (R , W),
   Return_Var   = var(Method_Type,return(Method_Name)),
   New_Call     = call(na,Pred_Def,Call_Parameters),
   (Pred_Type == void ->
       New_AST = New_Call;
       New_AST = assignment(na,'=',Return_Var,New_Call)
   ),
   NF is N2+1.


flatten_and_simplify_branches(St,Pred_Def,(N1,N2),New_St,Preds,(R,W),Ret):-
   St = block(_,B),!,
   declared_vars(B,Local_Vars), 
   flatten_statements_(B,Pred_Def,(N1,N2),New_St_,Preds,(R_,W_),Ret),
   New_St = [New_St_],
   R = R_ minus Local_Vars,
   W = W_ minus Local_Vars.
flatten_and_simplify_branches(St,Pred_Def,(N1,NF),New_St,Preds,(R,W),Ret):-
   St = if(_Loc,Expr,St1,St2),!,
   referenced_vars(Expr,(R0,W0)),
   flatten_and_simplify_branches(St1,Pred_Def,(N1,N2),New_Sts1,Preds1,(R1,W1),Ret1),
   flatten_and_simplify_branches(St2,Pred_Def,(N2,NF),New_Sts2,Preds2,(R2,W2),Ret2),
   map_append([Expr,cut],New_Sts1,Bodies1),
   negate(Expr,Neg_Expr),
   map_append([Neg_Expr],New_Sts2,Bodies2),
   New_St = Bodies1::Bodies2,
   Preds  = Preds1 :: Preds2,
   W      =  W0 ++ W1 ++ W2 , 
   R      = (R0  ++ R1 ++ R2) minus W,
   or(Ret1,Ret2,Ret).
flatten_and_simplify_branches(St,Pred_Def,(N1,N2),New_St,Preds,(R,W),Ret):-
   declared_vars([St],Local_Vars),	
   flatten_statement( St,Pred_Def,(N1,N2),New_St_,Preds,(R_,W_),Ret),
   New_St = [[New_St_]],
   R      = R_ minus Local_Vars,
   W      = W_ minus Local_Vars.

build_clauses([],_,_,_,[]).
build_clauses([Sts|RSts],Loc,Pred_Def,(R,W),[Cls|RCls]):-
   Cls = clause(Loc,Pred_Def,(R,W),Sts),
   build_clauses(RSts,Loc,Pred_Def,(R,W),RCls).



flatten_statement(AST,My_Method,(N1,NF),New_AST,New_Preds,(R,W),Found_Ret):-
   AST   = while(L,Expr,St),	
   Rec_Call = call(na,Pred_Def,(R,W)),
   (St = block(LB,Sts) ->
       Body = block(LB,Sts :: [Rec_Call]);

       LB = L,	
       Body = block(L,[Sts,Rec_Call])
   ),
   Lines   = (LB,(na,na)),
   Equi_If = if(Lines,Expr,Body,skip),
   flatten_statement(Equi_If,My_Method,(N1,NF),New_If,New_Preds,(R,W),Found_Ret),
   % kinda crappy, but neccesary to unify Pred_Def...
   last(New_Preds,If_Else_Pred),
   If_Else_Pred = pred(_,_,Pred_Def,(R,W),[],_),
   New_AST      = New_If.

   
flatten_statement(AST,My_Method,(N,N),New_AST,[],(R,W),true):-
   AST        = return(L,Expr),
   My_Method  = var(Method_Type,Method_Name),
   Return_Var = var(Method_Type,return(Method_Name)),
   New_AST    = assignment(L,'=',Return_Var,Expr),
   referenced_vars(Expr,(R,W)).

flatten_statement(AST,_,(N,N),AST,[],([],[]),true):-
   AST = return(_,void).
flatten_statement(AST,_,(N,N),AST,[],([],[]),false):-
   AST = skip.
% expression
flatten_statement(AST,_,(N,N),AST,[],(R,W),false):-
   referenced_vars(AST,(R,W)).	


:- pop_prolog_flag(discontiguous_warnings).


% This can be done in the bottom-up process but is less clear
declared_vars(Sts,Decl):-
   declared_vars_(Sts,[],Decl).    

declared_vars_([],Decl,Decl).
declared_vars_([St|R_St],Decl,DeclF):-
   St = declaration(_,Var,_),!,
   declared_vars_(R_St,[Var|Decl],DeclF).
declared_vars_([_|R_St],Decl,DeclF):-
   declared_vars_(R_St,Decl,DeclF).

referenced_vars(AST,(R,W)):-
   AST = call(_,_,Exprs),
   map_referenced_vars(Exprs,(R,W)),!.
% for recursive calls
referenced_vars(AST,([],[])):-
   AST = call(_,_,_).
referenced_vars(AST,(R,W)):-
   AST = binary_op(_,E1,E2),!,
   referenced_vars(E1,(R1,W1)),
   referenced_vars(E2,(R2,W2)),
   R = R1 ++ R2,
   W = W1 ++ W2.
referenced_vars(AST,(R,W)):-
   AST = unary_op(_,E),!,
   referenced_vars(E,(R,W)).
referenced_vars(AST,(R,W)):-
   AST = assignment(_,_,E1,E2),!,
   referenced_vars(E2,(R2,W2)),
   R = R2,
   W = [E1] ++ W2.
referenced_vars(AST,(R,[])):-
   AST = var(_,_Id),!,
   R = [AST].
referenced_vars(AST,([],[])):-
   AST = literal(_,_).

map_referenced_vars([],([],[])).
map_referenced_vars([E|RE],(R,W)):-
   referenced_vars(E,(E_R,E_W)),
   map_referenced_vars(RE,(RE_R,RE_W)),
   R = E_R ++ RE_R,
   W = E_W ++ RE_W.
	

modifier2entry(public,entry).
modifier2entry(private,non_entry).


negate(Expr,Neg_Expr):-
   Expr = binary_op(Op,Expr1,Expr2),!,
   negate_op(Op,Neg_Op),
   negate(Expr1,Neg_Expr1),
   negate(Expr2,Neg_Expr2),
   Neg_Expr = binary_op(Neg_Op,Neg_Expr1,Neg_Expr2).
negate(Expr,Neg_Expr):-
   Expr = unary_op('!',Neg_Expr),!.	
negate(Expr,Expr):-
   Expr = var(_,_),!.
negate(Expr,Expr):-
   Expr = literal(_,_),!.
negate(_,_):-
   throw(boolean_type_unsupported).	

% arithmetic operators enter through the third clause
negate_op(X,Y):-
   negate_op_(X,Y),!.	
negate_op(X,Y):-
   negate_op_(Y,X),!.
negate_op(X,X).

negate_op_('==','!=').
negate_op_('<=','>').
negate_op_('<','>=').

or(true,false,true).
or(true,true,true).
or(false,true,true).
or(false,false,false).

map_append(_,[],[]).
map_append(L,[H|T],[New_H|RT]):-
	New_H = L :: H,  
	map_append(L,T,RT).


