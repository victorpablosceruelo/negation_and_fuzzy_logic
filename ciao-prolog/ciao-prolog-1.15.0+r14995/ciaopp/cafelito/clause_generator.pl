:- module(_,
	%_,
	[cafelito2ciao/1,
	 cafelito2ciao/2,
	 cafelito2ciao/3,
	 incore/1,
         java_name_2_ciao_name/2],
	[assertions,regtypes, api(api_types)]).

:- use_module(library(write)).
:- use_module(library(pretty_print), [pretty_print/3]).
:- use_module(library(messages)).
:- use_module(library(assertions(assrt_write))).
:- use_module(program(assrt_db), [assertion_body/7]).

:- use_module(library(lists)).
:- use_module(library(sets), [ord_subtract/3]).
:- use_module(library(vndict), [varnamesl2dict/2]).
:- use_module(library(varnames(dict_types)), [varnamesl/1]).
:- use_module(library(filenames), [basename/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(user, ['$shell_module'/1]).
:- use_module(library(compiler), [use_module/3]).
:- use_module(library(terms), [atom_concat/2]).

% :- use_module(ciaopp(printer),      [output/1]).
% :- use_module(ciaopp(driver),       [clean_analysis_info/0, module/1 ]).

:- use_module(cafelito(unfolder), [ast_unfold/2]).
:- use_module(cafelito(unfolder_types), [method_call/1, variable_version/1, 
				     predicate/1, variable/1]).
:- use_module(cafelito(clause_generator_types)).
:- use_module(cafelito(format_utils), [upper/2]).

:- set_prolog_flag(write_strings,on).

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
%issue_debug_messages( parser ).
%issue_debug_messages( flattener ).
%issue_debug_messages( unfolder ).
%issue_debug_messages( clause_generator ).


:- doc(bug,"Output variable in non-void methods should not be
	    generated here. This is causing a cascade of checkings
	    (arity/generation of calls/ generation of heads).").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Main predicate: parse, modify AST and generate clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- push_prolog_flag(multi_arity_warnings,off).

cafelito2ciao(Java_File):-
   catch(cafelito2ciao_(Java_File,[],_),Exception,error_handler(Exception)).
cafelito2ciao(Java_File,Options):-
   catch(cafelito2ciao_(Java_File,Options,_),Exception,error_handler(Exception)).
cafelito2ciao(Java_File,Options,Ciao_Module):-
   catch(cafelito2ciao_(Java_File,Options,Ciao_Module),Exception,error_handler(Exception)).

:- pop_prolog_flag(multi_arity_warnings).


:- export(option/1).
:- regtype option/1
# "Change the behaviour of the Java to Ciao translation. Possible values are
@begin{itemize}
@item verbose. Pretty print on screen the generated Ciao file.
@item generate_ciao_file. Write on disk generated Ciao file. 
@end{itemize}".

option(verbose).
option(generate_ciao_file).



:- pred incore(Java_File):
	sourcename(Java_File)
#"Generate the equivalent Ciao code for @var{Java_File} in a file
 @tt{Java_File.pl} and load it in the shell so the predicates can be invoked
 as if it was a normal Ciao module".

incore(Java_File):-
   cafelito2ciao(Java_File,[]),
   java2ciao_filename(Java_File,Ciao_File),   
   '$shell_module'(Module),
   use_module(Ciao_File, all, Module).




% cafelito2ciao_(Java_File,Options):-
%    file_up_to_date(Java_File),!,
%    optional_message("Cafelito file unaltered. ",[]),
%    java2ciao_filename(Java_File,Ciao_File),
%    (member(verbose,Options) ->
%        file_to_string(Ciao_File,Program),
%        write_string(Program);
%        true
%    ).
cafelito2ciao_(Java_File,Options,Ciao_Module):-
   generate_ciao(Java_File,Ciao_Module),	
   (member(verbose,Options) ->
       pretty_print_module(Ciao_Module);
       true
   ),
   (member(generate_ciao_file,Options) ->
       java2ciao_filename(Java_File,Ciao_File),   
       pretty_print_module(Ciao_File,Ciao_Module)
      ;
       true
   ).

/*
file_up_to_date(_):- fail,!.
file_up_to_date(Java_File):-
   java2ciao_filename(Java_File,Ciao_File),
   modif_time(Java_File, Java_Time),
   modif_time(Ciao_File, Ciao_Time),
   Ciao_Time > Java_Time.
*/

generate_ciao(Java_File,Ciao_Module):-
   ast_unfold(Java_File,AST_Unfolded),
   save_AST(Java_File,AST_Unfolded),
   optional_message("Doing marking of singleton variables... ",[]),
   statistics(runtime,_),
   debug_message("AST Unfolded"),
   debug_message("~w",[AST_Unfolded]),
   remove_singletons(AST_Unfolded,AST_Cleaned),
   statistics(runtime,[_,T1]),
   optional_message("done in ~3D msec.",[T1],[]),
   %debug_message("~w",[AST_Cleaned]),
   optional_message("Doing generation of Ciao clauses... ",[]),
   statistics(runtime,_),
   ciao_module(AST_Cleaned,Ciao_Module),!,    
   statistics(runtime,[_,T2]),
   optional_message("done in ~3D msec.",[T2],[])
   .
   %,debug_message("~w",[Ciao_Module]).
generate_ciao(_,_):-
   throw(clause_generation_bug).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 CST (Cafelito Syntax Tree) storage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  	
:- pred save_AST(Java_File,AST) :
	( sourcename(Java_File), module(AST) ) 
   #"Saves the Abstract Syntax Tree @var{AST} of file @var{Java_File} to
     help future processing. For example, granularity in the annotation of 
     the original Cafelito can be controlled by checking predicates fields
     that are not present in the Ciao asserted clauses
     The suffix for the AST file is @tt{.cst} (Cafelito Syntax Tree)".

save_AST(Java_File,AST) :- 
   basename(Java_File,Java_File_Without_Extension),	
   atom_concat(Java_File_Without_Extension,'.cst',File),
   open(File,write,Str),	
   write_term(Str,AST,[quoted(true)]),
   write(Str,'.'),
   close(Str).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                 Elimination of singleton variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This can be done in a more natural way in unfolder: 
%
%  Before defining the Write parameters of a virtual predicate, check whether
%  they are used later in the program; if not, mark them as Read because
%  we are not interested in their final value
%   
% BUT
%  - every time we change the grammar (accept new constructions) we have
%    to change the code.
%  - The plain structure of the program here makes it easy to identify 
%    singletons    
%  - The algorithm has cuadratic complexity; the one presented here is linear 
%  - Code of unfolder becomes a blob

remove_singletons(Module,New_Module):-
   Module     = module(Loc,Id,File,Preds),
   del_singleton_preds(Preds,New_Preds),!,
   New_Module = module(Loc,Id,File,New_Preds). 
remove_singletons(_,_):-
   throw(singletons_removal_bug).	

del_singleton_preds([],[]).
del_singleton_preds([P|RP],[NP|RNP]):-
   del_singleton_pred(P,NP),
   del_singleton_preds(RP,RNP).

del_singleton_pred(Pred,New_Pred):-
   Pred     = pred(Loc,Mods,Var,Params,Asserts,Cls),
   del_singleton_clauses(Cls,New_Cls),
   New_Pred = pred(Loc,Mods,Var,Params,Asserts,New_Cls).

del_singleton_clauses([],[]).
del_singleton_clauses([Cls|RCls],[NCls|RNCls]):-
   del_singleton_cls(Cls,NCls),
   del_singleton_clauses(RCls,RNCls).

del_singleton_cls(Cls,New_Cls):-
   Cls    = clause(Loc,Var,(R,W),Sts),
   first_call(Sts,Unused_Set,First_Call,RSts),!,	
   unused_set(RSts,Unused_Set,New_RSts,Unused_Set1),
   remove_unused(W,Unused_Set1,Unused_Set2),
   Unused_Set2 = (_,Vars),
   unify_with_underscore(Vars),
   append(Init_Sts,[ _Old_Call|RSts],Sts),
   append(Init_Sts,[First_Call|New_RSts],New_Sts),
   New_Cls= clause(Loc,Var,(R,W),New_Sts).
del_singleton_cls(Cls,Cls).

first_call([St|RSt],Unused_Set,New_St,RSt):-	
  method_call(St),
  initial_unused_set(St,Unused_Set,New_Call),
  New_St = New_Call.
first_call([St|RSt],Unused_Set,New_St,RSt):-	
  St = assignment(Loc,Var,Call),
  method_call(Call),
  initial_unused_set(Call,Unused_Set,New_Call),
  New_St = assignment(Loc,Var,New_Call).
first_call([_|RSt],Unused_Set,First_Call,Rest):-	
  first_call(RSt,Unused_Set,First_Call,Rest).	

initial_unused_set(Call,Unused_Set,New_Call):-
  Call      = call(Loc,Method_Var,Params),
  Params    = (Read,Write_Input,Write_Output),
  add_unused(Write_Output,([],[]),Unused_Set),
  Unused_Set = (_,Vars),
  append(Read,Write_Input,Tmp1),
  append(Tmp1,Vars,New_Params),
  New_Call   = call(Loc,Method_Var,New_Params).


unused_set([],Unused,[],Unused).
unused_set([St|RSts],Unused,[NSt|NewRSts],New_Unused):-
   unused_set_st(St,Unused,NSt,Unused1),
   unused_set(RSts,Unused1,NewRSts,New_Unused).
   
unused_set_st(St,Unused,New_St,New_Unused):-
  St        = call(Loc,Method_Var,Params),!,
  Params    = (Read,Write_Input,Write_Output),
  add_unused(Write_Output,([],[]),Unused_Set),
  Unused_Set = (_,Vars),
  append(Read,Write_Input,Tmp1),
  append(Tmp1,Vars,New_Params),
  New_St    = call(Loc,Method_Var,New_Params),
  append(Read,Write_Input,Used),
  remove_unused(Used,Unused,Unused1),
  add_unused(Write_Output,Unused1,New_Unused).
unused_set_st(St,Unused,New_St,New_Unused):-
  St     = assignment(Loc,Var,Call),
  method_call(Call),!,
  unused_set_st(Call,Unused,New_Call,New_Unused),
  New_St = assignment(Loc,Var,New_Call).
% this case is valid for assigns to Unary/Bin Operations
unused_set_st(St,Unused,St,New_Unused):-
  St       = assignment(_,_,Op),
  unused_set_st(Op,Unused,Op,New_Unused),!.
% this case is valid for assigns to vars and literals
unused_set_st(St,Unused,St,New_Unused):-
  St       = assignment(_,_,Var_Lit),
  unused_set_vl(Var_Lit,Unused,New_Unused),!.
unused_set_st(St,Unused,St,New_Unused):-
  St       = binary_op(_,Var_Lit1,Var_Lit2),
  unused_set_vl(Var_Lit1,Unused,Unused1),
  unused_set_vl(Var_Lit2,Unused1,Unused2),
  New_Unused   = Unused2.
unused_set_st(St,Unused,St,New_Unused):-
  St       = unary_op(_,Var_Lit),
  unused_set_vl(Var_Lit,Unused,New_Unused).
unused_set_st(St,Unused,St,Unused).


unused_set_vl(Var,Unused,New_Unused):-
  variable_version(Var),
  remove_unused([Var],Unused,New_Unused).
unused_set_vl(_Lit,Unused,Unused).



add_unused(Vs,Unused,New_Unused):-
   Unused = (Variables,Vars),
   append(Vs,Variables,New_Variables),
   create_var_list(Vs,Tmp),
   append(Tmp,Vars,New_Vars),
   New_Unused = (New_Variables,New_Vars).

create_var_list([],[]).
create_var_list([_|RE],[_|RV]):-
   create_var_list(RE,RV).

% if it has been removed it's OK
remove_unused([],Unused,Unused).
remove_unused([V|RV],Unused,Final_Unused):-
   Unused = (Variables,Vars),	
   nth(N,Variables,V),!,
   nth(N,Vars,Erase_Me),
   V = Erase_Me,
   delete(Variables,V,New_Variables),
   ord_subtract(Vars,[Erase_Me],New_Vars),
   New_Unused = (New_Variables,New_Vars),
   remove_unused(RV,New_Unused,Final_Unused).
remove_unused([_|RV],Unused,Final_Unused):-
   remove_unused(RV,Unused,Final_Unused).

unify_with_underscore([]).
unify_with_underscore([V|RV]):-
   V = underscore,
   unify_with_underscore(RV).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                   Clause generation
%             
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ciao_module(Module,Ciao_Module):-
  Module = module(_,Name,File,Preds),
  imported_modules(Imported),
  entry_predicates(Preds,Entry_Preds),
  preds(Preds,(Name,File),Asserts,Clauses),
  Ciao_Module = module(Name,Entry_Preds,Imported,Asserts,Clauses),
  (compat(Ciao_Module,clause_generator_types:module) ->
        true;
        throw(internal_type_error(expected(clause_generator_types:module)))       
   ).


% gather entry preds AND initialize variable dictionary
entry_predicates([],[]).
entry_predicates([P|RP],[E|RE]):-
   P   = pred(_,(entry,_,_),Var,Params,_,_),!,
   Var = var(_,Name),
   Params = (R,W),
   length(R,Arity1),
   length(W,Arity2),
   Arity is Arity1 + Arity2,
   E = '/'(Name,Arity),
   entry_predicates(RP,RE).
entry_predicates([_P|RP],RE):-
   entry_predicates(RP,RE).

imported_modules([assertions,nativeprops]).


preds([],_,[],[]).
preds([P|RP],(Module,File),Asserts,Clauses):-
   pred(P,(Module,File),Asserts_El,Clauses_El),
   preds(RP,(Module,File),RAsserts,RClauses),
   append(Asserts_El,RAsserts,Asserts),
   append(Clauses_El,RClauses,Clauses).

pred(Pred,(Module,File),Asserts,Clauses):-
  Pred = pred(_,_,_,_,_Asserts,Cls),
  pred_assertions((Module,File),Pred,Asserts),
  clauses(Cls,(Module,File),Clauses).

clauses([],_,[]).
clauses([C|RC],(Module,File),[CC|RCC]):-
   clause(C,(Module,File),CC),
   clauses(RC,(Module,File),RCC).

clause(AST,(Module,File),Ciao_Cls):-
   AST   = clause(Lines,Variable,Params,Sts),	
   Params= (Read,Write),
   append(Read,Write,Pars),
   dictionary_get_all(Pars,[],_,Dict1), 
   dictionary_get(Variable,Dict1,_,Dict2),
   predicate_call(Variable,Pars,Dict2,Head,Dict3),   
   goals(Sts,Module,Dict3,Body,New_Dict),
   Lines = (L1,L2),
   Loc   =   loc(  
                  L1,
                  L2,
		  File,
		  Module
		 ),
   varnamesl2dict(New_Dict,VndDict),
   expand(Module,Head,Expanded_Head),
   Ciao_Cls = cls(
		   Expanded_Head,
		   Body,
		   Loc,
		   VndDict  
		  ).
   
goals([],_Module,Dict,'true',Dict).
goals([S|RS],Module,Dict,Goals,New_Dict):-
   goal(S,Module,Dict,CS,Dict1),
   goals(RS,Module,Dict1,RCS,New_Dict),
   (CS = 'true' ->    
       Goals = RCS;

       (RCS = 'true' ->	
           Goals = CS;
           Goals =  ','(CS,RCS)
       )
   ).


goal(AST,Module,Dict,Ciao_Goal,New_Dict):-
   AST  = assignment(_,Var1,Expr),
   Expr = call(_,Var2,Pars),!,
   append(Pars,[Var1],Pars1),
   predicate_call_(Var2,Pars1,Dict,Call,New_Dict),
   expand(Module,Call,Expanded_Call),
   Ciao_Goal = Expanded_Call.
goal(AST,_,Dict,Ciao_Goal,New_Dict):-
   AST = assignment(_,Var,Expr),
   Expr = binary_op(_,_,_),!,
   Var  = var(Type,_,_),
   dictionary_get(Var,Dict,Variable,Dict1),
   arith_expr(Expr,Type,Dict1,Ciao_Expr_Exp,Dict2),
   Ciao_Goal = 'arithmetic:is'(Variable,Ciao_Expr_Exp),
   New_Dict = Dict2.
goal(AST,_,Dict,Ciao_Goal,New_Dict):-
   AST = assignment(_,Var,Expr),
   dictionary_get(Var,Dict,Variable,Dict1),
   arith_expr(Expr,Dict1,RHS,Dict2),
   Ciao_Goal = 'arithmetic:is'(Variable,RHS),
   New_Dict = Dict2.
goal(AST,Module,Dict,Ciao_Goal,New_Dict):-
   AST = call(_,Var,Pars),!,
   predicate_call(Var,Pars,Dict,Call,New_Dict),
   expand(Module,Call,Expanded_Call),
   Ciao_Goal = Expanded_Call.
goal(AST,_,Dict,true,New_Dict):-
   AST     = declaration(_,Var),!,
   dictionary_get(Var,Dict,_,New_Dict).
goal(Expr,_,Dict,Ciao_Expr,New_Dict):-
   Expr = binary_op(_,_,_),!,
   arith_expr(Expr,Dict,Ciao_Expr,New_Dict).
goal(AST,_,D,'true',D):-
   AST = skip.
goal(AST,_,D,'!',D):-
   AST = cut.

:- push_prolog_flag(multi_arity_warnings,off).

arith_expr(Expr,Dict,Ciao_Expr,New_Dict):-
   Expr = var(_,_,_),!,
   dictionary_get(Expr,Dict,Variable,Dict1),
   New_Dict = Dict1,
   Ciao_Expr = Variable.
arith_expr(Expr,Dict,Ciao_Expr_Exp,New_Dict):-
   Expr = binary_op(Op,Expr1,Expr2),!,
   operator(Op,Ciao_Op),
   New_Expr = binary_op(Ciao_Op,Expr1,Expr2),
   binary_op_expr(New_Expr,Dict,Ciao_Expr_Exp,New_Dict).
arith_expr(Expr,Dict,Ciao_Expr,Dict):-
   Expr = literal(Type,Value),!,
   numeric(Type),
   Ciao_Expr = Value.
% this is crappy --> there should be a better way to 
% use one functor or another depending on the type of the 
% result
arith_expr(Expr,Type,Dict,Ciao_Expr_Exp,New_Dict):-
   Expr     = binary_op(Op,Expr1,Expr2),!,
   operator(Type,Op,Ciao_Op),
   New_Expr = binary_op(Ciao_Op,Expr1,Expr2),
   binary_op_expr(New_Expr,Dict,Ciao_Expr_Exp,New_Dict).

:- pop_prolog_flag(multi_arity_warnings).


binary_op_expr(Expr,Dict,Ciao_Expr_Exp,New_Dict):-
   Expr = binary_op(Ciao_Op,Expr1,Expr2),!,
   arith_expr(Expr1,Dict,Ciao_Expr1,Dict1),
   arith_expr(Expr2,Dict1,Ciao_Expr2,Dict2),
   functor(Ciao_Expr,Ciao_Op,2),
   arg(1,Ciao_Expr,Ciao_Expr1),
   arg(2,Ciao_Expr,Ciao_Expr2),
   module_of_operator(Module,Ciao_Op),
   expand(Module,Ciao_Expr,Ciao_Expr_Exp),
   New_Dict = Dict2.
   

predicate_call(Variable,Pars,Dict,Pred_Call,New_Dict):-
  Variable = var(Type,_,_),
  (Type = void ->	
      Pars1 = Pars;
      append(Pars,[Variable],Pars1)
  ),
  predicate_call_(Variable,Pars1,Dict,Pred_Call,New_Dict).

predicate_call_(Variable,Pars,Dict,Pred_Call,New_Dict):-
   functor2name(Variable,Name),
   predicate_get_args(Pars,Dict,Pars_Names,New_Dict),
   length(Pars,N_Args),
   functor(Pred_Call,Name,N_Args),
   Pred_Call =.. [Name|Pars_Names].

predicate_get_args([],Dict,[],Dict).
predicate_get_args([P|RP],Dict,[CP|RCP],New_Dict):-
   (P = literal(_,Value) ->
      CP    = Value,
      Dict1 = Dict;

      (P = underscore -> 
          CP = _,
	  Dict1 = Dict;
	  dictionary_get(P,Dict,CP,Dict1)
      )
   ),
   predicate_get_args(RP,Dict1,RCP,New_Dict).
   

functor2name(Pred,Name):-
   Pred = var(_,Name,_),!.
functor2name(Pred,Name):-
   Pred = var(_,Name).

var2name(Variable,Ciao_Name):-
   Variable = var(Type,Java_Name,Version),!,
   atom_number(Version1,Version),
   atom_concat([Java_Name,'_',Version1],Java_Name_Plus_Num),
   New_Variable = var(Type,Java_Name_Plus_Num),
   var2name(New_Variable,Ciao_Name).
var2name(Variable,Ciao_Name):-
   Variable = var(_,Java_Name),!,
   java_name_2_ciao_name(Java_Name,Ciao_Name).

:- discontiguous java_name_2_ciao_name/2.

:- pred java_name_2_ciao_name(Java_Var,Ciao_Var) :
   ( var(Java_Var), atom(Ciao_Var) ) => atom(Java_Var)
# "Given an atom @var{Java_Var} that represents the name of a Java
  variable, translate it into another atom @var{Ciao_Var} that
  represents a Ciao Variable starting with an upper-case character. ".

java_name_2_ciao_name(Java_Var,Ciao_Var):-
   var(Java_Var),
   !,
   name(Ciao_Var,Tmp1),
   append([Initial],Rest,Tmp1),
   upper(Lower_Initial,Initial),
   append([Lower_Initial],Rest,Java_String_Var),
   name(Java_Var,Java_String_Var).

:- pred java_name_2_ciao_name(Java_Var,Ciao_Var) :
   ( atom(Java_Var), var(Ciao_Var) ) => atom(Ciao_Var)
# "Given an atom @var{Ciao_Var} that represents the name of a Ciao
  variable, translate it into another atom @var{Java_Var} that
  represents a Java Variable starting with an lower-case character. ".

java_name_2_ciao_name(Java_Var,Ciao_Var):-
   name(Java_Var,Tmp1),
   append([Initial],Rest,Tmp1),
   upper(Initial,Upper_Initial),
   append([Upper_Initial],Rest,Ciao_String_Var),
   name(Ciao_Var,Ciao_String_Var).


numeric(byte).
numeric(short).
numeric(int).
numeric(long).
numeric(float).
numeric(double).


:- push_prolog_flag(multi_arity_warnings,off).

operator(byte,  '/','//').
operator(short, '/','//').
operator(int,   '/','//').
operator(long,  '/','/').
operator(float, '/','/').
operator(double,'/','/').
operator(_,X,Y):-
   operator(X,Y).

operator(+,   +).
operator(-,   -).
operator(/,   /).
operator(*,   *).
operator('%', mod).
operator(==,  '=').
operator('!=','\\==').
operator(<=,  '=<').
operator(>=,  '>=').
operator(<,   '<').
operator(>,   '>').

:- pop_prolog_flag(multi_arity_warnings).



% if the key does not exist on the dict, add it
dictionary_get(Var,Dict,Variable,Dict1):-
   var2name(Var,Name),	
   (contains1(Dict,'='(Name,Variable)) ->
       Dict1 = Dict;
       Dict1 = ['='(Name,_V)|Dict],
       Variable = _V
   ).

dictionary_get_all([],D,[],D).
dictionary_get_all([V|RV],Dict,[Var|RVar],New_Dict):-
   dictionary_get(V,Dict,Var,Dict1),
   dictionary_get_all(RV,Dict1,RVar,New_Dict).



module_of_operator(term_basic,   '=')  :-!.
module_of_operator(term_compare,'\\==') :-!.
module_of_operator(arithmetic,   '=<') :-!.
module_of_operator(arithmetic,   '>=') :-!.
module_of_operator(arithmetic,   '<')  :-!.
module_of_operator(arithmetic,   '>')  :-!.
module_of_operator(none,_).

expand(none,Functor,Functor):-!.
expand(Module,Functor,Expanded_Functor):-
     %Expanded_Functor = Module:Functor. 	
   Functor =.. [Head_Functor|Rest],	
   atom_concat([Module,':',Head_Functor],Expanded_Head_Functor),
   Expanded_Functor =.. [Expanded_Head_Functor|Rest].
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                   Assertion generation (API format)
%             
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- regtype pair_module_file/1.

pair_module_file((_Module, File)) :-
	sourcename(File).

:- pred pred_assertions(ModuleFile,Pred,Asserts) 
	: (predicate(Pred), pair_module_file(ModuleFile), var(Asserts))
        => list(Asserts,t_as)
   # "Automatically generate those assertions @var{Asserts} implied by the
      semantics of Java/Cafelito for predicate @var{Pred}".
 
pred_assertions((Module,File),Pred,Asserts):-
   Pred = pred(_,_,_,Params,User_Asserts,_),
   pred_user_assertions((Module,File),Pred,User_Asserts,Asserts1),
   Params = (R,W),
   append(R,W,Pars),
   % unified dictionary for all assertions
   dictionary_get_all(Pars,[],_,Dict),
   pred_type_assertions(Module,Pred,Dict,Asserts2), 
   append(Asserts1,Asserts2,Asserts).


pred_user_assertions(_,_,[],[]).
pred_user_assertions((Module,File),Pred,[UA|RUA],RAs):-
   % assertion empty: the user wrote 'if true'	
   UA = (_,_,_,[[]],[],[],_),
   pred_user_assertions((Module,File),Pred,RUA,RAs).
pred_user_assertions((Module,File),Pred,[UA|RUA],RAs):-
   UA = (_,_,_,[],[[]],[],_),
   pred_user_assertions((Module,File),Pred,RUA,RAs).
pred_user_assertions((Module,File),Pred,[UA|RUA],[As|RAs]):-
   UA = (Loc,Status,Type,Call,Succ,Comp,Dict),
   % complete location info for better error pinpointing
   Loc = (L1,L2),
   New_Loc = loc(L1,L2,File,Module),   
   % names of vars are now upper-case starting
   rename_vndict(Pred,Dict,Ciao_Like_Dict),
   % get assertion head
   Pred = pred(_,_,Pred_Def,Sig,_,_),
   Sig = (R,W),
   append(R,W,Pars),
   predicate_call_(Pred_Def,Pars,Ciao_Like_Dict,Head,_),
   expand(Module,Head,Expanded_Head),
   As = as(
	   Expanded_Head,  % head   
	   Status,         % status
	   Type,           % type
	   [],             % compat
	   Call,           % call
	   Succ,           % succ
	   Comp,           % comp
	   New_Loc,        % loc
           Ciao_Like_Dict, % dict
	   []              % comment
	   ),	   
   pred_user_assertions((Module,File),Pred,RUA,RAs).
	   

rename_vndict(_,[],[]).
rename_vndict(Pred,[H|T],[J|RJ]):-
   Pred = pred(_,_,Var,(R,_W),_,_),	
   Var = var(_,Pred_Name),
   H = '='(Java_Var,Variable),
   java_name_2_ciao_name(Java_Var,Ciao_Var),
   
   (member(var(_,Ciao_Var),R) ->
      J = '='(Ciao_Var,Variable);

      (Java_Var = '\\result' ->
         java_name_2_ciao_name(Pred_Name,Ciao_Pred_Name),
         J = '='(Ciao_Pred_Name,Variable);  
         J = '='(Ciao_Var,Variable)	
      )
   ),
   rename_vndict(Pred,T,RJ).


:- pred pred_type_assertions(Module, Pred,Dict,Type_Asserts) :
	( atm(Module), predicate(Pred), varnamesl(Dict), var(Type_Asserts) ) =>
	list(Type_Asserts, t_as)
   # "Automatically generate types
	assertions @var{Type_Asserts} for a given predicate @var{Pred}
	using dictionary @var{Dict}.  Note that in assertions types
	imply modes: int(A)=> ground(A), diferring from Prolog
	semantics".

pred_type_assertions(Module,Pred,Dict,Type_Asserts):-
   Pred = pred(_,Modifiers,Pred_Def,Sig,_,_),
   Sig = (R,W),
   append(R,W,Pars),
   predicate_call_(Pred_Def,Pars,Dict,Head,_),
   annotate_type(R,Dict,Call1),
   % if the predicate is not iterative (no base case), write variables 
   % are just that (vars) on input. In any case, the result (if any) 
   % is always var
   (Modifiers = (_,simple,_) ->
       annotate_var_mode(W,Dict,Call2);

       (member(Pred_Def,W) ->
          annotate_var_mode([Pred_Def],Dict,Call2);
          Call2 = []	
       )
   ),
  annotate_type(Pars,Dict,Succ),
  expand(Module,Head,Expanded_Head),
  append(Call1,Call2,Call),
  Loc=loc(0,0,unknown,Module),
  Pred_Assert1 = as(
		     Expanded_Head,  % head
		     true,           % status
		     calls,          % type
		     [],             % compat
		     Call,           % call 
		     [],             % succ 
		     [],             % comp     
		     Loc,            % loc
		     Dict,           % dic 
		     []              % comment
		   ),
  Pred_Assert2 = as(
		     Expanded_Head,  % head
		     true,           % status
		     success,        % type
		     [],             % compat
		     Call,           % call 
		     Succ,           % succ 
		     [],             % comp     
		     Loc,            % loc
		     Dict,           % dic 
		     []              % comment
		   ),
  Entry_Assert = as(
		     Expanded_Head,  % head
		     true,           % status
		     entry,          % type
		     [],             % compat
		     Call,           % call 
		     [],             % succ 
		     [],             % comp     
		     Loc,            % loc
		     Dict,           % dic 
		     []              % comment
		   ),
  Type_Asserts = [Pred_Assert1,Pred_Assert2,Entry_Assert].		   
		   


:- pred annotate_type(Vars,Dict,Vars_Types) :
	( list(Vars,variable), varnamesl(Dict), var(Vars_Types) )
        => list(Vars_Types)
   # "For every variable in @var{Vars} produce type properties @var{Vars_Types}
     used in assertions. Dictionary @var{Dict} contains the user-friendly name
     for each variable".

annotate_type([],_,[]).
annotate_type([Var|RVar],Dict,[Ann_Var|RAnn_Var]):-
   Var = var(Type,_),	
   dictionary_get(Var,Dict,Variable,_),
   atom_concat(['basic_props:',Type],Expanded_Type),
   Ann_Var  =.. [Expanded_Type,Variable],
   annotate_type(RVar,Dict,RAnn_Var).



:- pred annotate_var_mode(Vars,Dict,Vars_Variables) :
	( list(Vars,variable), varnamesl(Dict), var(Vars_Variables) )
        => list(Vars_Variables)
   # "For every variable in @var{Vars} generate a property stating that it is
     actually non instantiated (var(_) is true), and store the result in 
     @var{Vars_Variables}. Dictionary @var{Dict} contains the user-friendly name for
      each variable".

annotate_var_mode([],_,[]).
annotate_var_mode([Var|RVar],Dict,[Var_Var|RVar_Var]):-
   dictionary_get(Var,Dict,Variable,_),
   Var_Var  = 'term_typing:var'(Variable),
   annotate_var_mode(RVar,Dict,RVar_Var).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Error handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred error_handler(Ex)
   # "If @var{Ex} is not a variable, print an error message related
     to that exception".

error_handler(Ex):-
   var(Ex),!.	
error_handler(Ex):-
   error_handler_(Ex),!,
   fail.

error_handler_(cafelito_parse_error(Java_File)):-	
   error_message("~w is an invalid Cafelito file ",[Java_File]),
   error_message("The error can be caused by ~n \c
                  1) Java features unsupported in Cafelito ~n \c
                  2) A bug in the AST generator :-(",[]).
error_handler_(java_parse_error(Java_File)):-
   error_message("~w is not a valid Java file \c
		 or there is a bug in the parser: try javac nfile" ,
		 [Java_File]).
error_handler_(flattening_bug):-
   error_message("error flattening the parsed AST").
error_handler_(unfolding_bug):-
   error_message("error unfolding expressions").
error_handler_(clause_generation_bug):-
   error_message("error generating Ciao clauses").
error_handler_(internal_type_error(expected(X))):-
   error_message("Internal type error. Input format is not compliant with ~w",[X]). 	
error_handler_(Exception):-
   error_message("exception of type: ~w",[Exception]). 
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- push_prolog_flag(multi_arity_warnings,off).

:- discontiguous pretty_print_module/2.

:- pred pretty_print_module(Stream,Module) : 
	stream * gnd 
  # "Pretty print on stream @var{Stream} Ciao module (program).   
    @var{Module}. The client is responsable for opening and closing 
    the stream". 

pretty_print_module(Stream,Module) :-
   stream(Stream),!,	
   current_output(Current),
   set_output(Stream),
   Module   = module(Name,Exported,Imported,Asserts,Clauses),
   Mod_Decl = module(Name,Exported,Imported),
   pretty_print(directive(Mod_Decl),[],_),nl,
   pretty_print_cls(whatever,Asserts,Clauses),
   set_output(Current).
   

:- pred pretty_print_module(File,Module) : 
	sourcename * gnd 
   #"Pretty print on file @var{File} Ciao module (program).   
    @var{Module}". 

pretty_print_module(File,Module) :-
   sourcename(File),!,
   open(File,write,Stream),
   (pretty_print_module(Stream,Module) ->
% Changed message so that emacs mode is not confused
       simple_message("written intermediate file ~q",[File]);
       true
   ),	
   close(Stream).


:- pred pretty_print_module(Module) : gnd 
   #"Pretty print on screen Ciao module (program) @var{Module}". 

pretty_print_module(Module) :-
   current_output(Current),
   pretty_print_module(Current,Module).
 
:- pop_prolog_flag(multi_arity_warnings).


pretty_print_cls(_,[],[]).
pretty_print_cls(Actual,Asserts,[Cls|RCls]):-
   Actual = cls(
		 Head,
		 _,
		 _,
		 Dict
		),	
   Cls    = cls(Head,Body,_,_),!,
   pretty_print(clause(Head,Body),[],Dict),
   pretty_print_cls(Actual,Asserts,RCls).
pretty_print_cls(_,Asserts,[Cls|RCls]):-
   nl,nl,
   Cls = cls(
	      Head,
	      Body,
	      _,
	      Dict
	    ),
   pretty_print_as(Cls,Asserts,Rest_Asserts),
   pretty_print(clause(Head,Body),[],Dict),
   pretty_print_cls(Cls,Rest_Asserts,RCls).

pretty_print_as(Cls,[As|RAs],Rest_Asserts):-
   Cls = cls(
	       Head,
	       _,
	       _,
	       _
	    ),
   As   = as(
	       Head,
	       Status,
	       Type,
	       Compat,
	       Call,
	       Succ,
	       Comp,
	       _,
	       Dict,
	       _
	     ),!,
   assertion_body( Head , Compat , Call , Succ , Comp , [] , Body ),
   write_assertion(Head, Status, Type, Body, Dict, []),
   pretty_print_as(Cls,RAs,Rest_Asserts).
pretty_print_as(_,Rest_Asserts,Rest_Asserts).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              auxiliar preds	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

java2ciao_filename(Src_File,Ciao_File) :-
	basename(Src_File,Src_File_No_Extension),	
	atom_concat(Src_File_No_Extension,'.pl',Ciao_File).
