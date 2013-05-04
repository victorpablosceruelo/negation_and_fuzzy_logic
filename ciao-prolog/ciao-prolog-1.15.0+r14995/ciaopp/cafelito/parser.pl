 :- module(_,
	 [parse/2,parse/3],
	 [dcg,fsyntax,hiord]).


:- use_module(library(messages)).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(file_utils), [file_to_string/2, stream_to_string/2]).
:- use_module(library(system), [exec/4]).

:- use_module(cafelito(parser_utils)).
:- use_module(cafelito(java_lexer)).
:- use_module(cafelito(just_java_parser), [parse/1]).

:- use_module(cafelito(cml_parser), [method_annotation/3]).

:- set_prolog_flag(write_strings,on).


:- op(200,xfy,['or']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Testing predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- push_prolog_flag(multi_arity_warnings,off).

parse(Java_File,Tree):-
    file_to_string(Java_File,Java_Code),
    optional_message("Doing Cafelito grammatical recognition... ",[]),
    statistics(runtime,_),
    compilation_unit(Tree,(Java_Code,1),([],_)),!,
    statistics(runtime,[_,T]),
    optional_message("done in ~3D msec.",[T],[]),
    debug_message("Generated tree: ~w",[Tree]).
parse(Java_File,_Tree):-
   debug_message("Doing Java grammatical recognition... "),
   statistics(runtime,_),
   (just_java_parser:parse(Java_File) ->
       statistics(runtime,[_,T]),
       debug_message("done in ~3D msec.",[T]),
       throw(cafelito_parse_error(Java_File));

       throw(java_parse_error(Java_File))
   ).

parse(Java_File,javac,Tree):-
   simple_message("Doing javac compilation... "),
   statistics(runtime,_),
   atom_concat('javac ',Java_File,Javac),
   exec(Javac,_Input,_Output,Error),
   (get_code(Error,-1) ->
      statistics(runtime,[_,T]),
      simple_message("done in ~3D msec.",[T]),
      parse(Java_File,Tree);

      stream_to_string(Error,Cause),
      error_message(Cause),
      throw(javac_compilation_error(Java_File))
  ).

:- pop_prolog_flag(multi_arity_warnings).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   
%                    Auxiliary functions
% TODO --> move these rules to flattener because are SEMANTIC not syntactic
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Assign type (and default value, if neccessary) to a list of vars
% Can be done with unification but that would be pretty obscure
map_type_and_value([],_Type,[]).
map_type_and_value([(Line,Id,empty)|Rest],Type,[Var_Decl|Rest_Var_Decl]):-
   !,	
   default_value(Type,Expr),
   Id = id(Name),
   Var_Decl = declaration(Line,var(Type,Name),Expr),
   map_type_and_value(Rest,Type,Rest_Var_Decl).
map_type_and_value([(Line,Id,Expr)|Rest],Type,[Var_Decl|Rest_Var_Decl]):-
   Id = id(Name),	
   Var_Decl = declaration(Line,var(Type,Name),Expr),
   map_type_and_value(Rest,Type,Rest_Var_Decl).

default_value(byte  ,literal(int,'0')).
default_value(short ,literal(int,'0')).
default_value(int   ,literal(int,'0')).
default_value(long  ,literal(float,'0')).
default_value(float ,literal(float,'0')).
default_value(double,literal(float,'0')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                     DECLARATIONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



compilation_unit(T) -->
  comments_and_spaces, 
  zero_or_one([type_declaration(_)],[T1]),
  {
   T = T1
  }.

modifier(T) -->
    keyword("public"),  { T = public}
 |  keyword("private"), { T = private} 
 |  keyword("static") , { T = static}.

modifiers(T) --> 
  zero_or_more([modifier(_)],T).

type_declaration(T) -->
  (separator(";"),
   {
     T = ignore
   }
  | line(L1),modifiers(T1),class_or_interface_declaration(T2),line(L2),
    {
     T1 = [public],
     T2 = (Id,Methods),
     T  = class((L1,L2),Id,Methods)
    }
  ),!.

class_or_interface_declaration(T) -->
   keyword("class"),
   identifier(T1),
   class_or_interface_body(T2),
   {
    T1 = id(Name),	
    T = (Name,T2)
   }.

class_or_interface_body(T) -->
  separator("{"),
  zero_or_more([class_or_interface_body_declaration(_)],T),
  separator("}").

class_or_interface_body_declaration(T) -->
   (
    method_annotation(Asserts),
    line(L1),
    modifiers(T1),
    method_declaration(T2),
    {
     (T1 = [static,Access] ->
         true;
         T1 = [Access,static]
     ),
     T2    = (Variable,Formal_Parameters,Block),
     % retrieve ending line number
     Block = block((_,BL2),_),
     T     = method((L1,BL2),Access,Variable,Formal_Parameters,Asserts,Block)
    } 	
   | separator(";"),
     {
      T = ignore
     }
   ),!.

variable_declarator(T) -->
   variable_declarator_id(T1),
   zero_or_one([operator('=',ignore),variable_initializer(_)],T2),
   line(L2),
   {
    T2 = [] ->	
       T  = (L2,T1,empty);
       T2 = [T2_El], 	
       T  = (L2,T1,T2_El)
   }.

variable_declarator_id(T) -->
   identifier(T).

variable_initializer(T) -->
   expression(T).

method_declaration(T) -->
  result_type(T1),
  method_declarator(T2),
  line(L1),block(T3),line(L2),
  {
   T2 = (Id,Formal_Parameters),
   Id = id(Name),
   % in general, we are not interested in unnecessary nestings
   % but here it's important as entry point for the method
   (T3 = block(_,_) ->
      Block = T3;
      Block = block((L1,L2),[T3])
   ),
   T  = (var(T1,Name),Formal_Parameters,Block)
  }.

method_declarator(T) -->
  identifier(T1),
  formal_parameters(T2),
  {
   T=(T1,T2)
  }.	

formal_parameters_aux(T) -->
   zero_or_more([separator(","),formal_parameter(_)],T).	

formal_parameters(T) -->
  separator("("),
  zero_or_one([formal_parameter(_),formal_parameters_aux(_)],T),
  separator(")").

formal_parameter(T) -->
  type_(T1),
  variable_declarator_id(T2),
  {
   T2 = id(Id),	
   T =  var(T1,Id)
  }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      BLOCKS and COMMANDS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
statement(T1) -->
  ( 
    block(T1)
  | empty_statement(T1)
  | statement_expression(T1),separator(";")
  | if_statement(T1)
  | while_statement(T1)
  | do_statement(T1)
  | return_statement(T1)
  ),!.

% since local_variable_declaration is a list, we flatten the result
block(T) -->
   line(L1),	
   separator("{"),
   zero_or_more([block_statement(_)],T1),
   line(L2),
   separator("}"),
    {
     % if T1 is just 1 block, don't wrap it again
     T1 = [One_Block] ->
        T = One_Block;
        T = block((L1,L2),T1)
    }.

block_statement(T) -->
  ( local_variable_declaration(T)
  | statement(T1),
    {
     T = [T1]
    }  	 
  ),!.

local_variable_declaration(T) -->
  type_(Type),
  variable_declarator(T1),
  zero_or_more([separator(","),variable_declarator(_X)],T2),
  {
   map_type_and_value([T1|T2],Type,T)
  }.

empty_statement(T) -->
  separator(";"),
   {
    T = ignore
   }.

statement_expression_aux(T) -->
   assignment_operator(T1),expression(T2),
   {
    T = (T1,T2)
   }.

statement_expression(T) -->
    primary_expression(T1),
    zero_or_one([statement_expression_aux(_)],T2),
    line(L2),
    {
     T2 = [] ->
        T = T1;

	T2 = [(Assign_Op,Expr)],
	T = assignment(L2,Assign_Op,T1,Expr)
    }.

if_statement(T) -->
  line(L_If1),	
  keyword("if"),
  separator("("),expression(T1),separator(")"),
  statement(T2),
  line(L_Else1),
   {   
     % for more precision in line numbers
    (T2    = block((_,LBT2),_) ->
         L_If2 = LBT2;
	 L_If2 = L_Else1
    )
   },
  zero_or_one([keyword("else"),statement(_)],T3),
  line(L_End),
  {
   (T3 =[] ->
      T  = if(((L_If1,L_If2),(na,na)),T1,T2,skip);

      T3    = [T3_El],
      (T3_El = block((_,LBT3),_) ->
          L_Else2 =  LBT3;
	  L_Else2 =  L_End
      ),
      T = if(((L_If1,L_If2),(L_Else1,L_Else2)),T1,T2,T3_El)
   )
  }.

while_statement(T) -->
  line(L1),
  keyword("while"),
  separator("("),expression(Expr),separator(")"),
  statement(St),
  line(L2),
   {
    T = while((L1,L2),Expr,St)	
   }.

do_statement(T) -->
  line(L1),	
  keyword("do"),
  statement(St),
  keyword("while"),
  separator("("),expression(Expr),separator(")"),line(L2),separator(";"),
   {
    T = do((L1,L2),Expr,St)
   }.
return_statement(T) -->
  keyword("return"),
  zero_or_one([expression(_)],T1),
  line(L2),
  separator(";"),
   {
    T1 = [] ->
       T  = return(L2,void);
       T1 = [T1_El], 	
       T  = return(L2,T1_El)
   }.	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      EXPRESSIONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% right-associativity for assignment expressions comes for free
% FIXME
% if it's an assigment, the lhs has to have type 'name'
% LL(1) reference grammar (and JLS-ch18,Antlr) accept things like 'a+b=8;'
expression(T) -->
   equality_expression(T1),
   zero_or_one([assignment_operator(_), expression(_)],T2),
   line(L2),
    {
     T2 = [] ->
        T = T1;

	T2=[Op,Expr],
        T = assignment(L2,Op,T1,Expr)
    }.

% This trick does not confuse things (because assignments are treated separately
% from binary_operations) and simplifies future processing
assignment_operator(T) -->
 ( operator('=')   ,{T = '='}
 | operator('+=')  ,{T = '+'}
 | operator('-=')  ,{T = '-'}
 | operator('*=')  ,{T = '*'}
 | operator('/=')  ,{T = '/'}
 | operator('%=')  ,{T = '%'}
 ),!.

% left associativity for boolean/arithmetic expressions
% this is a semantic interpretation so maybe it shouln't be here
prefix(Lexpr,[],Lexpr).
prefix(Lexpr,[Op,Rexpr|R],Prefix):-
   New_Lexpr = binary_op(Op,Lexpr,Rexpr),
   prefix(New_Lexpr,R,Prefix).


% conditional_or_expression(T) --> 
%    conditional_and_expression(T1),
%    zero_or_more([operator('||'), conditional_and_expression(_)],T2),
%    {
%     prefix(T1,T2,T)
%    }.

% conditional_and_expression(T) --> 
%    equality_expression(T1), 
%    zero_or_more([operator('&&'), equality_expression(_)],T2),
%   {
%     prefix(T1,T2,T)
%    }.
 
equality_expression(T) --> 
   relational_expression(T1), 
   zero_or_more([(operator('==') or operator('!=')),
		relational_expression(_)],T2),
  {
    prefix(T1,T2,T)
   }.

relational_expression(T) -->
   additive_expression(T1),
   zero_or_more([( operator('<=') or operator('>=') 
                   or operator('<') or operator('>')),
                  additive_expression(_)],T2),
  {
    prefix(T1,T2,T)
  }.

additive_expression(T) --> 
   multiplicative_expression(T1),
   zero_or_more([(operator('+') or operator('-')),
                  multiplicative_expression(_)],T2),
   {
    prefix(T1,T2,T)
   }.
 
multiplicative_expression(T) --> 
   unary_expression_not_plus_minus(T1),
   zero_or_more([( operator('*') or operator('/') or operator('%')),
                  unary_expression_not_plus_minus(_)],T2),
   {
    prefix(T1,T2,T)
   }.

unary_expression_not_plus_minus(T) --> 
 ( operator('!'), unary_expression_not_plus_minus(T1),
    {
     T = unary_op('!',T1)
    }
  | primary_expression(T)
 ),!.

primary_expression(T) -->
   primary_prefix(T1),zero_or_more([arguments(_)],T2),line(L2),
   {
    T2 = [] ->
       T = T1;
       T = call(L2,T1,T2)
   }.

primary_prefix(T) -->
 (
   literal(T) 
 | variable(T)
 | separator("("), expression(T), separator(")")
 ),!.


arguments(T) -->
   separator("("),zero_or_one([argument_list(_)],T),separator(")").


argument_list(T) --> 
   expression(T1),zero_or_more([separator(",") ,expression(_)],T2),
   {
    T = [T1|T2]     
   }.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      TYPES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_(T) -->
   primitive_type(T).

primitive_type(T) -->  
   ( keyword("byte"),    { T = byte }	  
   | keyword("short"),   { T = short }	 
   | keyword("int"),     { T = int   }	 
   | keyword("long"),    { T = long  }	 
   | keyword("float"),   { T = float  }	  
   | keyword("double"),  { T = double  }	 
   ),!.

result_type(T) -->
 ( keyword("void"),      { T = void  }
 | type_(T)
 ),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      NAMES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

variable(T) -->
   identifier(id(Name)),
   {
    T = var(_T,Name)
   }.

