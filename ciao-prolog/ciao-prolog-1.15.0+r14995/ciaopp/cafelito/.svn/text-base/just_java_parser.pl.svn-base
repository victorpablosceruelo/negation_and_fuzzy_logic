%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                PARSER BASED ON JAVACC grammar
%            The lexer is based directly on JLS v3 
%
% This grammar is constructed upon JLSpecs v3 (->Java 1.5)
% although the one specified in the book (chp18) is not LL(1)
% and contains lots of typos. The extended version of it 
% ,scattered through the preceding chapters, is correct but LALR(1)
%
% Note the different use of '|' (DCG) and 'or' (preds in parsing_utils)
% expansions can do weird things otherwise
% 
%
% TODO
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(_,_,[dcg,fsyntax,hiord]).

:- use_module(library(lists)).
:- use_module(library(file_utils)).


% avoid nasty issues
:- set_prolog_flag(write_strings,on).
:- set_prolog_flag(multi_arity_warnings,off).


:- op(200,xfy,['or']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Testing predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse(Java_File):-
 	file_to_string(Java_File,Java_Code), 
	set_prolog_flag(write_strings,on),
 	compilation_unit(Java_Code,[]),!.
parse(Java_File):-
       throw(java_parse_error(Java_File)).

batch_parse_(P,NP,PF,NPF) -->
	one_or_more([comment or sp]),!,
	batch_parse_(P,NP,PF,NPF).
batch_parse_(P,NP,P,NP) -->
	"EOF".
batch_parse_(Passed,Not_Passed,PF,NPF,S0,SFF):-
	one_or_more([string_character],S0,S1),!,
	append(File,S1,S0),
	name(File_Atom,File),
	append("~/research/JSA/project/examples/jdk1.3.1src/",File,Path),
	name(Path_Atom,Path),
	(parse(Path_Atom) ->
	  Passed1 is Passed+1,
	  Not_Passed1 is Not_Passed,
	  nl,display('Parsed successfully --> '),display(File_Atom);

	  Passed1 is Passed, 
	  Not_Passed1 is Not_Passed + 1,
	  nl,display('Not passed --> '),display(File_Atom)
	),
	line_terminator(S1,SF),
	batch_parse_(Passed1,Not_Passed1,PF,NPF,SF,SFF).
	

batch_parse(Batch_File):-
 	file_to_string(Batch_File,File_List),
	batch_parse_(0,0,P,NP,File_List,_),
	nl,nl,display('**************  Results********************'),nl,
	display('passed...'),display(P),nl,
	display('NOT passed...'),display(NP),nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   
%                    Auxiliary functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this is neccessary for not changing the order of the parameters
% in a call of type 'zero_or_one(identifier("pepe"))'
% which otherwise is transformed to identifier(S0,"pepe",SF)
match_el(X,S0,SF):-
	functor(X,Pred,1),!,
	arg(1,X,P1),
	call(Pred,P1,S0,SF).
match_el(X,S0,S1):-
	list(X),!,
	append(X,S1,S0).
match_el(X,S0,SF):-
	functor(X,'or',2),!,
	 X = 'or'(Opt1,Opt2),
	(match_el(Opt1,S0,SF) ->
	   true;
	   match_el(Opt2,S0,SF)
	).
match_el(X,S0,SF):-
	call(X,S0,SF).

match([],SF,SF).
match([X|R],S0,SF):-
	match_el(X,S0,S1),
	match(R,S1,SF).

% if we do not make a copy of the term  nasty unifications take place
% eg: java.io.IOException against pattern zero_or_more(".",id(_X))
% in the first success _X is unified against "io"
% thus now the pattern is really (".",id("io"))
% next time match is invoked unification fails
zero_or_more(Pattern,S0,SF):-
	copy_term(Pattern,Pattern_Copy),
	match(Pattern,S0,S1),!,
	zero_or_more(Pattern_Copy,S1,SF).
zero_or_more(_Pattern,SF,SF).

one_or_more(Pattern,S0,SF):-
	copy_term(Pattern,Pattern_Copy),
	match(Pattern,S0,S1),!,
	zero_or_more(Pattern_Copy,S1,SF).

zero_or_one(Pattern,S0,SF):-
	match(Pattern,S0,SF),!.
zero_or_one(_Pattern,SF,SF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                     DECLARATIONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compilation_unit -->
  zero_or_more([comment or sp]),	
  zero_or_one([package_declaration]),
  zero_or_more([import_declaration]),
  zero_or_more([type_declaration]).


package_declaration -->
  keyword("package"),name_,separator(";").

import_declaration -->
  keyword("import"),
  zero_or_one([keyword("static")]),
  name_,
  zero_or_one([separator("."),operator("*")]),
  separator(";").

modifier -->
    keyword("public") 
 |  keyword("private") 
 |  keyword("protected") 
 |  keyword("static") 
 |  keyword("final") 
 |  keyword("native") 
 |  keyword("synchronized") 
 |  keyword("abstract") 
 |  keyword("threadsafe") 
 |  keyword("transient")  
 |  keyword("volatile")
 |  keyword("strictfp").

modifiers --> 
  zero_or_more([modifier]).

type_declaration -->
  (separator(";")
  |modifiers,
  ( class_or_interface_declaration
   | enum_declaration
   | annotation_type_declaration
   ),!
  ),!.

class_or_interface_declaration -->
  (keyword("class")|keyword("interface")),
   identifier(_),
   zero_or_one([type_parameters]),
   zero_or_one([extends_list]),
   zero_or_one([implements_list]),
   class_or_interface_body.

extends_list -->
  keyword("extends"),class_or_interface_type,
  zero_or_more([separator(","),class_or_interface_type]).

implements_list -->
  keyword("implements"),class_or_interface_type,
  zero_or_more([separator(","),class_or_interface_type]).

enum_declaration -->
  keyword("enum"),identifier(_),zero_or_one([implements_list]),enum_body.

enum_body_aux -->
   zero_or_more(class_or_interface_body_declaration).	

enum_body -->
  separator("{"),
  enum_constant,zero_or_more([separator(","),enum_constant]),
  zero_or_one([separator(";"),enum_body_aux]),
  separator("}").

enum_constant -->
  identifier(_),
  zero_or_one([arguments]),
  zero_or_one([class_or_interface_body]).

type_parameters -->
  operator("<"),
  type_parameter,
  zero_or_more([separator(","),type_parameter]),
  operator(">").

type_parameter -->
  identifier(_),zero_or_one([type_bound]).

type_bound -->
  keyword("extends"),
  class_or_interface_type,
  zero_or_more([operator("&"),class_or_interface_type]).	

class_or_interface_body -->
  separator("{"),
  zero_or_more([class_or_interface_body_declaration]),
  separator("}").

class_or_interface_body_declaration -->
   ( initializer
   | modifiers,
     ( class_or_interface_declaration
     | enum_declaration
     | constructor_declaration
     | field_declaration
     | method_declaration
     ),!
   |separator(";")
   ),!.

field_declaration -->
  type_,
  variable_declarator,
  zero_or_more([separator(","),variable_declarator]),
  separator(";"). 	

variable_declarator -->
   variable_declarator_id,
   zero_or_one([operator("="),variable_initializer]).

variable_declarator_id -->
   identifier(_),zero_or_more([brackets]).	

variable_initializer -->
 ( array_initializer
 | expression
 ),!.

array_initializer_aux -->
   zero_or_more([separator(","),variable_initializer]).

array_initializer -->
  separator("{"),
  zero_or_one([variable_initializer,array_initializer_aux]),
  zero_or_one([separator(",")]),
  separator("}").

method_declaration -->
  zero_or_one([type_parameters]),
  result_type,
  method_declarator,
  zero_or_one([keyword("throws"),name_list]),
  (block|separator(";")).

method_declarator -->
  identifier(_),formal_parameters,zero_or_more([brackets]).

formal_parameters_aux -->
   zero_or_more([separator(","),formal_parameter]).	
	
formal_parameters -->
  separator("("),
  zero_or_one([formal_parameter,formal_parameters_aux]),
  separator(")").	

formal_parameter -->
  zero_or_one([keyword("final")]),
  type_,
  zero_or_one(["..."]),
  variable_declarator_id.

constructor_declaration -->
  zero_or_one(type_parameters),
  identifier(_),
  formal_parameters,
  zero_or_one([keyword("throws"),name_list]),
  separator("{"),
   % My understanding is
  zero_or_one([explicit_constructor_invocation]),
  zero_or_more([block_statement]),
  separator("}").

explicit_constructor_invocation -->
  ( keyword("this"),arguments
  | zero_or_one([primary_expression,separator(".")]),
    keyword("super"),
    arguments,
    separator(";")
  ),!.

initializer -->
  zero_or_one([keyword("static")]),block.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      Annotations (Java 5)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annotation -->
  ( normal_annotation	
  | single_member_annotation
  | marker_annotation
  ).

normal_annotation -->
  "@",name_,separator("("),zero_or_one([member_value_pairs]),separator(")").

single_member_annotation -->
  "@",name_,separator("("),member_value,separator(")").

marker_annotation -->
  "@",name_.

member_value_pairs -->
  member_value_pair,zero_or_more([separator(","),member_value_pair]).

member_value_pair -->	
  identifier(_),operator("="),member_value.

member_value -->
 ( annotation	
 | member_value_array_initializer
 | conditional_expression
 ),!.

member_value_array_initializer -->
  separator("{"),
  member_value,
  zero_or_more([separator(","),member_value]),
  zero_or_one([separator(",")]),
  separator("}").

annotation_type_declaration -->
 "@",keyword("interface"),identifier(_),annotation_type_body.

annotation_type_body -->
  separator("{"),
  zero_or_more([annotation_type_member_declaration]),
  separator("}").

annotation_type_member_declaration -->
   (modifiers,
   ( type_,
     identifier(_),
     separator("("),separator(")"),
     zero_or_one([default_value]),
     separator(";")
   | class_or_interface_declaration
   | enum_declaration
   | annotation_type_declaration
   | field_declaration
   ),!
   |
   separator(";")
   ),!.

default_value -->
  keyword("default"),member_value.	
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      BLOCKS and COMMANDS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

statement -->
  ( labeled_statement
  | assert_statement
  | block
  | empty_statement
  | statement_expression,separator(";")
  | switch_statement
  | if_statement
  | while_statement
  | do_statement
  | for_statement
  | break_statement
  | continue_statement
  | return_statement
  | throw_statement
  | synchronized_statement
  | try_statement
  ),!.

assert_statement -->
   keyword("assert"),
   expression,
   zero_or_one([operator(":"),expression]),
   separator(";").

labeled_statement -->
   identifier(_),operator(":"),statement.

block -->
   separator("{"),zero_or_more([block_statement]),separator("}").	

block_statement -->
  ( local_variable_declaration
  | statement
  | modifiers,class_or_interface_declaration
  ),!.

local_variable_declaration -->
  zero_or_one([keyword("final")]),
  type_,
  variable_declarator,
  zero_or_more([separator(","),variable_declarator]).

empty_statement -->
  separator(";").

statement_expression_aux -->
   assignment_operator,expression.

statement_expression -->
 ( preincrement_expression
 | predecrement_expression	
 | primary_expression,
   zero_or_one([(operator("++") 
                or operator("--") 
		or statement_expression_aux)])
 ).
	
switch_statement_aux -->
  zero_or_more([block_statement]).
	
switch_statement -->
  keyword("switch"),
  separator("("),
  expression,
  separator(")"),
  separator("{"),
  zero_or_more([switch_label,switch_statement_aux]),
  separator("}").

switch_label -->
  ((keyword("case"),expression,operator(":"))
  |(keyword("default"),operator(":"))
  ),!.


if_statement -->
  keyword("if"),
  separator("("),expression,separator(")"),
  statement,zero_or_one([keyword("else"),statement]).	

while_statement -->
  keyword("while"),separator("("),expression,separator(")"),statement.

do_statement -->
  keyword("do"),statement,
  keyword("while"),
  separator("("),expression,separator(")"),separator(";").

for_statement -->
  keyword("for"),separator("("),
  ( type_,identifier(_),operator(":"),expression
  | zero_or_one([for_init]),separator(";"),
    zero_or_one([expression]),separator(";"),
    zero_or_one([for_update])
  ),!,
  separator(")"),statement.

for_init -->
 ( local_variable_declaration
 | statement_expression_list
 ),!.

statement_expression_list -->
  statement_expression,
  zero_or_more([separator(","),statement_expression]).

for_update -->
  statement_expression_list.

break_statement -->
  keyword("break"),zero_or_one([identifier(_)]),separator(";").	

continue_statement -->
  keyword("continue"),zero_or_one([identifier(_)]),separator(";").	

return_statement -->
  keyword("return"),zero_or_one([expression]),separator(";").	

throw_statement -->
  keyword("throw"),expression,separator(";").	

synchronized_statement -->
  keyword("synchronized"),separator("("),expression,separator(")"),block.

try_statement -->
   keyword("try"),
   block,
   zero_or_more([keyword("catch"),separator("("),formal_parameter,separator(")"),block]),
   zero_or_one([keyword("finally"),block]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      EXPRESSIONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expression -->
	conditional_expression,
	zero_or_one([assignment_operator, expression]).

assignment_operator -->
 ( operator("=")
 | operator("+=")
 | operator("-=")
 | operator("*=")
 | operator("/=")
 | operator("&=")
 | operator("|=")
 | operator("^=")
 | operator("%=")
 | operator("<<=")
 | operator(">>=")
 | operator(">>>=")
 ),!.

conditional_expression -->
   conditional_or_expression, 
   zero_or_one([operator("?"), expression, operator(":") ,expression]) .

conditional_or_expression --> 
   conditional_and_expression,
   zero_or_more([operator("||"), conditional_and_expression]) .

conditional_and_expression --> 
   inclusive_or_expression, 
   zero_or_more([operator("&&"), inclusive_or_expression]) .
 
inclusive_or_expression --> 
   exclusive_or_expression,
   zero_or_more([operator("|"), exclusive_or_expression]) .
	
exclusive_or_expression -->
   and_expression,
   zero_or_more([operator("^"),and_expression]) .

and_expression --> 
   equality_expression,
   zero_or_more([operator("&"), equality_expression]) .

equality_expression --> 
   instanceof_expression, 
   zero_or_more([(operator("==") or operator("!=")),
		instanceof_expression]) .

instanceof_expression -->
	relational_expression,
	zero_or_one([keyword("instanceof"),type_]) .

relational_expression -->
   shift_expression,
   zero_or_more([( operator("<=") or operator(">=") 
                   or operator("<") or operator(">")),
                  shift_expression]) .

shift_expression --> 
   additive_expression,
   zero_or_more([(operator(">>>") or operator(">>") or operator("<<")),
                  additive_expression]) .

additive_expression --> 
   multiplicative_expression,
   zero_or_more([(operator("+") or operator("-")),
                 multiplicative_expression]) .

multiplicative_expression --> 
   unary_expression,
   zero_or_more([( operator("*") or operator("/") or operator("%")),
                  unary_expression]) .

unary_expression --> 
 ( operator("+"), unary_expression 
 | operator("-"), unary_expression
 | preincrement_expression 
 | predecrement_expression
 | unary_expression_not_plus_minus
 ),!.

predecrement_expression --> 
   operator("--"), primary_expression.

preincrement_expression -->
   operator("++"), primary_expression.

unary_expression_not_plus_minus --> 
 (  operator("~"), unary_expression
  | operator("!"), unary_expression
  | cast_expression
  | postfix_expression
 ),!.
 
postfix_expression -->
   primary_expression,
   zero_or_one([(operator("++") or operator("--"))]).

cast_expression --> 
 (  separator("("), primitive_type, separator(")"), unary_expression
 |  separator("("), reference_type, 
    separator(")"), unary_expression_not_plus_minus
 ),!.

primary_expression -->
   primary_prefix,zero_or_more([primary_suffix]) .


member_selector -->
   separator("."),type_arguments,identifier(_).

primary_prefix -->
 (
   literal(_) 
 | keyword("this")
 | keyword("super"),separator("."),identifier(_)
 | separator("("), expression, separator(")")
 | allocation_expression
 | result_type,token(".class")
 | name_
 ),!.

primary_suffix -->
 ( 
   separator("."),keyword("this")
 | separator("."),allocation_expression 	
 | member_selector
 | separator("["),expression,separator("]")
 | separator("."),identifier(_)
 | arguments
 ),!.

arguments -->
   separator("("),zero_or_one([argument_list]),separator(")").

argument_list --> 
   expression,zero_or_more([separator(",") ,expression]) .

% note how do we translated the LOOKAHEAD(2) of the original grammar
allocation_expression -->
 ( keyword("new"),primitive_type,!,array_dims_and_inits
 | keyword("new"),class_or_interface_type,
   zero_or_one([type_arguments]) ,
   ( array_dims_and_inits
   | arguments,
     zero_or_one([class_or_interface_body])
   ),!
 ).


array_dims_and_inits -->
   one_or_more([brackets]),!,array_initializer
 | one_or_more([separator("["),expression,separator("]")]),
   zero_or_more([brackets]).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      TYPES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


type_ -->
  ( reference_type
  | primitive_type
  ),!.

reference_type --> 
  ( primitive_type,!,one_or_more([brackets])
  | class_or_interface_type,zero_or_more([brackets])
  ),!.

class_or_interface_type_aux -->
   zero_or_one([type_arguments]). 	

class_or_interface_type --> 
   identifier(_),
   zero_or_one([type_arguments]),
   zero_or_more([separator("."),identifier(_),class_or_interface_type_aux]).

type_arguments -->
   operator("<"),
   type_argument,zero_or_more([separator(","),type_argument]),
   operator(">").	


type_argument -->
   (reference_type	
   |operator("?"),zero_or_one([wildcard_bounds])
   ),!.

wildcard_bounds -->
 ( keyword("extends"),reference_type
 | keyword("super")  ,reference_type
 ),!.

primitive_type -->  
   ( keyword("boolean")
   | keyword("byte") 
   | keyword("short")
   | keyword("int")
   | keyword("long")
   | keyword("char")
   | keyword("float") 
   | keyword("double")
   ),!.

result_type -->
 ( keyword("void")
 | type_
 ),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      NAMES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name_ --> 
   identifier(_),zero_or_more([separator(".") ,identifier(_)]).

name_list -->
   name_,zero_or_more([separator(","),name_]).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      TOKENS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nothing -->"".
white --> 
   " "
 |"\t"
 |"\f"
 | line_terminator.
 
line_terminator -->
 ("\r\n"
 |"\r"
 |"\n"
 ),!.

sp  --> one_or_more([white]).
opt_sp  --> zero_or_more([white]).

keyword(K,S0,SF) :-
  keyword_(S0,S1),
  append(K,S1,S0),
  sp(S1,S2),!,
  zero_or_more([comment or sp],S2,SF).
keyword(K,S0,SF) :-
  keyword_(S0,S1),
  append(K,S1,S0),
  separator(_,S1,_),
  SF = S1.
% the only exception to the rule [sic]
keyword("default",S0,SF) :-
  keyword_(S0,S1),
  append("default",S1,S0),
  operator(":",S1,_),
  SF = S1.

keyword_ -->
  "abstract"|"assert"| "boolean" |"break" |"byte"         |"case"       
  |"catch"     |"char"       |"class"     |"const"        |"continue" 
  |"default"   |"do"         |"double"    |"else"         |"enum"
  |"extends"   |"final"      |"finally"   |"float"        | "for"   
  | "goto"     | "if"        |"implements"|"import"       |"instanceof"
  |"int"       |"interface"  |"long"      |"native"       |"new"  
  |"package"   | "private"   | "protected"| "public"      | "return" 
  | "short"    | "static"    |"strictfp"  |"super"        | "switch" 
  | "synchronized"| "this"   | "throw"    |"throws"       |"transient"
  |"try"       | "void"      | "volatile" |"while".


java_letter([X|SF],SF) :- X >= 0'a, X =< 0'z, !.
java_letter([X|SF],SF) :- X >= 0'A, X =< 0'Z, !.
java_letter([X|SF],SF) :- X = 0'$,!.
java_letter([X|SF],SF) :- X = 0'_,!.
java_digit([X|SF],SF)  :- X >= 0'0, X =< 0'9.
java_letter_or_digit -->
   java_letter
 | java_digit.	


identifier(Id,S0,SF) :-
   java_letter(S0,S1),	
   zero_or_more([java_letter_or_digit],S1,S2),	
   append(Id,S2,S0),
   not_keyword(Id),
   not_literal(Id),
   zero_or_more([comment or sp],S2,SF).

not_keyword(Id):-keyword_(Id,[]),!,fail.
not_keyword(_).	

not_literal(Id):- boolean_literal(Id,[]),!,fail.
not_literal(Id):- null_literal(Id,[]),!,fail.
not_literal(_).

literal(L,S0,SF):-
  literal_(S0,S1),	
  append(L,S1,S0),
  zero_or_more([comment or sp],S1,SF).

literal_(S0,SF):-
 floating_point_literal(S0,SF),!.
literal_(S0,SF):-
 integer_literal(S0,SF),!.
literal_(S0,SF):-
 character_literal(S0,SF),!.
literal_(S0,SF):-
 string_literal(S0,SF),!.
literal_(S0,SF):-
  boolean_literal(S0,SF),!.
literal_(S0,SF):-
  null_literal(S0,SF).


digit -->
   "0"
 | non_zero_digit.

digits -->
   one_or_more([digit]).	

non_zero_digit -->
   "1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9".

decimal_numeral -->
  "0"
 | non_zero_digit,zero_or_more([digit]).
 
octal_digit -->
	"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7".

octal_numeral -->
	"0",one_or_more([octal_digit]).

hex_digit -->
	digit|"a"|"b"|"c"|"d"|"e"|"f"|"A"|"B"|"C"|"D"|"E"|"F".

hex_numeral -->
	("0x"|"0X"),one_or_more([hex_digit]).

integer_type_suffix -->
	"l"|"L".

integer_literal -->
 ( hex_numeral
 | octal_numeral
 | decimal_numeral
 ),
 !,
 zero_or_one([integer_type_suffix]).


exponent_part -->
	("e"|"E"),zero_or_one(["+" or "-"]),one_or_more([digit]).

float_type_suffix -->
	"f"|"F"|"d"|"D".

floating_point_literal -->
	digits,".",zero_or_more([digit]),
	zero_or_one([exponent_part]),
	zero_or_one([float_type_suffix]),
	 !.
floating_point_literal -->
	".",digits,
	zero_or_one([exponent_part]),
	zero_or_one([float_type_suffix]),
	 !.
floating_point_literal -->
	digits,
	exponent_part,
	zero_or_one([float_type_suffix]),
	 !.
floating_point_literal -->
	digits,
	zero_or_one([exponent_part]),
	float_type_suffix. 


unicode_input_character -->
   unicode_escape
 | raw_input_character.

unicode_escape -->
   "\\",unicode_marker,hex_digit,hex_digit,hex_digit,hex_digit.

unicode_marker --> one_or_more(["u"]).	

% FIXME
% U should be explicitely matched against unicode characters
raw_input_character([_U|H],H).

valid_input_character(C):- line_terminator(C,[]),!,fail.
valid_input_character(_).

input_character(S0,SF):-
   unicode_input_character(S0,SF),
   append(C,SF,S0),   
   valid_input_character(C).	

octal_escape -->
  "\\",
  ( ("0"|"1"|"2"|"3"),octal_digit,octal_digit
  | octal_digit,octal_digit
  | octal_digit
  ),
  !.

escape_sequence -->
 ( "\\b"	
 | "\\t"
 | "\\n"
 | "\\f"
 | "\\r"
 | "\\\""
 | "\\\'"
 | octal_escape
 | "\\\\"
 ),!.

single_character_("\'"):- !,fail.
single_character_("\\"):- !,fail.
single_character_(_).

single_character(S0,SF):-
   input_character(S0,SF),
   append(C,SF,S0),
   single_character_(C).

character_literal -->
	"'",(single_character | escape_sequence),"'".


string_character_("\""):- !,fail.
string_character_("\\"):- !,fail.
string_character_(_).

string_character(S0,SF):-
   input_character(S0,SF),
   append(C,SF,S0),
   string_character_(C).

string_literal -->
	"\"",zero_or_more([string_character or escape_sequence]),"\"".

comment -->
 ( traditional_comment
 | end_of_line_comment
 ),!.

traditional_comment -->
   "/*",comment_tail.

end_of_line_comment -->
   "//",zero_or_more([characters_in_line]).

comment_tail -->
 (  "*",comment_tail_star
 | not_star,comment_tail
 ),!.

comment_tail_star -->
 (  "/"
 | "*",comment_tail_star
 | not_star_not_slash, comment_tail
 ),!.

not_star_("*"):-!,fail.
not_star_(_).

not_star(S0,SF):-
  input_character(S0,SF),
  append(C,SF,S0),
  not_star_(C),!.
not_star(S0,SF):-
  line_terminator(S0,SF).	

not_star_not_slash_("/"):-!,fail.
not_star_not_slash_(_).

not_star_not_slash(S0,SF):-
   not_star(S0,SF),
   append(C,SF,S0),
   not_star_not_slash_(C).

characters_in_line -->
   one_or_more([input_character]).

boolean_literal-->
   "true"
 | "false".

null_literal -->
   "null".

separator(S,S0,SF) :-
   member(S, 	
    ["(",")","{","}","[","]",";",",","."]),
   append(S,S1,S0),
   zero_or_more([comment or sp],S1,SF).

brackets --> 
   separator("["),separator("]").

operator(O,S0,SF):-
   operator_(S0,S1), 	
   append(O,S1,S0),
   zero_or_more([comment or sp],S1,SF).
 
operator_ -->
 ( ">>>=" | ">>>"| ">>="| ">>"| ">="| ">"
 | "<<="| "<<"| "<="| "<"
 | "=="| "="
 | "!="| "!"
 | "/="| "/"
 | "+="|"++"| "+"
 | "-="|"--"| "-"
 | "*="| "*"
 | "&&"| "&="| "&"
 | "||"| "|="| "|"
 | "^="| "^"
 | "%="| "%"
 | "~" | "?" | ":" 
 ),!.

token(".class") -->
    ".class",zero_or_more([comment or sp]).	

           
                                                                  	
