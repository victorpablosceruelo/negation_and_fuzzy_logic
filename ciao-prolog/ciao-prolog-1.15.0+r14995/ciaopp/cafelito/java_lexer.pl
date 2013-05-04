:- module(_,
	   [
	     identifier/3,
             keyword/3,               
             literal/3,
	     separator/3,
	     operator/3,
	     comments_and_spaces/2
            ],
            [dcg,fsyntax,hiord]).
:- use_module(library(lists)).

% avoid nasty issues
:- set_prolog_flag(write_strings,on).
:- set_prolog_flag(multi_arity_warnings,off).


:- op(200,xfy,['or']).

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
match_el(X,(S0,L0),(S1,L0)):-
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


% Redefining of 'C' so we can pass line numbers in the hidden pars
:- redefining('C'/3).
'C'((S0,L0), X, (S,L)) :-
	!,
	term_basic:'C'(S0, X, S),
	( X = 0'\n -> 
	     L is L0 + 1 ;
	     L  = L0 
        ).
'C'(S0, X, S) :-
	term_basic:'C'(S0, X, S).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                      TOKENS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% nothing -->"".

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
% opt_sp  --> zero_or_more([white]).

comments_and_spaces -->
  zero_or_more([comment or sp]).

keyword(K,P0,PF) :-
  P0 = (S0,L0),
  keyword_(S0,S1),
  append(K,S1,S0),
  P1 = (S1,L0),
  sp(P1,P2),!,
  zero_or_more([comment or sp],P2,PF).
keyword(K,P0,PF) :-
  P0 = (S0,L0),
  keyword_(S0,S1),
  append(K,S1,S0),
  P1 = (S1,L0),
  separator(_,P1,_),
  PF = P1.
% the only exception to the rule [sic]
keyword("default",P0,PF) :-
  P0 = (S0,L0),
  keyword_(S0,S1),
  append("default",S1,S0),
  P1 = (S1,L0),
  operator(':',P1,_),
  PF = P1.


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
  |"try"       | "void"      | "volatile" |"while"
% CML begin
  |"check"     | "trust"     | "requires" |"ensures"      |"entry"  | "comp" 
% CML end
   .


java_letter([X|SF],SF) :- X >= 0'a, X =< 0'z, !.
java_letter([X|SF],SF) :- X >= 0'A, X =< 0'Z, !.
java_letter([X|SF],SF) :- X = 0'$,!.
java_letter([X|SF],SF) :- X = 0'_,!.
java_digit([X|SF],SF)  :- X >= 0'0, X =< 0'9.
java_letter_or_digit -->
   java_letter
 | java_digit.	


identifier(id(Id_Atom),P0,PF) :-
   P0 = (S0,L0),
   java_letter(S0,S1),	
   zero_or_more([java_letter_or_digit],S1,S2),	
   append(Id,S2,S0),
   not_keyword(Id),
   not_literal(Id),
   name(Id_Atom,Id),
   zero_or_more([comment or sp],S2,SF),
   PF = (SF,L0).

not_keyword(Id):-keyword_(Id,[]),!,fail.
not_keyword(_).	

not_literal(Id):- boolean_literal(Id,[]),!,fail.
not_literal(Id):- null_literal(Id,[]),!,fail.
not_literal(_).

literal(Literal,P0,PF):-
  P0 = (S0,L0),
  literal_(Type,S0,S1),	
  append(Value,S1,S0),
  name(Atom,Value),
  Literal = literal(Type,Atom),
  zero_or_more([comment or sp],S1,SF),
  PF = (SF,L0).

literal_(float,S0,SF):-
 floating_point_literal(S0,SF),!.
literal_(int,S0,SF):-
 integer_literal(S0,SF),!.
literal_(char,S0,SF):-
 character_literal(S0,SF),!.
literal_(string,S0,SF):-
 string_literal(S0,SF),!.
literal_(boolean,S0,SF):-
  boolean_literal(S0,SF),!.
literal_(null,S0,SF):-
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

unicode_marker --> one_or_more([unicode_prefix]).	

unicode_prefix --> "u".

% FIXME
% U should be explicitely matched against unicode characters
raw_input_character([_U|H],H).

valid_input_character(C):- line_terminator(C,[]),!,fail.
valid_input_character(_).

input_character((S0,L0),(SF,L0)):-
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

single_character((S0,L0),(SF,L0)):-
   input_character((S0,L0),(SF,L0)),
   append(C,SF,S0),
   single_character_(C).

character_literal -->
   "'",(single_character | escape_sequence),"'".

string_character_("\""):- !,fail.
string_character_("\\"):- !,fail.
string_character_(_).

string_character((S0,L0),(SF,L1)):-
   input_character((S0,L0),(SF,L1)),
   append(C,SF,S0),
   string_character_(C).

string_literal -->
	"\"",zero_or_more([string_character or escape_sequence]),"\"".

comment -->
 (
%%%% begin CML addition
   cml_comment,!,
    {
     fail
    }
 | ats
%%%% end CML addition
 | traditional_comment
 | end_of_line_comment
 ),!.

%%%% begin CML addition
cml_comment -->
   "/*@",cml_comment_tail.

cml_comment_tail -->
 (  "@",cml_comment_tail_star
 | one_or_more([not_at]),cml_comment_tail
 ),!.

cml_comment_tail_star -->
 (  "*/"
 | "@",cml_comment_tail_star
 | one_or_more([not_at_not_slash]), cml_comment_tail
 ),!.

not_at((S0,L0),(SF,L1)):-
  input_character((S0,L0),(SF,L1)),
  append(C,SF,S0),
  not_at_(C),!.
not_at((S0,L0),(SF,L1)):-
  line_terminator((S0,L0),(SF,L1)).	

not_at_("@"):-!,fail.
not_at_(_).

not_at_not_slash((S0,L0),(SF,L1)):-
   not_at((S0,L0),(SF,L1)),
   append(C,SF,S0),
   not_at_not_slash_(C).

not_at_not_slash_("/"):-!,fail.
not_at_not_slash_(_).

ats -->
   one_or_more([at]).

at --> "@".

%%%% end CML addition

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

not_star((S0,L0),(SF,L1)):-
  input_character((S0,L0),(SF,L1)),
  append(C,SF,S0),
  not_star_(C),!.
not_star((S0,L0),(SF,L1)):-
  line_terminator((S0,L0),(SF,L1)).	

not_star_not_slash_("/"):-!,fail.
not_star_not_slash_(_).

not_star_not_slash((S0,L0),(SF,L1)):-
   not_star((S0,L0),(SF,L1)),
   append(C,SF,S0),
   not_star_not_slash_(C).

characters_in_line -->
   one_or_more([input_character]).

boolean_literal-->
   "true"
 | "false".

null_literal -->
   "null".

separator(S,(S0,L0),(SF,L1)) :-
   member(S, 	
    ["(",")","{","}","[","]",";",",","."]),!,
   append(S,S1,S0),
   zero_or_more([comment or sp],(S1,L0),(SF,L1)).
%%%% begin CML addition
separator(S,(S0,L0),(SF,L1)) :-
   member(S, 	
    ["/*@","*/"]),!,
   append(S,S1,S0),
   zero_or_more([comment or sp],(S1,L0),(SF,L1)).
%%%% end CML addition


operator(O,(S0,L0),(SF,L1)):-
   operator_(S0,S1),
   append(O_String,S1,S0),
   name(O,O_String),
   zero_or_more([comment or sp],(S1,L0),(SF,L1)).

operator_-->
 ( ">>>="
 | ">>>" 
 | ">>=" 
 | ">>"  
 | ">="  
 | ">"   
 | "<<=" 
 | "<<"  
 | "<="  
 | "<"   
 | "=="  
 | "="   
 | "!="  
 | "!"   
 | "/="  
 | "/"   
 | "+="  
 | "++"  
 | "+"   
 | "-="  
 | "--"  
 | "-"   
 | "*="  
 | "*"   
 | "&&"  
 | "&="  
 | "&"   
 | "||"  
 | "|="  
 | "|"  
 | "^=" 
 | "^" 
 | "%="
 | "%" 
 | "~" 
 | "?" 
 | ":" 
 ),!.

token(".class") -->
    ".class",zero_or_more([comment or sp]).
