:- module(_,_,[assertions,nativeprops]).

:- use_module(library(read_from_string), [read_from_string_atmvars/2]).
:- use_module(ilciao(java_interface), 
	[
	    java_create/2,
	    java_invoke/2
	]).    
:- use_module(library(messages)).

:- doc(bug,"parse_resource_function/2 and parse_size_function only work
                with prefix expressions in general form.").

get_java_resources(Resources):-
	java_create('soot.ciao.CiaoInterface',CiaoInterface),
	java_invoke(CiaoInterface,getResources(ResourcesAsString)),
	read_from_string_atmvars(ResourcesAsString,Resources),
	warning_message_if_no_resources(Resources),
	!.
get_java_resources([]):-
	warning_message("No resources are defined by the user").

get_java_cost(size,AtomId,Size):-
	java_create('soot.ciao.CiaoInterface',CiaoInterface),
	java_invoke(CiaoInterface,getCost('SIZE',AtomId,Size0)),
	parse_size_function(Size0,Size),
	!.
get_java_cost(Resource,AtomId,Cost):-
	java_create('soot.ciao.CiaoInterface',CiaoInterface),
	java_invoke(CiaoInterface,getCost(Resource,AtomId,Cost0)),
	parse_resource_usage_function(Cost0,Cost),
	!.
get_java_cost(_ResourceFactory,_AtomId,error).
	
parse_resource_usage_function("inf",inf):-!.
parse_resource_usage_function(S,N):-
 	number_codes(N,S),!.
parse_resource_usage_function(S,GenExp):-
	read_from_string_atmvars(S,GenExp).
parse_resource_usage_function(S,_):-
	error_message("The resource usage function ~q could not be parsed",[S]),!,fail.

parse_size_function(S,Sizes):-
	read_from_string_atmvars(S,GenExp),
	parse_sizes(GenExp,Sizes).
parse_size_function(S,_):-
	error_message("The size function ~q could not be parsed",[S]),!,fail.
parse_sizes([],[]).
parse_sizes([void|Rs],[inf|Zs]):-
	parse_sizes(Rs,Zs).
parse_sizes([R|Rs],[R|Zs]):-
	parse_sizes(Rs,Zs).

warning_message_if_no_resources(Resources):-
	Resources == [],
	warning_message("No resources are defined by the user").
warning_message_if_no_resources(_).

% parse_expression([],[]).
% parse_expression(S,[T|Rs]):-
% 	read_from_string_atmvars(S,T,R),
% 	parse_expression(R,Rs).
	
%string_to_expr("4 * (size(arg(1)) + size(arg(2))) * size(ret)",Res).	


% Expr  := n | size(n) | (Expr BinOp Expr) | exp(Expr,n) | log(Expr,n)	
% BinOp := + | - | * | /


% delimitator(32). % space

% parse_expression([],[]).
% parse_expression([S|Ss],Es):-
% 	delimitator(S),!,
% 	parse_expression(Ss,Es).
% parse_expression([S|Ss],Es):-
% 	number(S),
% 	parse_expression(Ss,Es).
	



