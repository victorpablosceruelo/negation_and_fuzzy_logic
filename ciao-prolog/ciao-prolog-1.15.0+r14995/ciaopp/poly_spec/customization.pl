:- module(customization,
	[
	    get_all_options/3,
	    global/2,
	    local/2,
	    default_depth_lim/1
	],[]).

:- use_package(hiord).
:- use_package(assertions).

:- doc(title,"Customization of Poly-Controlled Partial Deduction").

:- doc(author, "Germ@'{a}n Puebla").

:- doc(module," This module contains the required predicates for
	customizing poly-controlled partial deduction."). 

:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(lists), [length/2]).


:- pred get_all_options(G,U,D) : ( term(G), term(U), term(D)) => (
	list(G), list(U), int(D)) # "Returns all options used to
	customize the behaviour of pcpe".


get_all_options(G,U,D_Lim):-
	global_controls(G),
	local_controls(U),
	depth_limit(D_Lim).

:- pred global_controls(G) : term => list
	# "Returns the set @var{G} of global controls, coded as numbers
         (to save memory)".

global_controls(G):-
	current_pp_flag(poly_global_control,Globals),
	length(Globals,Len),
	enumeration(1,Len,G).

:- pred local_controls(U) : term => list
	# "Returns the set @var{U} of local controls, coded as numbers
         (to save memory)".

local_controls(U):-
	current_pp_flag(poly_local_control,Locals),
	length(Locals,Len),
	enumeration(1,Len,U).

:- pred global(Ind,G) : int * term => int * list
	#"Given a code @var{Ind}, returns the corresponding global
         control rule @var{G}".

global(Ind,G):-
	num(Ind),!,
	current_pp_flag(poly_global_control,Globals),
	elementAt(Ind,Globals,G).
global(G,G).

:- pred local(Ind,L) : int * term => int * list
	#"Given a code @var{Ind}, returns the corresponding local
         control rule @var{L}".

local(Ind,L):-
	num(Ind),!,
	current_pp_flag(poly_local_control,Locals),
	elementAt(Ind,Locals,L).
local(L,L).

:- pred enumeration(Beg,End,Enum) 
	: int * int * term => int * int * list
	# "Returns an enumeration @var{Enum} from @tt{Beg} to
         @var{End}".

enumeration(N,N,[N]).
enumeration(B,N,[B|NT]):-
	B1 is B+1,
	enumeration(B1,N,NT).

elementAt(1,[H|_T],H):-!.
elementAt(N,[_H|T],H):-
	N>1,!,
	N1 is N-1,
	elementAt(N1,T,H).

:- pred default_depth_lim(D) : term => int #"Indicates the default
	depth @var{D} in which configurations are closed, evaluated
	and pruned, in cases where this flag has not been set and
	pruning is mandatory, such as in branch and bound techinques".

default_depth_lim(3).

:- pred depth_limit(D) : term => int
	#"Indicates depth in which configurations are closed,
	evaluated and pruned".

depth_limit(D):-  current_pp_flag(poly_depth_lim,D).

