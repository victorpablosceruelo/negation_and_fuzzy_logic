:- module(arg_filtering_pcpe,
	[ 
	    create_filters/2 
	],[]).


:- use_package(assertions).

:- doc(title,"Argument Filtering").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module implements filtering of ground
	redundant arguments.").


:- use_module(spec(arg_filtering), 
 	[ 
	  filter/3,
	  filter_args/2
 	]).
:- use_module(api(api_predcl), [add_defined_pred/2]).
:- use_module(program(itf_db), [current_itf/3, curr_file/2]).

:- reexport(spec('arg_filtering'),
	[
	    clean_up_filters/0,
	    filter/3	
	]).


:- pred create_filters(+Conf,+Exp) # "Asserts filter predicates for
	all code generated from @var{Confs}. Special consideration is
	taken with exported predicates contained in @var{Exp}".

create_filters([],_).
create_filters([t(A,_,_,AG)|T],Exported):-
	member(Exp,Exported),
	Exp == A,
	!,
	asserta_fact(filter(_,AG,AG)),
	create_filters(T,Exported).
create_filters([t(_,_,_,AG)|T],Exported):-
	filter_args(AG,Filter),
	asserta_fact(filter(_Id,AG,Filter)),
	add_defined(Filter),
	create_filters(T,Exported).

add_defined(Key):-
	functor( Key , F , A ),
	(current_itf( defines, F, A) ->
	    true
	;
	    curr_file(_,M),
	    add_defined_pred(Key,M)).
