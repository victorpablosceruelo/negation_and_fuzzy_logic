:- module( rtchecks_stack , [
			 push_parent_goal/1,
			 current_parent_goal/2,
			 pop_parent_goal/1,
			 clean_parent_stack/1,
			 show_stack/1
		      ] , [assertions, regtypes] ).


:- use_package( library( 'ciaopp/api/ciaopp_api'   ) ).
:- use_module(library(ciaopp(api(xtchecks_msg)))).

:- data parent_goal/2.

:- meta_predicate push_parent_goal( addmodule ).
:- meta_predicate current_parent_goal( addmodule ).
:- meta_predicate pop_parent_goal( addmodule ).

:- trust pred push_parent_goal( Term , M ) : term * atm

# "Push @var{Term} in the @var{M} module stack.".

push_parent_goal( Term , M ) :-
	asserta_fact( parent_goal( M , Term ) ).
push_parent_goal( Term , M ) :-
	retract_one_fact( parent_goal( M , Term ) ),
	fail.

:- trust pred current_parent_goal( M , Term ) : atm * term

# "@var{Term} is the top of the module @var{M} stack.".

current_parent_goal( M , Term ) :-
	current_fact( parent_goal( M , Term ) ),
	!.
current_parent_goal( _ , unknown ).

:- trust pred pop_parent_goal( Term , M ) : term * atm

# "The top of the module @var{M} stack is removed. @var{Term} is used
  in case of fail.".

pop_parent_goal( Term , M ) :-
	retract_one_fact( parent_goal( M , Term ) ).

pop_parent_goal( Term , M ) :-
	asserta_fact( parent_goal( M , Term ) ),
	fail.

retract_one_fact( Fact ) :-
	retract_fact( Fact ),
	!.

:- trust pred clean_parent_stack( M ) : atm

# "Cleans the module @var{M} stack.".

clean_parent_stack( M ) :-
	retractall_fact( parent_goal( M , _ ) ),
	!.
clean_parent_stack( _ ).


:- trust pred clean_parent_stack( M ) : atm

# "Shows the call stack based in asserted fact of
  @pred{parent_goal/2}.".

show_stack( M ) :-
	current_parent_goal( M , Goal ),
	note_message( "Call to ~w" , [ Goal ] ),
	fail.
show_stack( _ ).
