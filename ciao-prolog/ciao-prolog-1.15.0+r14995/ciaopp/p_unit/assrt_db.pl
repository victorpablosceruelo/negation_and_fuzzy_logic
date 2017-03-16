:- module(assrt_db,
	[ assertion_read/9,
	  add_assertion_read/9,
	  remove_assertion_read/9,
	  removeall_assertion_read/9,
	  ref_assertion_read/10,
	  reload_assertion_read/0,
	  assertion_of/9,
	  assertion_body/7,
	  cleanup_assrt_db/0,
	  load_lib_assrt/1,
	  gen_lib_assrt/1,
	  cleanup_lib_assrt/0
	],
	[ assertions
	]).

% Documentation
:- use_module(library(assertions(assertions_props))).
:- use_module(library(assertions(c_itf_props))).

:- doc(cleanup_assrt_db,"Cleanups the database.").

cleanup_assrt_db:-
	retractall_fact(pgm_assertion_read(_,_,_,_,_,_,_,_,_)),
	retractall_fact(assertion_of(_,_,_,_,_,_,_,_,_)).

%% ---------------------------------------------------------------------------
:- pred assertion_of(Goal,M,Status,Type,Body,Dict,Source,LB,LE)
:: ( moddesc(M), assrt_status(Status), assrt_type(Type),
     assrt_body(Body), dictionary(Dict), int(LB), filename(Source),
     int(LE) ) + no_rtcheck

# "Each fact represents an assertion for @var{Goal} 
   in module @var{M}, which has status @var{Status} and is of type
   @var{Type}.  @var{Body} is the actual body of the
   assertion. @var{Dict} contains the names of the variables which
   appear in the assertion. @var{Source} is the file in which the
   assertion appears (treats included files correctly). @var{LB} and
   @var{LE} are the first and last line numbers in this source file in
   which the assertion appears (if the source is not available or has
   not been read @var{LB}=@var{LE}=0).  @var{Goal} is always
   a term of the same functor and arity as the predicate it represents
   (i.e., it is not in Functor/Arity format). It may be normalized
   or not, i.e., it may contain modes or properties in its arguments, 
   depending on the
   normalizations options (see @pred{opts/1}). @var{Body} is always
   normalized, but the properties or property conjunctions inside may
   not -- see @pred{normalize_assertions_pass_one/1} and
   @pred{normalize_assertions_pass_two/1} in @lib{assrt_norm}.".

:- data assertion_of/9. 

%% ---------------------------------------------------------------------------

:- doc(assertion_read/9,"Same as @tt{assertion_of/9} but assertions
   are already normalized and fully expanded (including module names).").

assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
	pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE).
assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
	lib_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE).

:- data pgm_assertion_read/9. 
:- data lib_assertion_read/9. 

%% ---------------------------------------------------------------------------

:- pred add_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)

# "Adds an entry for an assertion located in a preprocessing unit
   module (but not in library modules).".

add_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
	assertz_fact(pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).

%% ---------------------------------------------------------------------------

:- pred remove_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)

# "Removes an entry for an assertion located in a preprocessing unit
   module (but not from library modules).".

remove_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
	retract_fact(pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).
% remove_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
% 	retract_fact(lib_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).

%% ---------------------------------------------------------------------------

:- pred removeall_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)

# "Removes all entries matching arguments for assertions located in a
   preprocessing unit module (but not from library modules).".

removeall_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
	retractall_fact(pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).
% removeall_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE):-
% 	retractall_fact(lib_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE)).

%% ---------------------------------------------------------------------------

:- pred ref_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE,Ref)

# "Erases an entry for an assertion for which we have its reference
  @var{Ref}.  Warning: this predicate is very dangerous and error
  prone.  It must be used with care.".

:- doc(bug, "This predicate is very dangerous and error prone.").

ref_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE,Ref):-
	current_fact(pgm_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE),Ref).
%%%%%%%	!.
ref_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE,Ref):-
	current_fact(lib_assertion_read(Goal,M,Status,Type,Body,Dict,Source,LB,LE),Ref).

%% ---------------------------------------------------------------------------

:- pred reload_assertion_read

# "Reloads the library assertions from the library assertions file. This should be
   called only after changes in the assertions or properties from libraries (for
   example, after running compile-time checks.)".

reload_assertion_read:-
	display(user_error,'Not implemented yet.'),
	nl(user_error).

%% ---------------------------------------------------------------------------
:- prop assertion_body(Pred,Compat,Call,Succ,Comp,Comm,Body)
   # "@var{Body} has the structure of the body of an assertion for @var{Pred} 
      with the fields @var{Compat}, @var{Call}, @var{Succ}, @var{Comp}, and
      @var{Comm}.".

assertion_body(Pred,Compat,Call,Succ,Comp,Comm,
	      (Pred::Compat:Call=>Succ+Comp#Comm)).

%% ---------------------------------------------------------------------------

:- use_module(library(write), [writeq/2]).
:- use_module(library(read), [read/2]).

:- pred load_lib_assrt(Stream)

# "Loads the facts for lib_assertion_read/9 from the stream @var{Stream}.".

load_lib_assrt(Stream):-
	repeat,
	read(Stream,Fact),
	(
	    Fact = end_of_file ->
	    !
	;
	    assertz_fact(Fact),
	    fail
	).

%% ---------------------------------------------------------------------------

:- pred cleanup_lib_assrt

# "Cleans up all facts of lib_assertion_read/9.".

cleanup_lib_assrt:-
	retractall_fact(lib_assertion_read(_,_,_,_,_,_,_,_,_)).

%% ---------------------------------------------------------------------------

:- pred gen_lib_assrt(Stream)

# "Saves the facts for lib_assertion_read/9 to the stream @var{Stream}
  from pgm_assertion_read/9.".

gen_lib_assrt(Stream):-
	assertion_read(PD,M,Status,Type,Body,Dict,S,LB,LE),
	assertion_body(Pred,Compat,Call,Succ,Comp,_Comm,Body),
	assertion_body(Pred,Compat,Call,Succ,Comp,"",Body1), %no comment is stored.
	writeq(Stream,lib_assertion_read(PD,M,Status,Type,Body1,Dict,S,LB,LE)),
	display(Stream,'.'),nl(Stream),
	fail.
gen_lib_assrt(_).
