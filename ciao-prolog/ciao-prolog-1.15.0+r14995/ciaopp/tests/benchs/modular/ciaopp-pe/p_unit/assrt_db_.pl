:- module(assrt_db_,
	[ assertion_read/9,
	  assertion_of/9,
	  assertion_body/7,
	  cleanup_assrt_db/0
	],
	[ assertions
	]).

% Documentation
:- use_module(library(assertions(assertions_props))).
:- use_module(library(assertions(c_itf_props))).

:- doc(cleanup_assrt_db,"Cleanups the database.").

cleanup_assrt_db:-
	retractall_fact(assertion_read(_,_,_,_,_,_,_,_,_)),
	retractall_fact(assertion_of(_,_,_,_,_,_,_,_,_)).

%% ---------------------------------------------------------------------------
:- pred assertion_of(Goal,M,Status,Type,Body,Dict,Source,LB,LE)
:: ( moddesc(M), assrt_status(Status), assrt_type(Type),
     assrt_body(Body), dictionary(Dict), int(LB), filename(Source),
     int(LE) )

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

:- doc(assertion_read/9,"Same as @tt{assertion_of/9} but assertions
   are already normalized and fully expanded (including module names).").

:- data assertion_read/9. 

%% ---------------------------------------------------------------------------
:- prop assertion_body(Pred,Compat,Call,Succ,Comp,Comm,Body)
   # "@var{Body} has the structure of the body of an assertion for @var{Pred} 
      with the fields @var{Compat}, @var{Call}, @var{Succ}, @var{Comp}, and
      @var{Comm}.".

assertion_body(Pred,Compat,Call,Succ,Comp,Comm,
	      (Pred::Compat:Call=>Succ+Comp#Comm)).

%% ---------------------------------------------------------------------------

:- doc(version_maintenance,dir('../version')).

%% ---------------------------------------------------------------------------
