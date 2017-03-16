:- module(clause_db_,
	[ clause_read/7, 
	  prop_clause_read/7,
	  clause_locator/2, 
	  add_clause_locator/2,
	  literal_locator/2, 
	  source_clause/3, 
	  cleanup_clause_db/0,
	  cleanup_clause_db_code/0
	], [ assertions, basicmodes, regtypes ]).

:- use_module(clidlist_, [clid_of_atomid/2]).
:- use_module(itf_db_, [curr_file/2]).

:- use_module(library(messages)).

:- doc(cleanup_clause_db,"Cleanups the database.").

cleanup_clause_db:-
	retractall_fact(clause_read(_,_,_,_,_,_,_)),
	retractall_fact(prop_clause_read(_,_,_,_,_,_,_)),
	retractall_fact(source_clause(_,_,_)),
	retractall_fact(locator(_,_,_,_)),
	retractall_fact(locator(_,_)).

cleanup_clause_db_code:-
	retractall_fact(clause_read(_,_,_,_,_,_,_)),
%	retractall_fact(prop_clause_read(_,_,_,_,_,_,_)),
	retractall_fact(source_clause(_,_,_)),
	retractall_fact(locator(_,_,_,_)),
	retractall_fact(locator(_,_)).

%% ---------------------------------------------------------------------------
:- doc(clause_read(M, Head, Body, VarNames, Source, LB, LE),
   "Each fact is a clause of module @var{M}.
    The clause is @var{Head:-Body} (if a directive, @var{Head} is a number,
   see @pred{c_itf:clause_of/7}). @var{VarNames} contains the names of the 
   variables of the clause. @var{Source} is the file in which the
   clause appears (treats included files correctly). @var{LB} and
   @var{LE} are the first and last line numbers in this source file in
   which the clause appears (if the source is not available or has
   not been read @var{LB}=@var{LE}=0). @var{VarNames} is not a variable, 
   and @var{Head:-Body} is fully expanded, included module names.").

:- data clause_read/7.

:- doc(prop_clause_read/7,"Same as @tt{clause_read/7} but for the
   properties not in the current module.").

:- data prop_clause_read/7.

:- doc(source_clause(Key,Clause,Dict),"Current module has @var{Clause}
   identified by @var{Key} with variable names @var{Dict}.").

:- data source_clause/3.

%% ---------------------------------------------------------------------------

:- data locator/4, locator/2.

:- pred clause_locator(K,L) :: atom * locator
	# "The (current module) clause identified by @var{K} is located in 
           the source file around @var{L}.".

clause_locator(K,L):-
	var(L),
	!,
	locate_clause_locator(K,L).
clause_locator(K,loc(S,L0,L1)):- 
	ground( (K,S,L0,L1) ),
	!,
	(
	    locator(K,S,L0,L1)
	-> 
	    true
	;
	    assertz_fact(locator(K,S,L0,L1))
	).
clause_locator(K,loc(S,L0,L1)):- 
	ground( K ),
	!,
	current_fact(locator(K,S,L0,L1)).
% clause_locator(K,Q):- 
% 	!,
% 	assertz_fact(locator(K,Q)).



add_clause_locator( K , loc(S,L0,L1) ) :- 
	ground( (K,S,L0,L1) ),
	!,
	asserta_fact( locator(K,S,L0,L1) ).

add_clause_locator( A , B ) :- 
	error_message( 
	 "Internal Error: add_clause_locator: ~w and ~w have to be ground~n" ,
	 [ A , B ] ),
	fail.




locate_clause_locator(K,loc(S,L0,L1)):-
	current_fact(locator(K,S,L0,L1)),
	!.
locate_clause_locator(K,L):-
	current_fact(locator(K,Q)),
	!,
	locate_clause_locator(Q,L).
locate_clause_locator(K,loc(File,1,1)):-
  	curr_file(File,_),
% 	error_message( 
% 	"locate_clause_locator: Internal Error: no loc found for Key ~w",
% 	[K] ),
	K = 0.

% --- for the future
%locate_clause_locator(K,_):-
%	throw(no_locator(K)).



literal_locator(K,L):-
	clid_of_atomid(K,ClK),
	locate_clause_locator(ClK,L), 
	!.
% DTM: This clause did not exist, so it has to fail
%literal_locator(K,_L):-
%	error_message(
%	"literal_locator: Internal Error: no loc found for Key ~w",
%	[K] ),
%	 fail.
% --- for the future
%literal_locator(K,_L):-
%	throw(no_locator(K)).

:- doc(doinclude,locator/1).
:- regtype locator(Id)
	# "@var{Id} is a reference for a program clause/directive of the
	  form @includedef{locator/1} that helps identifying the
	  point in the program source file where it appears.".
locator(loc(_Source,_LB,_LE)).

%% ---------------------------------------------------------------------------

:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+593,2004/07/30,21:36*39+'CEST'),
   "clause_locator still had bugs.  (David Trallero Mena)").

:- doc(version(1*0+592,2004/07/30,21:36*22+'CEST'), "Added
   add_clause_locator.  (David Trallero Mena)").

:- doc(version(1*0+543,2004/07/14,18:04*20+'CEST'), "People cannot
   live with a throw, changed it to error_message.  (David Trallero
   Mena)").

:- doc(version(1*0+499,2004/07/02,13:26*56+'CEST'), "modified
   locate_clause_locator to throw and error when needed. Also some
   cuts added because empty solutions were given.

    (David Trallero Mena)").

:- doc(version(1*0+355,2004/02/25,19:16*02+'CET'), "Added
   cleanup_clause_db_code/0 to cleanup module state except properties.
   (Jesus Correas Fernandez)").

