:- module(clause_db,
	[ clause_read/7, 
	  prop_clause_read/7,
	  add_prop_clause_read/7,
	  reload_clause_read/0,
	  clause_locator/2, 
	  add_clause_locator/2,
	  literal_locator/2, 
	  source_clause/3, 
	  cleanup_clause_db/0,
	  cleanup_clause_db_code/0,
	  load_lib_props/1,
	  gen_lib_props/1,
	  cleanup_lib_props/0
	], [ assertions, basicmodes, regtypes ]).

:- use_module(program(clidtypes), [clid/1]).
:- use_module(program(clidlist_basic), [clid_of_atomid/2]).
%:- use_module(program(itf_base_db), [curr_file/2]).
:- use_module(library(messages)).

%% ---------------------------------------------------------------------------
:- doc(bug,"1. There are invalid clause-keys in calls to clause_locator.
        E.g., from entries or exports during analysis.").
:- doc(bug,"2. We should get rid of dummy clause locators.").
%% ---------------------------------------------------------------------------

:- doc(cleanup_clause_db,"Cleanups the database.").

cleanup_clause_db:-
	retractall_fact(pgm_prop_clause_read(_,_,_,_,_,_,_)),
	cleanup_clause_db_code.

cleanup_clause_db_code:-
	retractall_fact(clause_read(_,_,_,_,_,_,_)),
%	retractall_fact(pgm_prop_clause_read(_,_,_,_,_,_,_)),
	retractall_fact(source_clause(_,_,_)),
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

%% ---------------------------------------------------------------------------

:- doc(prop_clause_read/7,"Same as @tt{clause_read/7} but for the
   properties not in the current module.").

prop_clause_read(M, Head, Body, VarNames, Source, LB, LE):-
	pgm_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE).
prop_clause_read(M, Head, Body, VarNames, Source, LB, LE):-
	lib_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE).

:- data pgm_prop_clause_read/7.
:- data lib_prop_clause_read/7.


%% ---------------------------------------------------------------------------

:- pred add_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE)

# "Adds an entry for a property located in a user module (but not the
   current module).".

add_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE):-
	assertz_fact(pgm_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE)).

%% ---------------------------------------------------------------------------

:- pred reload_clause_read

# "Reloads the library properties from the library properties
   file. This should be called only after changes in the properties
   from libraries (for example, after running compile-time checks.)".

reload_clause_read:-
	display(user_error,'Not implemented yet.'),
	nl(user_error).

%% ---------------------------------------------------------------------------

:- doc(source_clause(Key,Clause,Dict),"Current module has @var{Clause}
   identified by @var{Key} with variable names @var{Dict}.").

:- data source_clause/3.

%% ---------------------------------------------------------------------------

:- data locator/2.

:- pred clause_locator(K,L) :: atm * location_t
	# "The (current module) clause identified by @var{K} is located in
           the source file around @var{L}.".

clause_locator(K,L) :- locator(K,L).
%	if(locator(K,L), true, dummy_clause_locator(L)).

:- pred add_clause_locator/2 : clid * location_t.
add_clause_locator(K, L) :- 
	( locator(K,L) -> true
	; asserta_fact(locator(K, L))
	).

:- doc(bug, "dummy_clause_locator/1 is a kludge.  A better way is
	to have locators for all clause keys. --EMM").

% dummy_clause_locator(loc(File,1,1)):- curr_file(File,_).

:- pred literal_locator/2 : clid * var => clid * location_t.

literal_locator(K,L):-
	clid_of_atomid(K,ClK),
	clause_locator(ClK, L).

%--------------------------------------------------------------------------

:- use_module(library(write), [writeq/2]).
:- use_module(library(read), [read/2]).

:- pred cleanup_lib_props

# "Cleans up all facts of lib_prop_clause_read/7 predicate.".

cleanup_lib_props:-
	retractall_fact(lib_prop_clause_read(_,_,_,_,_,_,_)).

%--------------------------------------------------------------------------

:- pred load_lib_props(Stream)

# "Loads the facts for lib_prop_clause_read/7 from the stream @var{Stream}.".

load_lib_props(Stream):-
	repeat,
	read(Stream,Fact),
	(
	    Fact = end_of_file ->
	    true
	;
	    assertz_fact(Fact),
	    fail
	).

%--------------------------------------------------------------------------

:- pred gen_lib_props(Stream)

# "Saves the facts for lib_prop_clause_read/7 to the stream @var{Stream}
  from pgm_prop_clause_read/7.".

gen_lib_props(Stream):-
	prop_clause_read(M, Head, Body, VarNames, Source, LB, LE),
	writeq(Stream,lib_prop_clause_read(M, Head, Body, VarNames, Source, LB, LE)),
	display(Stream,'.'),nl(Stream),
	fail.
gen_lib_props(_).

