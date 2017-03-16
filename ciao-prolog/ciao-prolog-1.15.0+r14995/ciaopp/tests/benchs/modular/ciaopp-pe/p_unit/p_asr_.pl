:- module(p_asr_,
	[ set_libs/2,
	  preprocessing_unit/3,
	  preprocessing_unit_opts/4,
	  do_cache/0,
%jcf-begin
	  cleanup_code_and_related_assertions/0,
	  delayed_checks/0,
	  user_module/1,
	  activate_second_translation/2,
	  save_exported_assertions_of/2,
	  save_relevant_properties_of/2,
	  deactivate_second_translation/2,
	  file_path/2,
%	  save_clauses_of/2,
%	  save_assertions_of/2,
	  save_itf_of/2,
%jcf-end
	  show_asr/1

	],
	[ 
	  assertions, 
	  basicmodes, 
	  regtypes,
	  .('../api/ciaopp_api_')
%	  show_trans
	]).


% Documentation
:- use_module(library(assertions(c_itf_props))).

:- use_module(p_canonical_).


%% :- doc(doinclude,assertion_read/9).
%% :- doc(doinclude,clause_read/7).

:- doc(title,"Assertion processing library").

:- doc(author,"Manuel Hermenegildo").
:- doc(author,"Francisco Bueno").

:- doc(module,"

This library processes the @tt{.asr} files @cindex{.asr files},
which are a cache of the assertions relevant to the exported predicates of a
file. To be able to correctly interpret such assertions, the definitions of
the exported and local properties transitively used by the
exported properties are also cached. This information forms the @em{assertion
interface} @cindex{assertion interface} of the file.

Currently, @tt{.asr} files have the assertions for exported predicates and
all property definitions plus all assertions related to properties. This is
a superset of the assertion interface, but it is easier to treat.

For the purpose of preprocessing the current module, more than its assertion
interface is required. In this case, the @em{preprocessing unit} of
the file should be gathered together. @cindex{preprocessing unit} 
Currently, a superset of the preprocessing unit is put together by this
module. It is made up of the current module source, the assertions for
predicates exported by the related files, the properties defined in the
related files and their assertions, and the assertions for exported predicates,
the properties and their assertions in files transitively imported by the
related files, up to a file that does not export any property.
Note that this is a superset of the
preprocessing unit, since not all such properties may be needed to
interpret the assertions imported by the current module (and the assertions
for predicates exported by non-related files are useless).

The data collected by the predicates exported by this library is asserted in
different modules. If you want to have access to it, you may want to consider
importing libraries @lib{ciaopp/p_unit}, @lib{ciaopp/p_unit/itf_db}, 
@lib{ciaopp/p_unit/assrt_db}, or @lib{ciaopp/p_unit/clause_db}.

").

%% The preprocessing unit is made up of the file code, the assertion
%% interfaces of modules imported (which are called the @em{related
%% files}), and the definitions of the exported properties and of all
%% local properties transitively used by the exported properties for
%% files which export a property transitively used by one of the exported
%% properties of the related files.

:- doc(bug,"1. Should expand module qualifications in the
   relevant directives: initialization, on_abort, ... (multifile, dynamic,
    data, and meta_predicate are handled via itf_db.").

:- doc(bug,"2. Should go into higher order properties and check the
   arguments for import/export also (and should probably look at the
   meta-predicate declarations for these)?").

:- doc(bug,"3. Opaque properties are not handled yet.").

:- doc(bug,"4. Save assertion heads WITH modes (non-normalized) and 
   normalize them only when asserting in the database.").

:- doc(bug,"5. No way of expanding when reading the .asr. Currently
   not reading them!").

:- doc(bug,"6. Add support for something like the ciaoc -u option.").

%% :- doc(bug,"7. Several copies of the same assertions remain in DB.").

:- doc(bug,"8. Have to cleanup code: reduce asserts.").

%% Solved:
%% :- doc(bug,"9. Currently, if a related file does not export a property
%%    the transitive closure from this file does NOT occur: this is not
%%    correct. Now solved.").
%% :- doc(bug,"10. Implicit importation of builtin modules from another 
%% 	builtin module does not work: properties are not read in. This is 
%% 	relevant when using package pure for properties callable/1 and iso/1 in
%% 	basiccontrol. This was part of the previous bug: now solved.").

%% Solved with set_ciaopp_expansion(true)
%% :- doc(bug,"11. Things like this won't work:
%%    current_itf(imports,,(rt_module_exp(_483,fact,mmatrixpaco,-,fail,_488),
%%                          set_fact(_488)),_197)
%%    .").

:- doc(bug,"12. Should properties be defined only in terms of other
	properties? Currently, if this is not the case, predicates used
        in the definition of properties are not cached.").

:- doc(bug,"13. The modedef of parametric modules that may appear in the
	output will be wrong since call/2 is module expanded for the current
        module instead of for the proper hiord_rt:call/2.").

:- doc(bug,"14. When saving the assertions of dynamic.pl:
        WARNING: (lns 343-345) Predicate current_predicate/1 undefined 
        in source").

% :- doc( bug , "15. When loading an user file (no module
%    declaration), the error:
%    call filenames:basename(prelude.pl,user(/usr/cvs/Benchmarks/ciaopp/types/headunify))
%    ?  {ERROR: atomic_basic:atom_codes/2, arg 1 - expected atom, found
%    user(...)} appears" ).

% ===========================================================================

% ISO-Prolog compatibility libraries
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(dynamic), [assertz/1, retract/1]).  

% Other libraries
:- use_module(library(compiler(c_itf)), 
	[activate_translation/3,clause_of/7,cleanup_c_itf_data/0,
	 comp_defines/1,def_multifile/4,defines/5,defines_module/2,
	 exports/5,false/1,imports_pred/7,location/3,
	 module_error/0,module_error/1,
	 module_expansion/9,meta_args/2,process_file/7]).
:- use_module(library(compiler(translation)), 
	[expand_clause/6,del_goal_trans/1,del_clause_trans/1]).
:- use_module(library(ctrlcclean), 
	[ctrlc_clean/1,delete_on_ctrlc/2,ctrlcclean/0]).
:- use_module(library(errhandle), [error_protect/1]).  
:- use_module(library(fastrw), [fast_read/1, fast_write/1]).
:- use_module(library(formulae), [list_to_conj/2, conj_to_list/2]).
:- use_module(library(messages)).  
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(read), [read/1]).
%% :- use_module(library(system),
%% 	[fmode/2,chmod/2,file_exists/1,file_exists/2,delete_file/1]).
:- use_module(library(system), [working_directory/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(filenames), [
	                                no_path_file_name/2,
					basename/2
				      ] ).

% Own libraries
:- use_module(assrt_db_).
:- use_module(assrt_norm_).
:- use_module(clause_db_).
:- use_module(itf_db_, [assert_itf/5, cleanup_itf_db/0]).

% asr files
:- use_module(aux_filenames_, [get_module_filename/3]).
:- use_module(library(system), [modif_time/2]).

%% ---------------------------------------------------------------------------
:- pred asr_version(atm) 
# "Contains a version number which identifies
   the @tt{.asr} files associated with this version of the assertion
   library. Should be changed every time changes are made which render
   the @tt{.asr} files incompatible, since this forces recomputation
   of all such files.".

asr_version('5.0').

%% ---------------------------------------------------------------------------
:- pred set_libs(go(OldLibs),go(NewLibs)) :: list(atom) * list(atom) 
# "The arguments contain library directories that will be used to
   locate files used as modules, syntax files, or which appear in
   include declarations. @var{OldLibs} is the current set of libraries
   and @var{NewLibs} the new one.".

:- multifile library_directory/1.
:- dynamic library_directory/1.

set_libs(OldLibs,Libs) :-
	findall(D,retract(library_directory(D)),OldLibs),
	set_lib_dirs(Libs).

set_lib_dirs([]).
set_lib_dirs([H|T]) :- 
	assertz(library_directory(H)),
	set_lib_dirs(T).

%% ---------------------------------------------------------------------------
:- pred cleanup_code_and_related_assertions/0
# "Cleans up data asserted by assertion/code reader/normalizer.".

cleanup_code_and_related_assertions :-
        cleanup_c_itf_data,
        cleanup_itf_db,
	cleanup_clause_db,
	cleanup_assrt_db.

%% ---------------------------------------------------------------------------
:- pred preprocessing_unit(in(I),out(M),out(E)) :: filename * moddesc * switch
# "This is the main entry point to the @concept{assertion reader/normalizer}. 
   Reads all declarations and code in @var{I} and leaves everything asserted 
   in the database. Clauses are stored in @pred{clause_db:clause_read/7}.
   Assertions are normalized and stored in @pred{assrt_db:assertion_read/9}.
   Everything indexed on @var{M}, the module defined in @var{I}.

   Also, it reads and normalizes assertions @em{of the exported predicates}
   in all files related to @var{I} (i.e., imported by it, directly or by
   reexportation), leaving them also asserted in 
   @pred{assrt_db:assertion_read/9} facts. All local property definitions 
   which are transitively used by the exported properties of the related files 
   are also stored in @pred{clause_db:prop_clause_read/7} facts. If up to
   date @tt{.asr} files exist
   for any of the related files, the information is read directly from such
   @tt{.asr} files. @cindex{.asr files} Otherwise, the @tt{.pl} file
   is read and an up to date @tt{.asr} file is generated.

   The same processing of the previous paragraph is done also for files which
   export a property transitively used by one of the exported properties of
   the related files.

   Since this predicate is intended for gathering file information for
   purposes which can be other than compilation to executable code
   (e.g., generating documentation or in the preprocessor) this
   predicate catches errors and proceeds in cases where file
   processing (e.g., during actual compilation) might normally abort.".

:- pred preprocessing_unit(in(Is),out(Ms),out(E)) :: list(filename) * list(moddesc) * switch
# "The same as in the previous case, but considering a list of file
  names in @var{Is} and @var{Ms}.".

:- pred preprocessing_unit_opts(in(I),in(Opts),out(M)) 
:: filename * list(atom) * moddesc
# "Version which accepts some options in @var{Opts}. In particular,
   @tt{'-v'} produces verbose output for debugging. Also passes
   on the options in @var{Opts} to pass two of the assertion normalizer.".

preprocessing_unit(Is,Ms,E):- 
	% DTM : This line is used for debugging
%	preprocessing_unit_opts(Is,['-v'],Ms,E).
	preprocessing_unit_opts(Is,[],Ms,E).


preprocessing_unit_opts(I,Opts,M,E):-
	cleanup_code_and_related_assertions,
	(  ( prolog_flag(verbose_compilation,on,on) ; member('-v',Opts) )
	-> Verb = verbose
	;  ( member('-d',Opts)
	   -> Verb = debug
	   ;  Verb = quiet ) ),
	% init related files db for the closure
	clean_up_facts,

%	read_cache,
	
	% process main file
	(
	    I = [_|_]
	->
   	    process_main_files(  I  , Opts ,  M  , Verb )
	;
 	    process_main_files( [I] , Opts , [M] , Verb )
	),
	% traverse the related files closure
	related_files_closure(direct,Verb,Opts),
	% check for props in the related files
%	ver_esp_asr,
	delayed_checks,
	% any error upon loading?
	there_was_error(E).



% ver_esp_asr :-
% 	display( 'imprimiendo la prueba\n\n' ),
% 	current_fact(
% 			assertion_read(PD,_AM,_Status,prop,_Body,_Dict,_S,_LB,_LE) ),
% 	display( PD ) , nl,
% 	fail.
%
% ver_esp_asr :-
% 	display( '\n\nimprimiendo los irrelevantes\n\n' ),
% 	current_fact( irrelevant_file(PD) ),
% 	display( PD ) , nl,
% 	fail.
% ver_esp_asr.






process_main_files( [] , _Opts,[],_Verb ).
process_main_files( [ I | IL ] , Opts , [ M | ML ] , Verb ) :-
        error_protect(ctrlc_clean(
		process_file(I, asr, any, 
		                process_main_file(M,Verb,Opts), 
                                false, false, do_nothing )
				 )),
	process_main_files(IL,Opts,ML,Verb).




clean_up_facts :-
	retractall_fact( processed_file(_)  ),
	retractall_fact( related_file(_)    ),
	retractall_fact( irrelevant_file(_) ).




there_was_error(yes):- module_error, !.
there_was_error(yes):- module_error(_), !.
there_was_error(no).


%% ---------------------------------------------------------------------------
%% Main file (current module) processing
%% ---------------------------------------------------------------------------


%% this file have to assert related_file fact to be processed later.

process_main_file(Base,M,Verb,Opts):- 
        verb_message(Verb, '{Processing main module ' ),

	defines_module(Base,M),

	assertz_fact( processed_file( Base ) ),

	assert_itf(defines_module,M,_,_,Base),

	%% forces generation of defines/5 data (used below)
	c_itf:comp_defines(Base),

	%% (can not!) checks that properties are identifiable
	normalize_assertions(M,Base,Opts),
	
        %% save clauses, assertions, and itf (everything expanded)
        activate_second_translation(Base,M),

	% treat assertions
	get_assertions_of( _ , M , Assrt ),
	compound_to_simple_assrt( Assrt , NAssrt ),

	% Save orignal pred assertions
	comment_original_pred_assertions( Assrt ),

	% Add clauses to DB
	assert_clauses_of(Base,M),

	% Add assertions to DB
	add_assertions( NAssrt ),

	% add ift facts to DB
	save_itf_of(Base,M),

        deactivate_second_translation( Base , M ),

	%% initialize the (directly) related files of Base
	assert_related_files_( Base , Verb ),
        verb_message( Verb , '}' ).




save_itf_of(Base,M):-
	defines(Base,F,A,DefType,Meta),
	assert_itf(defines,M,F,A,M),
	save_meta_dynamic(Meta,DefType,M,F,A),
	fail.
save_itf_of(Base,M):-
	imports_pred(Base,IM,F,A,DefType,Meta,_EndFile),
	assert_itf(imports,M,F,A,IM),
	save_meta_dynamic(Meta,DefType,M,F,A),
	fail.
save_itf_of(Base,M):-
	exports(Base,F,A,_DefType,_Meta),
	assert_itf(exports,M,F,A,M),
	fail.
save_itf_of(Base,M):-
	def_multifile(Base,F,A,DynType),
	assert_itf(multifile,M,F,A,DynType),
	fail.
save_itf_of(_Base,_M).

save_meta_dynamic(Meta,DefType,M,F,A):-
	( Meta\==0
	-> assert_itf(meta,M,F,A,Meta)
         ; true ),
	( ( DefType=dynamic ; DefType=data ; DefType=concurrent )
	-> assert_itf(dynamic,M,F,A,DefType)
         ; true ).




:- pred comment_original_pred_assertions( A )
	: list( A , as )

# "Look for pred assertions into @var{A} and add them to commented
  assertions DB. This is necessary only for the output.".


comment_original_pred_assertions( [] ).

comment_original_pred_assertions( [ A | As ] ) :-
	A = as${ type => pred },
	!,
	add_commented_assertion( A ),
	comment_original_pred_assertions( As ).

comment_original_pred_assertions( [ _ | As ] ) :-
	comment_original_pred_assertions( As ).



%% %%% REVISED TILL HERE

:- data processed_file/1.
:- data related_file/1.
:- data irrelevant_file/1.

   % module M is (resp.) 
   % processed/related/processed but irrelevant (a leave in the closure)

related_files_closure(Rel,Verb,Opts):-
	current_fact(related_file(_)), !,
	related_files(Rel,Verb,Opts).
related_files_closure(_Rel,_Verb,_Opts).

related_files(Rel,Verb,Opts):-
	retract_fact(related_file(I)),
	\+ current_fact(processed_file(I)),
	% lets ass this at the begining
	\+ user_module(I),
        error_protect(ctrlc_clean(
		   process_file(I, asr, any, 
		                process_related_file(Rel,Verb,Opts), 
                                false, asr_readable(Verb), do_nothing )
				 )),
	fail.
related_files(_Rel,Verb,Opts):-
	related_files_closure(trans,Verb,Opts).

% DTM: Cannot this be asserted at the beginning?
user_module(user).  %% 'user' module cannot be treated as a normal module.

do_nothing(_).


% fail ==> force generation of .asr
asr_readable( Base , Verb ) :-
        % DTM: If you suspect that asr files are being 
	%     reading more than once, uncomment these lines
        %
	% display( 'reading ' ) , display( Base ) , nl,
	(
	    current_fact( processed_file( Base ) )
	->
	     verb_message( _ , ['Internal Error: file ' , Base , 
	                        ' is beeing processed twice!' ] )
	;
	     assertz_fact( processed_file( Base ) ),
  	     get_module_filename( pl  , Base , PlName  ),
	     get_module_filename( asr , Base , AsrName ),
	     file_up_to_date(AsrName,PlName),
	     
%	     display( 'Reading asr file ' ) , display( AsrName ) , nl,
	     
	     read_asr_file( AsrName , Verb ),
	     defines_module(Base,M),
	     assert_itf(defines_module,M,_,_,Base)
        ).




file_up_to_date(AsrName,PlName):-
	modif_time(AsrName, AsrTime),
	modif_time(PlName, PlTime),
	PlTime < AsrTime.



add_related_file( IMAbs ) :-
 	\+ current_fact(processed_file(IMAbs)),
 	\+ current_fact(related_file(IMAbs)),
 	assertz_fact(related_file(IMAbs)),
% 	display( added_related_file( IMAbs ) ),nl,
	!.

add_related_file( _IMAbs ).


%% ---------------------------------------------------------------------------
%% Preprocessing Unit closure

assert_related_files0(direct,Base,_M,Verb):- !,
	assert_related_files_(Base,Verb).
assert_related_files0(trans,Base,M,Verb):-
	assert_related_files(Base,M,Verb).

% the closure finalizes when there is no property exported:
assert_related_files(Base,M,Verb):-
	relevant_prop(M,Prop),
	functor(Prop,F,A),
	exports(Base,F,A,_DefType,_Meta),
	!,
	assert_related_files_(Base,Verb).

assert_related_files(_Base,M,_Verb):-
	Fact = irrelevant_file(M),
	assertz_fact( Fact ),
	write_asr_fact( Fact ).
		




:- data related_file_on_asr/1.

assert_related_files_(Base,_Verb):-
	retractall_fact(related_file_on_asr(IMAbs)),
	imports_pred(Base,IM,_F,_A,_DefType,_Meta,_EndFile),
% NEW
 	file_path(Base,CWD),
 	absolute_file_name( IM ,'' , '.pl' , CWD , _ , IMAbs , _ ),

% OLD:
% 	file_path(Base,Path),
% 	working_directory(Old,Path),
% 	absolute_file_name(IM,'','.pl','.',_,IMAbs,_),
% 	working_directory(_Path,Old),
%
% TEST THAT OLD IS LIKE NEW	
% 	dtm_proof( Base, IM , IMAbs2 ) ,
% 	( 
% 	    IMAbs2 \== IMAbs
% 	->
% 	    display( 'no es igual en el caso de ' ) ,
% 	    display( IMAbs2 ) , display( '   ' ),
% 	    display( IMAbs ), nl,nl
% 	;
% 	    true
% 	),

	\+ current_fact(related_file_on_asr(IMAbs)),
	( 
	    current_fact(generate_asr_file)
	->
	    asserta_fact(related_file_on_asr(IMAbs)),
	    fast_write(related_file(IMAbs))
	;
	    true
	),
	add_related_file(IMAbs),
% 	display( imabs( IMAbs ) ),nl,
	fail.
assert_related_files_(_Base,_Verb).



% dtm_proof( Base , IM , IMAbs ) :-
% 	absolute_file_name(IM,'','.pl', Base ,_,IMAbs,_).



file_path(Base,Path):-
	atom_codes(Base,Bases),
	no_path_file_name(Bases,Names),
	append(Paths,Names,Bases),
	atom_codes(Path,Paths).




relevant_prop(M,Prop):-
	current_fact(
		assertion_of(PD,M,_Status,prop,_Body,_Dict,_S,_LB,_LE) ),
	functor( PD   , F , A ),
	functor( Prop , F , A ).




assert_clauses_of(Base,M):-
	db_clause_of(_H,Base,M,Head,Body,VarNames,Source,Line0,Line1),
	has_to_be_asserted( M , Head , Body , Source ),
	assertz_fact(clause_read(M,Head,Body,VarNames,Source,Line0,Line1)),
	fail.
assert_clauses_of(_Base,_M).




get_assertions_of( Pred , M , As ):-
	findall( A , get_one_assertion_of( Pred , M , A), As ),
	!.

get_assertions_of( _Pred , _M , [] ).




fast_write_assertions( [] ).

fast_write_assertions( [ A | As ] ) :-
	A = as${
		     head    => ExpPD,
		     compat  => Co,
		     call    => Ca,
		     succ    => Su,
		     comp    => Cp,
		     status  => Status,
		     type    => Type,
		     dic     => Dict,
		     comment => Cm,
		     locator => Loc
		 },
	Loc = loc${
		      module     => M,
		      file       => Source,
		      line_begin => LB,
		      line_end   => LE
		  },
	assertion_body(ExpPD,Co,Ca,Su,Cp,Cm,Body1),
	ASSRT  = assertion_read(ExpPD,M,Status,Type,Body1,Dict,Source,LB,LE),
	fast_write( ASSRT ),
	fast_write_assertions( As ).
		  
	

%%% --------------------------------
%%% --- TEMPORARY
%%% --------------------------------

%% by default we include everything from our own module
has_to_be_asserted( user(_) , _Head , _Body , _Source ) :-
	!.

%% by default we include everything from our own module
has_to_be_asserted( Module , _Head , _Body , Source ) :-
	get_module_from_path( Source , Module ),
	!.

%% a directive has to be keep iff it belongs tu a package which
%% is not syntax one
has_to_be_asserted( _ , Head , Body , Source ) :-
	!,
	get_module_from_path( Source , Module ),
	(
	    is_syntax_package( Module )
	-> 
	    ( 
		% if it is a directive
		number( Head )
	    ->
	        % no necesary to add directives from syntax packages
	        add_package_to_output( Module ),
	        fail
	    ;

		% ERROR: A syntax package is adding a clause => then
		% it is no syntax one
		error_message( "Package ~w is said to be syntax " ||
		       "package but has the clause: ~w :- ~w. " ||
		       "The Output will be incorrect." ,
		       [Module , Head , Body] )
	    )
	;
	\+ is_included_by_default_module( Module )
	).




get_module_from_path( Path , Module ) :-
	no_path_file_name( Path , File ), 
 	basename( File , Module ).


%%% --- DTM: Should be in ITF!

is_syntax_package( rtchecks   ).
is_syntax_package( basicmodes ).
is_syntax_package( isomodes   ).
is_syntax_package( assertions ).
is_syntax_package( regtypes   ).

is_included_by_default_module( nonpure ).
is_included_by_default_module( prelude ).

%% ---------------------------------------------------------------------------
%% Module Name Expansion in the DB

%% --- DTM: The Dict should be vnDict (to complete variables and unify with 
%%          clauses one)

get_one_assertion_of( PD , M , As2 ):-
	current_fact(assertion_of(PD,M,Status,Type,Body0,Dict,Source,LB,LE)),
	assertion_body(PD,Co,Ca,Su,Cp,Cm,Body0),
	LOC = loc( Source , LB , LE ),
	expand_subbody( Co , M , Dict , ECo , LOC ),
	expand_subbody( Ca , M , Dict , ECa , LOC ),
	expand_subbody( Su , M , Dict , ESu , LOC ),
	expand_subbody( Cp , M , Dict , ECp , LOC ),

	module_expand( PD , true , M, Dict , ExpPD , _ , Source , LB , LE ),

%	build_a_fake_body(Co,Ca,Su,Cp,FakeBody),
%	module_expand(PD,FakeBody,M,Dict,ExpPD,ExpBody,Source,LB,LE),
%	split_a_fake_body(ECo,ECa,ESu,ECp,ExpBody),
	
	As2 = as${
		     head    => ExpPD,
		     compat  => ECo,
		     call    => ECa,
		     succ    => ESu,
		     comp    => ECp,
		     status  => Status,
		     type    => Type,
		     dic     => Dict,
		     comment => Cm,
		     locator => Loc
		 },
	Loc = loc${
		      module     => M,
		      file       => Source,
		      line_begin => LB,
		      line_end   => LE
		  }.


expand_subbody( C , M , Dict , CO , loc( Source , L0 , L1 ) ) :-
	asbody_to_conj( C , CC ),
	module_expand( in_assertion_body , CC , M, Dict , _ , EC , Source , L0 , L1 ),
	asbody_to_conj( CO , EC ).

	

% build_a_fake_body(Co,Ca,Su,Cp,(CCo;CCa;CSu;CCp)):-
% 	list_to_conj(Co,CCo),
% 	list_to_conj(Ca,CCa),
% 	list_to_conj(Su,CSu),
% 	list_to_conj(Cp,CCp).

% split_a_fake_body(Co,Ca,Su,Cp,(CCo;CCa;CSu;CCp)):-
% 	conj_to_list(CCo,Co),
% 	conj_to_list(CCa,Ca),
% 	conj_to_list(CSu,Su),
% 	conj_to_list(CCp,Cp).

db_clause_of(Head,Base,M,H,B,VarNames,Source,Line0,Line1):-
%% clause_of/7 is a predicate (no data) in c_itf.pl)
	clause_of(Base,Head,Body,VarNames,Source,Line0,Line1),
	( 
	    number(Head)
	-> 
	    H = Head,
	    B = Body,
	    VarNames = [ ]
	;
	    module_expand( Head , Body , M , VarNames,
	                   H    , B    , Source , Line0 , Line1 )
	).

activate_second_translation(Base,M):-
        activate_translation(Base,M,add_clause_trans),
        activate_translation(Base,M,add_goal_trans),
        expand_clause(0,0,M,_,_,_). % Translator initialization

deactivate_second_translation(_Base,M):-
	del_goal_trans(M),
	del_clause_trans(M).

module_expand(Head,Body,M,VarNames,H,B,Source,Line0,Line1):-
	(
	    c_itf:module_expansion( Head , Body , M , VarNames , asr,
	                            _H0 , _B0 , H , B )
	->
	    ( VarNames = [ ], ! ; true )
	;
%%	    error_message( loc(Source,Line0,Line1),
%%	                   "Unable to expand~n  ~q :- ~q",[Head,Body]),
%% DTM: just trying the pretty printer!
%% --- DTM: this is an assertion, not a clause
	    error_message( loc(Source,Line0,Line1),
	                   "Unable to expand~n  ~p", 
			   ['$clause'(Head,Body,VarNames)]),
	    fail
	).


%% ---------------------------------------------------------------------------
%% Related file processing

:- data generate_asr_file/0.

process_related_file(Base,Rel,Verb,Opts):-
	defines_module(Base,M),

	assertz_fact( processed_file( Base ) ),
%	display( processed_file( Base ) ) , nl ,

	assert_itf(defines_module,M,_,_,Base),
        verb_message(Verb,['{Processing related module ',M]),
	%% .asr file
	get_module_filename(asr,Base,AsrName),
	( 
	    open_asr_to_write(  AsrName , Stream , CI ),
	    !,
	    write_asr_header,
	    fast_write( defines( M , Base ) ),
	    set_fact(generate_asr_file)
	;
	    retractall_fact(generate_asr_file)
	),
	%% inhibits the expansion of meta-predicates
	%% (can not!) checks that properties are identifiable
	normalize_assertions(M,Base,Opts),

	%% saves exported assertions, identifies relevant properties,
	%% and saves such property definitions
        activate_second_translation(Base,M),
	save_exported_assertions_of(Base,M),
	save_relevant_properties_of(Base,M),
        deactivate_second_translation(Base,M),

	%% store (more) files related to Base
	assert_related_files0(Rel,Base,M,Verb),

	%% .asr file
	(
	    current_fact(generate_asr_file) 
	->
	    close_asr( Stream , CI ),
	    retractall_fact(generate_asr_file)
	;
	    true
	),
        verb_message( Verb , '}' ).



write_and_save_assertions_of( P , M ) :-
	get_assertions_of( P , M , Assrt ),
	compound_to_simple_assrt_same_pred( Assrt , NAssrt ),
	add_assertions( NAssrt ),
	(
 	    current_fact(generate_asr_file) 
	->
	    fast_write_assertions( NAssrt )
	;
	    true
	).

% --- notify internal error!
% write_and_save_assertions_of( P , M ) :-
	




save_exported_assertions_of( Base , M ) :-
	exports( Base , F , A , _DefType , _Meta ),
	functor( PD , F , A ),
	write_and_save_assertions_of( PD , M ),
	fail.

save_exported_assertions_of( _Base , _M ).




save_relevant_properties_of(Base,M):-
	relevant_prop( M  , Prop ),
	save_predicate_clauses_of( Base , M , Prop ),
	functor( Prop , F , A ),
	\+ exports( Base , F , A , _DefType , _Meta ),
	write_and_save_assertions_of( Prop , M ),
	fail.

save_relevant_properties_of(_Base,_M).




save_predicate_clauses_of( Base , M , Prop ) :-
	db_clause_of(Prop,Base,M,Head,Body,VarNames,Source,Line0,Line1),
	Fact = prop_clause_read(M,Head,Body,VarNames,Source,Line0,Line1),
	write_asr_fact( Fact ),
	assertz_fact( Fact ),
	fail.

save_predicate_clauses_of( _Base , _M , _Prop ).



% save_predicate_assertions_of(M,Prop):-
% 	save_one_assertion_of(Prop,M),
% 	fail.
% save_predicate_assertions_of(_M,_Prop).


%% ---------------------------------------------------------------------------
%% Checking for properties in assertions
%% ---------------------------------------------------------------------------

:- data warned/3.

delayed_checks:-
	retractall_fact(warned(_,_,_)),
%	current_fact(
			assertion_read(PD,M,_Status,Type,Body,_Dict,S,LB,LE),
		    %),
	\+ current_fact(irrelevant_file(M)),
	\+ Type = modedef,
	
%	display( assertion_read(PD,M,Type,Body) ), nl , nl,
	
	functor(PD,F,A),
       	assertion_body(_NPD,CNDP,CNCP,CNAP,CNGP,_CO,Body),
	Where=loc(S,LB,LE),
	check_properties(CNDP,F,A,M,Where),
	check_properties(CNCP,F,A,M,Where),
	check_properties(CNAP,F,A,M,Where),
	check_properties(CNGP,F,A,M,Where),
	fail.
delayed_checks.

check_properties([],_F,_A,_M,_Where) :-
	!.

check_properties( [(P1;P2)] ,F,A,M,Where):-
	!,
	check_properties(P1,F,A,M,Where),
	check_properties_special_case(P2,F,A,M,Where).

check_properties([Prop|Props],F,A,M,Where):-
	!,
	functor(Prop,PF,PA),
	check_property(PF,PA, Prop , F,A,M,Where),
	check_properties(Props,F,A,M,Where).

check_properties(PROP,F,A,M,Where):-
	error_message(Where,
	    "INTERNAL ERROR: check_properties: list of properties " ||
            "expected as argument. "||
	    "Found: ~q. It was used in an assertion ~w in module ~w",
	    [PROP, F/A, M ]).


% Here is the case:
%
%  The body assertion _type_ is a list. Then ';' were introduced and
% things like [A;B], with A, B lists, are now accepted.  A problem
% araise when we have something like [A;B;C].  As ';' works like a
% functor, we got ';'(A,(B;C)), with A,B and C list, _BUT_ in:
%
% check_properties( [(P1;P2)] ,F,A,M,Where):-
% 	!,
% 	check_properties(P1,F,A,M,Where),
% 	check_properties(P2,F,A,M,Where).
%
% P2 is (B;C) so it is not a list!.
%
% Then, here we have the special case:
check_properties_special_case( (P1;P2) ,F,A,M,Where) :-
	!,
	check_properties(P1,F,A,M,Where),
	check_properties_special_case(P2,F,A,M,Where).

check_properties_special_case( P1 ,F,A,M,Where) :-
	check_properties(P1,F,A,M,Where).



check_property(call,_PA, _Prop ,_F, _A,_M,_Where):- 
	!.

check_property(';' , 2, ';'( A , B ) , _F,_A,_M,_Where) :-
	check_properties( A , _F,_A,_M,_Where),
	check_properties( B , _F,_A,_M,_Where),
	!.

check_property(PF,PA, _Prop , _F,_A,_M,_Where):-
%	relevant_prop(_AM,Prop), !.
	functor(PD,PF,PA),
	current_fact(
	         assertion_read(PD,_AM,_Status,prop,_Body,_Dict,_S,_LB,_LE) ),
	!.
check_property(PF,PA,_Prop,_F,_A,M,_Where):-
	warned(PF,PA,M), 
	!.
check_property(PF,PA,_Prop,F,A,M,Where):-
	warning_message(Where,
	    "~w used in an assertion for ~w in ~w is not a property",
	    [PF/PA,F/A,M]),
	asserta_fact(warned(PF,PA,M)).


%% ---------------------------------------------------------------------------
%% SHOW ASR FILE
%% ---------------------------------------------------------------------------

:- pred show_asr( +File )

# "Read and shows the asr @var{File} file.".

show_asr( File ) :-
	open(File,read,Stream),
        current_input(O),
        set_input(Stream),
	read( X ),
	display( 'ASR VERSION: ' ),
	display( X ),
	nl,
	read_and_show,
	close(Stream),
	set_input(O).


read_and_show :-
	fast_read( T ) ,
	display( T ) , nl, nl,
	read_and_show.

read_and_show.



%% ---------------------------------------------------------------------------
%% READ ASR FILE
%% ---------------------------------------------------------------------------


read_asr_file( AsrName , Verb ) :-
	catch( open(AsrName, read, Stream) , _ , fail ),
	current_input(CI),
	set_input(Stream),
	(
	  asr_version(V),
	  read(v(V)), 
	  !,
	  verb_message(Verb, ['{Reading ',AsrName]),
	  read_asr_data_loop( Verb ),
	  set_input( CI ),
	  close( Stream ),
	  verb_message( Verb , '}' )
	;
	  verb_message(Verb,['{Old version in ',AsrName,'}']),
	  set_input(CI),
	  close(Stream),
	  fail
	).


%% fast_read/1 version (just fails at end of file)
read_asr_data_loop( Verb ) :-
	(
	    fast_read(X)
	-> 
   	    read_asr_data_loop__action( X ),
	    read_asr_data_loop(Verb)
	;  
	    true 
	).


% not fails + determinist
read_asr_data_loop__action( defines(M,Base) ) :-
	assert_itf(defines_module,M,_,_,Base).

read_asr_data_loop__action( related_file( M ) ) :-
	add_related_file( M ).

read_asr_data_loop__action( X ) :-
	assertz_fact(X). % assertion_read/9
	

%% ---------------------------------------------------------------------------
%% WRITE ASR FILE
%% ---------------------------------------------------------------------------

write_asr_fact( X ) :-
	current_fact(generate_asr_file),
	!,
	fast_write( X ).

write_asr_fact( _ ).




write_asr_fact( [ X | Xs ] ) :-
	write_asr_fact( X ),
	write_asr_fact( Xs ).

write_asr_fact_list( [ ] ).




write_asr_header :-
	asr_version(V),
	displayq(v(V)), 
	display(' .'), 
	nl.




open_asr_to_write(  AsrName , Stream , CI ) :-
	(
	    catch(open(AsrName, write, Stream),_,fail) 
	->
	    current_output(CI),
	    set_output(Stream)
	).




close_asr( Stream , CI ) :-
	set_output(CI),
	close(Stream).

%% ---------------------------------------------------------------------------

:- set_prolog_flag(multi_arity_warnings, off).

% --- remove this line: testing...
%verb_message(_,Message) :-
%	io_aux:message(Message).

verb_message(verbose,Message) :-
	io_aux:message(Message).
verb_message(debug,Message) :-
	io_aux:message(Message).
verb_message(quiet,_Message).

verb_message(verbose,Type,Message) :-
	io_aux:message(Type,Message).
verb_message(debug,Type,Message) :-
	io_aux:message(Type,Message).
verb_message(quiet,_Type,_Message).

:- set_prolog_flag(multi_arity_warnings, on).


%% ---------------------------------------------------------------------------
% CACHE
%% ---------------------------------------------------------------------------


ast_cache( [
	library(aggregates),
	library(debugger),
	library(lists),
% hiord?
%	library(hiord_rt),
	library(rtchecks_mod),
	library(sort),
	library(terms_check),

	engine(term_basic),
	engine(arithmetic),
	engine(debugger_support),
	engine(mattr_global),
	engine(term_compare),
	engine(term_typing),
	engine(atomic_basic), 
	engine(exceptions),
	engine(prolog_flags),
	engine(attributes),
	engine(basic_props),
	engine(internals),
	engine(basiccontrol),
	engine(io_aux),
	engine(streams_basic),
	engine(data_facts),
	engine(io_basic),
	engine(system_info)
	   ]
	 ).


% --- DTM: Just to see it it works...
read_cache :-
	read_asr_file( '/home/dtm/Ciaopp/Benchmarks/ciaopp/checks/ciaopp_cache' , quiet ).


do_cache :-
	clean_up_facts,	
	retractall_fact( assertion_read( _ , _, _ , _ , _ , _ , _ , _ , _ ) ),
	ast_cache( Modules ),
	
	% this funcion assert 'related_files' in order to make
	% related_files_closure work.
	transform_to_related_files( Modules , _Files , Names ),
	related_files_closure( direct , quiet , [] ),
	open_asr_to_write( ciaopp_cache , Stream , CI ),
	write_asr_header,
	set_fact( generate_asr_file ),
	(
	% save assertions_of
	save_cache_assertions( Names ),

	% save prop_clauses
	save_prop_clauses( Names ),

	% save related_files
	save_related_files,

	% save processed_files
	save_processed_files
       ->
          true
       ;
	  message( error, ['There was an error generating cache.'] )
        ),
	% save processed_files	
        retractall_fact(generate_asr_file),
	close_asr( Stream , CI ).




transform_to_related_files( [] , [] , [] ).

transform_to_related_files( [ M | Ms ] , [ F | Fs ] , [ N | Ns ] ) :-
	absolute_file_name( M , F ),
	!,
	get_module_from_path( F , N ),
	add_related_file( F ),
	transform_to_related_files( Ms , Fs , Ns ).

transform_to_related_files( [ _ | Ms ] , F , N ) :-
	transform_to_related_files( Ms , F , N ),
	!.



save_cache_assertions( [] ).

save_cache_assertions( [ M | Ms ] ) :-
	get_assertions( _ , as${ locator => loc${ module => M }} , true , L ),	
	fast_write_assertions( L ),
	save_cache_assertions( Ms ).




save_prop_clauses( [] ).

save_prop_clauses( [ M | Ms ] ) :-
	save_prop_clauses__( M ),
	save_prop_clauses( Ms ).




save_prop_clauses__( M ) :-
        % relevant_prop( M  , Prop ),
	% db_clause_of( Prop , _   , M,Head,Body,VarNames,Source,Line0,Line1),
	prop_clause_read(        M,Head,Body,VarNames,Source,Line0,Line1),
	Fact = prop_clause_read( M,Head,Body,VarNames,Source,Line0,Line1),
	write_asr_fact( Fact ),
	fail.

save_prop_clauses__( _ ).





save_related_files :-
	current_fact( related_file( F ) ),
	write_asr_fact( related_file( F ) ),
	fail.

save_related_files.




save_processed_files :-
	current_fact( processed_file( F ) ),
	write_asr_fact( processed_file( F ) ),
	fail.

save_processed_files.







%% ---------------------------------------------------------------------------

:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+783,2004/10/23,06:37*39+'CEST'), "Fixed bug
   which did not find correctly the local modules.  (David Trallero
   Mena)").

:- doc(version(1*0+778,2004/10/21,21:19*10+'CEST'), "Added
   necessary funtionality for @pred{check_properties} to check
   properties of a list of list of disjuntions ( [A;B;C] ).  (David
   Trallero Mena)").

:- doc(version(1*0+777,2004/10/21,21:18*48+'CEST'),
   "@pred{irrelevant_file/1} was not saved in the ast files, so
   check_properties did different things when loading the ast file than
   when generating it. (David Trallero Mena)").

:- doc(version(1*0+655,2004/09/20,20:36*53+'CEST'), "The 'pred
   assertions that are read and processed are now saved as commented
   assertions to allow producing nice output.  (David Trallero Mena)").

:- doc(version(1*0+654,2004/09/20,20:36*11+'CEST'), "Added some
   ':- pred' declaration that were missing.  (David Trallero Mena)").

:- doc(version(1*0+617,2004/09/10,19:08*58+'CEST'), "Solved bug
   15.  (David Trallero Mena)").

:- doc(version(1*0+614,2004/09/09,17:35*18+'CEST'), "Documented
   use of preprocessing_unit/3 when first and second arguments are
   lists instead of file names.  (Jesus Correas Fernandez)").

:- doc(version(1*0+602,2004/08/08,14:31*44+'CEST'), "Assertions
   are simplified when generating the .ast file.  (David Trallero
   Mena)").

:- doc(version(1*0+601,2004/08/08,14:30*53+'CEST'), "Added
   show_asr.  (David Trallero Mena)").

:- doc(version(1*0+600,2004/08/08,14:30*35+'CEST'), "Fixed bug
   7. The same asr file was read several times.  (David Trallero
   Mena)").

:- doc(version(1*0+594,2004/07/30,21:36*55+'CEST'), "Added
   has_to_be_asserted.  (David Trallero Mena)").

:- doc(version(1*0+584,2004/07/26,13:54*16+'CEST'), "Internal
   predicates @pred{related_file/1} and @pred{processed_file/1} now
   store basenames instead of full module file names.  (Jesus Correas
   Fernandez)").

:- doc(version(1*0+582,2004/07/26,13:50*23+'CEST'), "calls to
   @pred{absolute_file_name/2} changed to @pred{absolute_file_name/7}.
   (Jesus Correas Fernandez)").

:- doc(version(1*0+546,2004/07/15,11:37*38+'CEST'), "Added support
   for writing asr files (in ciaopp format, with '.ast'
   extension). asr files are stored where @code{asr_dir} flag
   indicates: either the directory where the corresponding source file
   resides, or a given directory for all source files. Assertion
   reading procedure does not fail if asr files cannot be written on
   disk (in order to work with library directories with user write
   permissions disabled.  (Jesus Correas Fernandez)").

:- doc(version(1*0+364,2004/03/04,13:41*46+'CET'), "Added extra
   argument to return an error flag.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+356,2004/02/25,19:20*03+'CET'), "Some internal
   predicates exported to be used from m_unit.pl.  (Jesus Correas
   Fernandez)").

:- doc(version(1*0+282,2004/02/02,20:11*10+'CET'), "Now .ast files
   are read.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+256,2004/01/29,16:01*29+'CET'), "Assert
   location/3 so that cioapp gives proper messages on module
   expansion.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+71,2003/09/12,12:01*16+'CEST'), "Added
   multifile to visible.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+70,2003/09/12,11:18*51+'CEST'), "call/n valid
   property in assertions.  (Francisco Bueno Carrillo)").

:- doc(version(1*0+26,2003/08/21,16:33*27+'CEST'), "Bug Fixed
   which prevented loading modules residing in a directory
   different from the current directory. This bug also raised in some
   cases when a module used another module in a different directory.
   (Jesus Correas Fernandez)").

:- doc(version(1*0+14,2003/07/21,15:41*51+'CEST'), "Expansion of
   meta-predicates inhibited.  (Francisco Bueno Carrillo)").

:- doc(version(1*9+78,2003/04/21,17:09*39+'CEST'), "Simplified the
   processing of p_asr.  (Francisco Bueno Carrillo)").

:- doc(version(1*9+74,2003/04/08,13:13*23+'CEST'),
   "generate_asr_file/2 changed to generate_asr_files/2, to reflect
   the 'multifile' nature of assertion file generation.  (Jesus
   Correas Fernandez)").

:- doc(version(1*9+69,2003/03/18,19:32*08+'CET'), "Now p_asr reads
   from .ast files.  (Jesus Correas Fernandez)").

%% ---------------------------------------------------------------------------
