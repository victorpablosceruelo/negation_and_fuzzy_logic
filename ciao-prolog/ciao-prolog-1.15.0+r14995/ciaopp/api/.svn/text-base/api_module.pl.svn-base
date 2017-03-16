:- module( api_module ,
	[% CLEAN
	  cleanup_actions/0
	, cleanup_module/0
	% DEFINING
	, define_new_module/2
        , define_new_module/3
	, define_new_module/4
	, add_exported_predicate/2
	, add_package_to_output/1
	, load_package_info/1
	% OPERATORS
	, add_operator_to_output/3
	, swap_to_module_operators/0
	, swap_to_ciaopp_operators/0
	% LOADING
	, load_related_modules/2
	% MODULE NAME
	, add_action/1
	, remove_action/1
	, get_packages_to_output/1
	, get_output_name/2
	, output_extension/2
	, loading_module_lock/0
        , loading_module_unlock/0
	% COMMENTS
	, add_comment/1
	, get_comment/1
	],
	[ assertions , isomodes, regtypes ] ).


:- use_module(library(aggregates)).
:- use_module(library(operators)).
:- use_module(library(lists), [append/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(filenames), [file_name_extension/3]).

:- use_module(library(compiler(c_itf)), [define_ops/0]).

:- use_module(program(p_asr), [load_related_files/2]).
:- use_module(program(p_unit), [curr_module/1,
	                        load_module_info/5]).
:- use_module(program(itf_db), [curr_file/2,
	                        assert_itf/5]).

:- use_module(ciaopp(api(api_base))).
:- use_module(ciaopp(api(api_predcl))).
:- use_module(ciaopp(api(api_stuff))).

:- use_module(ciaopp(api(comment_db)), 
	                                [ add_comment/3
	                                , get_comment/3 ] ).

:- use_package( ciaopp( 'api/api_types'  ) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLEAN UP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cleanup_module :-
	retractall_fact( api_output_op( _ , _ , _ ) ),
	retractall_fact( output_packages( _ )       ),
	retractall_fact( loaded_packages( _ )       ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   MODULE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag( multi_arity_warnings, off ).

:- pred define_new_module( Module , File )
# "Same as define_new_module( Module , File , [] , [] ).".

define_new_module( Module , File ) :-
	define_new_module( Module , File , [] , [] ).


:- pred define_new_module( Module , File , Predicates )
# "Same as define_new_module( Module , File , Predicates , [] ).".

define_new_module( Module , File , Predicates ) :-
	define_new_module( Module , File , Predicates , [] ).


:- pred define_new_module( Module , File , Predicates , Packages ) 
	: (atom( Module ), atom( File ), list( Predicates ) ,
          list( Packages , atom ))

# "Defines a new module @var{Module} that is supposed to rely on file
  @var{File} (NOTE that the basename of @var{File} will be used to
  place the output files when calling @pred{output/0}). File has to be
  an _absolute path_, use @pred{absolute_file_name/7} to tranform a
  relative or alias path into absolute. @var{Predicates} are the list
  of predicates that will be exported by the module. Each element of
  @var{Predicates} has to have the form F/A, where F is the functor
  and A the arity. To add new exported predicates use the
  @pred{add_exported_predicate/2} predicate. @var{Packages} are the
  packages that the module will use. NOTE that the necessary
  information for these packages will be loaded.".

define_new_module( Module , File , Predicates , Packages ) :-
	cleanup_all,
	assert_curr_file( File , Module ),
	setup_actions( Module , File ),
	add_exported_predicate( Predicates , Module ),
	add_package_to_output( Packages ).



:- pred assert_curr_file( F , M ) : (atom(F),atom(M))
# "Asserts the fact @pred{curr_file/2} with F and M as arguments.".

:- pred assert_curr_file( F , M ) : (list(F),list(M))
# "This is the case is used when many modules are loaded. It asserts
  several fact of @pred{curr_file/2}.".

assert_curr_file( [] , [] ) :-
	!.
assert_curr_file( F , M ) :-
	atom( F ),
	!,
	absolute_file_name( F , AF ),
	asserta_fact( curr_module( M ) ),
	asserta_fact( curr_file( AF , M ) ).
assert_curr_file( [F|Fs] , [M|Ms] ) :-
	assert_curr_file( F  , M  ),
	assert_curr_file( Fs , Ms ).



setup_actions( Modules , File ) :-
	( atom( File ) ->
	  get_module_base( File, Base ),
	  add_action( Base ) ; true ),
	setup_actions__( Modules ).

setup_actions__( M ) :-
	atom( M ),
	!.
setup_actions__( M ) :-
	list( M ),
	M = [_|MT],
	MT \== [],
	add_action( 'modules' ).
% --- user???
setup_actions__( _ ).



:- pred load_related_modules( Files , M ) : (list( Files ), atom(M))

# "Elements of the list of @var{Files} with the form library(atom) or
  just an atom, are loaded as relative modules (a module that is
  imported by the main module, i.e., the module that is being
  analized). @var{M} is necessary to specify which module imports the
  related modules.".

load_related_modules( Files , M ) :-
	get_abs_filename( Files , AbsFiles ),
	load_related_files( AbsFiles , M ).




:- pred add_exported_predicate( P , M ) : atom( M )

# "Add the predicate @var{P} to the module @var{M}. @var{P} has the
  format F/A.".

add_exported_predicate( F/A , M ) :-
	assert_itf( exports , M , F , A , M ),
	!.
add_exported_predicate( F/A , M ) :-
	atom_concat( [M , ':' , F] , MF ),
	assert_itf( new_exports , M , MF , A , M ),
	!.
add_exported_predicate( [P|Ps] , M ) :-
	add_exported_predicate( P  , M ),
	add_exported_predicate( Ps , M ).
add_exported_predicate( [] , _ ).

:- set_prolog_flag( multi_arity_warnings, on ).

:- pred add_package_to_output( A )
 	: atom( A )

# "Add the package @var{A} to the output packages list. These packages
   are asked when doing the output and will be the third argument of
   ':- module' declaration. Also the necesary information
   (metapredicates) from these packages are loaded into CiaoPP for
   correct unexpansion.".

:- pred add_package_to_output( A )
 	: list( A , atom )

# "Add a list of packages to the output packages list.".

:- data output_packages/1.

add_package_to_output( [] ) :-
	!.

add_package_to_output( [A|As] ) :-
	!,
	add_package_to_output( A  ),
	add_package_to_output( As ).

add_package_to_output( A ) :-
	atom(A),
	(current_fact( output_packages( X ) , Ref ) -> true ; X = []),
	!,
	( 
	    member( A , X ) 
	->
	    true
	;
	    (nonvar(Ref) -> erase( Ref ) ; true ),
	    append( X , [A] , XA ),
	    % if this predicate is called when a module is being loaded
	    % then we only have to add the packate to the list and no to
	    % read it, because it has been already read.
	    %
	    % More clear:
	    % load module
	    %     load package that it is in the module
	    %        assert necessary directives from the package
	    %        (look at has_to_be_asserted)
	    %           Add the package to the output if the package 
	    %           has some transformations <--- this is the case,
	    %                   we dont need to read the package again.
	    (
		is_loading_module_lock 
	    ->
	        true
	    ;
		select_pred_type( [internal, metapred] , Preds ),
		get_abs_filename( library( A ) , File ),
		load_module_info(File, package, false, true, [load_ops, Preds])
	    ),
	    asserta_fact( output_packages( XA ) )
	).



:- pred load_package_info( A )
 	: atom( A )

# "Same as @pred{add_package_to_output/1} but the package will not
  appear in the output package list. Use this predicate only when you
  know what you are doing. This predicate is only useful in case a
  package we include uses an inline expansion over @var{A} package.".

:- data loaded_packages/1.

load_package_info( [] ).
load_package_info( [A|As] ) :-
	load_package_info( A ),
	load_package_info( As ).
load_package_info( A ) :-
	atom(A),
	(current_fact( output_packages( X ) ) -> true ; X = []),
	(current_fact( loaded_packages( Y ) , Ref ) -> true ; Y = []),
	( 
	    (member( A , X ) ; member( A , Y )) 
	->
	    true
	;
	    (nonvar(Ref) -> erase( Ref ) ; true ),
	    append( Y , [A] , YA ),
	    select_pred_type( [internal, metapred] , Preds ),
	    get_abs_filename( library( A ) , File ),
	    load_module_info(File, package, false, true, [load_ops, Preds]),
	    asserta_fact( loaded_packages( YA ) )
	).

	    

:- data api_output_op/3.
:- data api_output_saved_op/3.


:- pred add_operator_to_output( Prec, Type, OP ) 
	: (int(Prec),atm(Type),atm_or_atm_list(OP))

# "When CiaoPP loads a module, the operator directives are not executed
  (they are read as directives, but not processed). The @pred{write/1}
  predicate looks up into the operator DB (read more in
  @pred{current_op/3}) to decide if a predicate of arity 2 is an operator
  or not. Whenever it is an operator, @pred{write/1} writes the atom in the
  middle of the arguments (infix mode) or in pre/post-fix mode if the
  operator arity is 1. Then, CiaoPP has to remember the loaded-module
  operators when output (that call to write) is being done. This predicate
  is used to define the operators that will take into account when doing
  the output. The argurments @var{Prec}, @var{Type} and @var{OP} are the
  same as in @pred{op/3}.".


add_operator_to_output( A , B , C ) :-
	asserta_fact( api_output_op( A , B , C ) ).



:- pred swap_to_module_operators

# "Restore CiaoPP read-module operators to operators DB. The side effect is
  that @pred{write/1} (read output process) writes the read module
  considering the operators defined in it".

swap_to_module_operators :-
	current_op( A , B , C ),
	asserta_fact( api_output_saved_op( A , B , C ) ),
	C \= ',', % Avoid permission error redefining ','/2 (ISO Compliance) -- EMM
	op( 0 , B , C ),
	fail.
swap_to_module_operators :-
	api_output_op( A , B , C ),
	op( A , B , C ),
	fail.
swap_to_module_operators :-
	standard_ops,
	define_ops.



:- pred swap_to_ciaopp_operators

# "Restores CiaoPP program operators.".

swap_to_ciaopp_operators :-
	current_op( _ , B , C ),
	C \= ',', % Avoid permission error redefining ','/2 (ISO Compliance) -- EMM
	op( 0 , B , C ),
	fail.
swap_to_ciaopp_operators :-
	retract_fact( api_output_saved_op( A , B , C ) ),
	op( A , B , C ),
	fail.
swap_to_ciaopp_operators.



:- pred action(+Action) 

# "An @var{Action} is an atom that represent the name of the analysis
  or transformation executed by @pred{analyse/1} or
  @pred{transform/1}. It is used to assign a name
  (filename_shfr_pred_op.pl for example) to the output file when
  @pred{output/0} is executed".



:- data action/1.


cleanup_actions :-
	retract_fact( action( _ ) ),
	fail.
cleanup_actions.


:- pred add_action( M )
	: atom( M )
# "Remember action @var{M}.".

add_action( M ) :-
	assertz_fact( action( M ) ).


:- pred remove_action( M )
	: atom( M )
# "Removes action @var{M}.".

remove_action( M ) :-
	retract_fact( action( M ) ).


:- pred get_output_name( Name , Path ) : (atom( Name ),atom(Path))
# "Generate an output name with the actions stored.".

get_output_name( OptFile , Path ) :-
	findall( X , action( X ) , L ),
	% if this literal fails then there is no module loaded!
	L = [Path|Ls],
	( Ls == [] -> L2 = [Path,none] ; L2 = L ),
	atom_concat_with_underscore( L2 , File ),
	curr_file(Src_File,_),
	( output_extension( Src_File , Extension ) -> true ),
	atom_concat( [File, '_co.',Extension] , OptFile ).



:- pred output_extension( File , Ext ) # "Depending on the extension
of @var{File} and the one of the file used in @pred{module/1}, it
returns the correspongind file extension in @var{Ext}.".

output_extension( File , Ext ) :-
	curr_file( Src_File , _ ),
	( file_name_extension( Src_File , _  , '.pl' ),
	  Ext = pl
	; file_name_extension( Src_File , _ , '.java' ),
	  ( file_name_extension( File , _ , '.pl' )
	  ->  Ext = pl  ;  Ext = java )
	;
	  Ext = pl
	).



atom_concat_with_underscore( [L] , L ) :-
	!.
atom_concat_with_underscore( [A|As] , L ) :-
	!,
	atom_concat_with_underscore( As , AsC ),
	atom_concat( [ A , '_' , AsC] , L ).
atom_concat_with_underscore( L , L ).



:- pred get_packages_to_output( X )

# "Returns in @var{X} the list of packages to be used in the module
  directive of the output file.".

get_packages_to_output( X ) :-
	current_fact( output_packages( X ) ),
	!.
get_packages_to_output( [ ] ).

:- pred loading_module_lock

# "It is for internal use. When loading a module there are some
  different predicates that behaves in a different way, like
  @pred{add_package_to_output/1}.".

:- data api_load_module_lock/0.

loading_module_lock :-
	asserta_fact( api_load_module_lock ).


:- pred is_loading_module_lock

# "Fails if there is no lock.".

is_loading_module_lock :-
	api_load_module_lock.


:- pred loading_module_unlock

# "It is for internal use. Remove the loading lock.".

loading_module_unlock :-
	retractall_fact( api_load_module_lock ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   COMMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred add_comment( C ) 
	: t_comment( C )
# "Add comment @var{C} to the comment DB.".

add_comment( C ) :-
	C = comment${ where => W , type => T , comment => Comm },
%	add_nl_if_there_is_not( Comm, CommNL ),
	add_comment( W , T , Comm ).

/*
add_nl_if_there_is_not( "\n", "\n" ) :-
	!.
add_nl_if_there_is_not( [], "\n" ) :-
	!.
add_nl_if_there_is_not( [C|Cs], [C|Cr] ) :-
	!,
	add_nl_if_there_is_not( Cs, Cr ).
add_nl_if_there_is_not( A, AS ) :-
	atom( A ),
	atom_codes( A , A1 ),
	add_nl_if_there_is_not( A1, A2 ),
	atom_codes( AS, A2 ).
*/

:- pred get_comment( C ) 
	=> t_comment( C )
# "Add comment @var{C} to the comment DB.".

get_comment( C ) :-
	C = comment${ where => W , type => T , comment => Comm },
	get_comment( W , T , Comm ).
