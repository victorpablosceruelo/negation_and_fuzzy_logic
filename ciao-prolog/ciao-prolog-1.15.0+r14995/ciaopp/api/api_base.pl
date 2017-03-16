:- module( api_base ,
	[% GET
	  get_key/2
	% LOCATORS
	, loc_unknown/1
	% MODULES AND NAMES
	, get_mod_pred/3
	, get_module/2
	, get_module_base/2
	, get_abs_filename/2
	% DICTIONARIES
	, dic_vars/2
	, dic_names/2
	, dic_join/3
	% STUFF
	, member_and_remove/3
	, remove/3
	, append_if_not_member/3
	, remove_repited/3
	, api_check/4
	, file_up_to_date/2
	, set_modified_flag/0
	, clear_modified_flag/0
	, is_modified/0
	],
	[ assertions , regtypes ] ).


:- use_module(library(lists), [append/3]).

:- reexport(library(messages)).
:- reexport(library(formulae)).

:- use_module(ciaopp(api(api_internal_types))).

:- use_package(.(api_internal_dec)).

:- reexport(library(vndict),   [null_dict/1]).
:- use_module(library(system), [modif_time/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   GET
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- discontiguous get_key/2.

:- pred get_key( OldCls , ClKey )
# "@var{Clkey} is the clause key of old clause format @var{OldCls}".

get_key( ((clause(H,_B): _Id),_) , ClKey ) :- 
	!,
	(
	    var( ClKey ) 
	->
	    functor( H     , F , A ),
	    functor( ClKey , F , A )
	;
	    H = ClKey
	).

:- pred get_key( LitPO , ClKey ) : t_lit_ppi_id(LitPO) => t_cls_key(ClKey)
# "@var{Clkey} is the clause key of old clause format @var{OldCls}".

:- pred get_key( LitPO , ClKey ) : t_cls(LitPO) => t_cls_key(ClKey)
# "@var{Clkey} is the clause key @var{OldCls}".

% just a body literal
get_key( H:_Id , ClKey ) :- 
	!,
	functor( H     , F , A ),
	functor( ClKey , F , A ).

get_key( cls${ key => Key } , Key ) :-
	nonvar( Key ),
	!.

get_key( cls${ head => H  } , ClKey ) :-
	functor( H     , F , A ),
	functor( ClKey , F , A ),
	!.

% --- REVIEW THIS!!! hoe to get infor form the Abs type?
% get_key( Cl_ID , ClKey ) :-
% 	t_cls_ppi_id( Cl_ID ),
% 	!,
% 	clid2data( Cl_ID , F , A , _ ),
% 	functor( ClKey , F , A ).

%just a new body literal
get_key( H , ClKey ) :- 
	functor( H     , F , A ),
	functor( ClKey , F , A ),
	!.

get_key( H , _ ) :- 
	error_message( "Internal Error: Cannot find the key of ~q",
	                [H] ),
	fail.










:- pred loc_unknown( L ) 
# "Generates and unknown locator. Usefull when adding clauses,
  assertions or facts.".

loc_unknown( Loc ) :-
	Loc = loc${ line_end   => 0 , file   => unkown ,
	            line_begin => 0 , module => unknown  }.




:- pred get_mod_pred( MF , M , P )
	: atom(MF)
        => (atom(M), atom(P))

# "Extracts the module (@var{M}) and the predicate (@var{P}) from
  @var{MF}, where @var{MF} is expected to be an atom of the form
  'module:predicate'.".


% This predicate is necessary because generate_unexpanded_data
% try to the by fail all type_of_goal. The problem is when 
% getting the module and the predicate from the 'module:file'
% atom:
% 'arithmetic:=:=' is arithmetic and =:=
% but with backtracking it is 'arithmetic:=:' and = !!!
get_mod_pred( MF , IM , F ) :-
	  atom_concat(IM, F0, MF),
	  atom_concat(':', F, F0),
	  !.


:- pred get_module( Name , Mod )
	: (atom( Name ) ; term(AbsoluteName))
        => atom( Mod )

# "For a given path or alias path @var{AbsoluteName}, the module is
  returned in @var{Mod}.".

get_module( AbsoluteName , M ) :-
	get_module_base( AbsoluteName , Base ),
	get_module__( Base , M ).



:- pred get_module_base( Name , Base )
	: (atom( Name ) ; term(AbsoluteName))
        => atom( Base )

# "For a given path or alias path @var{Name}, the module base (the
  absolute directory included the file that defines the module without
  the extension) is returned in @var{Base}.".

:- pred get_module_base( Name , Base )
	: list( Name )
        => atom( Base ).

get_module_base( [AbsoluteName] , [M] ) :-
	!,
	get_module_base( AbsoluteName , M ).
get_module_base( [A|As] , [M|Ms] ) :-
	!,
	get_module_base( A  , M  ),
	get_module_base( As , Ms ).
% atom: relative or absolute path
get_module_base( AbsoluteName , M ) :-
	atom( AbsoluteName ),
	!,
	get_module__remove_extension( AbsoluteName , M ).
% Path Alias
get_module_base( AliasPath , M ) :-
	get_abs_filename( AliasPath, AbsoluteName ),
	get_module_base( AbsoluteName , M ).



get_module__remove_extension( [] , [] ) :-
	!.
get_module__remove_extension( [M|Ms] , [Mne|Mnes] ) :-
	    !,
	    get_module__remove_extension( M  , Mne  ),
	    get_module__remove_extension( Ms , Mnes ).
get_module__remove_extension( Pl , NoPl ) :-
	    atom_concat( NoPl , '.pl' , Pl ),
	    !.
get_module__remove_extension( Java , NoJava ) :-
	    atom_concat( NoJava , '.java' , Java ),
	    !.
get_module__remove_extension( A , A ).



get_module__( [] , [] ) :-
	!.
get_module__( [NE|NEs] , [M|Ms] ) :-
	!,
	get_module__( NE  , M  ),
	get_module__( NEs , Ms ).
get_module__( NoExtension , M ) :-
	atom_concat( _ , PossibleFile , NoExtension ),
	atom_concat( '/' , PF2 , PossibleFile ),
	!,
	get_module__( PF2 , M ).
get_module__( M , M ).


:- pred get_abs_filename( File , AbsFile )
	: (atom(File) ; term(File))
        => (atom(AbsFile))

# "@var{File} can be a relative, absolute or path alias filename. If
  @var{File} is instead a list, each element of the list should be one
  of the previous cases. In the latter case a list with the
  correspoding absolute filename is returned in
  @var{AbsFile}. Otherwise it is an atom.".

:- pred get_abs_filename( File , AbsFile )
	: list(File) => list(AbsFile).

get_abs_filename( [] , [] ) :- 
	!.
get_abs_filename( [F|Fs] , [AF|AFs] ) :-
	!,
	get_abs_filename( F  , AF  ),
	get_abs_filename( Fs , AFs ).
get_abs_filename( File , AbsFile ) :-
	absolute_file_name(File,'_opt','.pl','.',_,AbsFile,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   DICTIONARIES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --- complete!
:- pred dic_vars( _ , _ ).

dic_vars( dic( V , _ ) , V ).

:- pred dic_names( _ , _ ).

dic_names( dic( _ , N ) , N ).

:- pred dic_join( _ , _ , _ ).

dic_join( dic( A , B ) , dic( A1 , B1 ) , dic( AJ, BJ ) ) :-
	append( A , A1 , AJ ),
	append( B , B1 , BJ ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred member_and_remove( A , L , LR ) 
	: (term(A),list(L))
        => list(LR)

# "If @var{A} belong to @var{L}, @var{LR} is like @var{L} but without
  @var{A}. Otherwise, @var{LR} is @var{L}.".


member_and_remove( A , [A|As] , As ) :-
	!.
member_and_remove( A , [B|As] , [B|Bs] ) :-
	member_and_remove( A , As , Bs ).



:- pred remove( A , L , LR ) 
	: (term(A),list(L))
        => list(LR)

# "Removes all occurences of @var{A} in the list @var{L}.".

remove( A , [A|As] , As ) :-
	!.
remove( A , [B|As] , [B|Bs] ) :-
	remove( A , As , Bs ).


:- pred append_if_not_member( L , A , LR ) 
	: (term(A),list(L))
        => list(LR)

# "@var{A} is appended to the list @var{A} iff @var{A} is not in the
  list @var{L}.".


append_if_not_member( A      , B , C ) :- 
	list( B ),
	!,
	append_if_not_member_list( A , B , C ).
append_if_not_member( []     , A , [ A  ] ) :- 
	!.
append_if_not_member( [A|As] , A , [A|As] ) :- 
	!.
append_if_not_member( [A|As] , E , [A|Bs] ) :-
	append_if_not_member( As , E , Bs ).


append_if_not_member_list( [] , B , B ) :- 
	!.
append_if_not_member_list( L , B , L ) :-
	L = [A|_],
	member( A , B ),
	!.
append_if_not_member_list( [A|As] , B , [A|Bs] ) :-
	!,
	append_if_not_member_list( As , B , Bs ).



:- pred remove_repited( L , A , LR ) 
	: (term(A),list(L))
        => list(LR)

# "Remove all instances of @var{A} form list @var{L}. The result is
  returned in @var{LR}.".

remove_repited( [ L | Ls ] , A , L_out ) :-
	(   member( L , A ) ->
	    remove_repited( Ls , A , L_out )
	;
	    remove_repited( Ls , [ L | A ] , L_out )
	).
remove_repited( [] , A , A ).



:- meta_predicate api_check( goal , ? , ?, ? ).

:- pred api_check( Goal , Message , Opts, T ) 
	: (callable(Goal),string(Message),term(T))

# "It executes the goal @var{Goal}. if it success do nothing. If it
  fails, print a message using format with @var{Message} and @var{T}
  as arguments and then it fails.".

api_check( Goal , Message , Opts , T ) :-
	(  Goal 
	-> true
	;  % append( "Internal Error: ~w: " , Message, M ),
	   error_message( ("Internal Error: ~w: "||Message) , [T|Opts] ),
	   fail
	).




:- pred file_up_to_date(Target,Source) 

# "Success if @var{Source} file has older modfiication time than
  @var{Target} file.".


file_up_to_date(AsrName,PlName):-
	modif_time(AsrName, AsrTime),
	modif_time(PlName, PlTime),
	PlTime < AsrTime.



%%  Modified FLAGS
%%%%%%%%%%%%%%%%%%%

:- data modified_flag/0.

:- pred set_modified_flag

# "Set modified flag.".

set_modified_flag :-
	current_fact( modified_flag ),
	!.
set_modified_flag :-
	asserta_fact( modified_flag ).


:- pred is_modified

# "Success if modified flag was set using @pred{set_modified_flag}.".

is_modified :-
	current_fact( modified_flag ).


:- pred clear_modified_flag

# "Clear modified flag.".

clear_modified_flag :-
	retractall_fact( modified_flag ).
