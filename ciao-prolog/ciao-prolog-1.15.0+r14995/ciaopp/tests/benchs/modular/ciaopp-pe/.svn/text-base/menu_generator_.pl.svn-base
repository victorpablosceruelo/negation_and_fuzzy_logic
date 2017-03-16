:-module( menu_generator_ , 
	             [
		      menu/1,
		      menu/2,
		      menu/3,
		      get_flag/3,
		      set_flag/3,
		      space/1,
		      get_menu_configs/1,
		      save_menu_config/1,
		      remove_menu_config/1,
		      restore_menu_config/1,
		      show_menu_configs/0,
		      show_menu_config/1
		     ], [ hiord , assertions , persdb , .('api/api_menu_') ] ).

:- use_module(library(aggregates)).
:- use_module(library(write)).

:- use_module(preprocess_flags_).
:- use_module(library(prompt)).
:- use_module(library(lists), [
	                           reverse/2, 
	                           append/3,
				   select/3
				  ] ).

:- data      menu_flag/3.

% :- multifile menu_default/3.
% :- multifile menu_opt/6.
% :- meta_predicate menu_opt( ? , ? , ? , pred(1) , pred( 0 ) , pred(2) ).


persistent_dir( menudbdir , '~/.ciao.d/menu_flags' ).

:- persistent( menu_config/2 , menudbdir ).



%:- multifile pp_flag/2 , 
%	     valid_flag_values/2,
%	     pp_flag/1.
% There was a bug: One time we tried to create a menu for mecce. We
% changed some preprocess_flags predicates to multifiles. Then we
% discover a bug in Ciao, that didnt allow us to execute code form
% multifiles. So, we have to retract the changes. Unfortunately, I
% forgot to comment the multifiles lines in menu_generator, also, in
% preprocess_flags (exporting this "new-multifiles" predicates). When
% menu_generator was trying to find the possible options of a flag (it
% uses valid_flags_values/2), the predicate fails (it was not
% multifile), so it tried with valid_flag_value/2 (note there is no s at
% the end) and a findall... you can imagine the rest.


:- trust pred menu_flag( Menu , Flag , Value ) :
	term * atom * term

# "This predicate is internal and stored the menu configuration.
 @var{Menu} is the menu, as defined in @pred{menu_opt/6} and
 @var{Flag} is the flag value stored. @var{Value} is the value
 stored.".




:- trust pred menu_default( Menu , Flag , DefaultValue ) :
	term * atm * atm

# "@var{Menu} is a term that has to correspond with the 1st argument
  of @pred{Menu}. @var{Flag} is the desired flag to have a default
  value. @var{DefaultValue} is the default value of @var{Flag}.".


:- trust pred menu_default( Menu , Flag , DefaultValue )  => 
	atm  * atm * atm.


:- trust pred menu_default( Menu , Flag , DefaultValue ) :
	atm  * var * var

# "This call mode can be used to ask which flags and its values
  has a menu @var{menu}".


:- trust pred menu_default( Menu , Flag , DefaultValue ) :
	atm  * atm * var

# "This call mode can be used to ask which value have the flag
  @var{Flag} in the menu @var{menu}".




:- pred menu_opt( Menu , Flag , Text , Guard , BeforePrinting , SelectedHook ) :
	term * atm * atm * callable * callable * callable

# "@var{Menu} is a term that specifies the menu name. It can be an
  atom or just a predicate of arity 1, where the 1st argument
  indicates the menu level (i.e., ana(1) is the level 1 of 'ana'
  menu). @var{Flag} is the flag that will be asked. 

  @{Text} is the test that will be printed when asking the
  @var{Flag}. 

  @var{Guard} is a predicate of arity 1 that is invoked to see if the
  flag should be asked. The argument is the selected menu options till
  moment in the way: [flag1=value1, flag2=value2, ...]. 

  @var{BeforePrinting} is a predicate of arity 0, that is invoked
  whenever the menu option has been selected the validator menu
  options chooser.

  @var{SelectedHook} is a predicate of arity 2, that is invoked
  whenever the flag has been selected by the user. The 1st argument
  are the current selected values, including the current flag, and in
  the 2nd argument the possible modified list is expected.

  In summary, if @var{Guard} holds, then @var{BeforePrinting} is
  executed (no action is taken whether it fails or not), and after the
  user has types the option @var{SelectedHook} is invoked.".


:- trust pred menu_opt( Menu , Flag , Text , Guard , BeforePrinting , SelectedHook ) :
	term * term * term * term * term *term.


:- trust pred menu_config( Menu , List ) =>
	atm * list.


:- trust pred menu_config( Menu , List ) :
	atm * list

# "@pred{menu_config/2} is used to store user menu configuration
   permanently.  @var{Menu} is a term that has to correspond with the
   1st argument of @pred{Menu}. @var{List} are the flags saved for
   that menu in the way [flag1=value1, flag2=value2, ...].".


:- trust pred menu_config( Menu , List ) :
	atm * var

# "This call mode can be used to ask the @var{List} flag
  configuation of a menu @var{Menu}.".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEFINITIONS         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- argnames cc( flag , message , guard, pre, post ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% THE OUTPUT FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pred space( N ) : num( N )
# "prints @var{N} spaces.".


space( 0 ) :- !.

space( N ) :-
	N > 0,
	!,
	N1 is N - 1,
	display( ' ' ),
	space( N1 ).

space( _ ).




display_with_spaces( Label , DesiredLen ) :-
	display( Label ),
	display( ':' ),
	atom_length( Label , Len ),
	Rest is DesiredLen - Len - 1,
	space( Rest ).




mul_display( L ) :-
	nmul_display( L , 0 , 80 ).

nmul_display( [A|R] , Init , Max ) :-
	atom( A ),
	!,
	line_position( user_output , Current ),
	atom_length( A , AL ) , 
	(
	    AL + Current > Max
	->
	    nl,
	    space( Init )
	;
	    true
	),
	display( user , A ),
	nmul_display( R , Init , Max ).

nmul_display( [A|R] , _Init , Max ) :-
	list( A ),
	!,
	line_position( user_output , NInit ),
	display( '[' ),
	line_position( user_output , LInit ),
	list_display( A , LInit , Max ),
	nmul_display( R , NInit , Max ).

nmul_display( [_|R] , Init , Max ) :-
	!,
	nmul_display( R , Init , Max ).

nmul_display( [] , _ , _ ).



list_display( [ ] , _ , _ ) :- 
	!.

list_display( [A] , Init , Max ) :- 
	!,
	atom(A),
	line_position( user_output , Current ),
	atom_length( A , AL ) , 
	(
	    AL + Current + 1 > Max
	->
	    nl,
	    space( Init )
	;
	    true
	),
	display( user , A   ),
	display( user , ']' ).

list_display( [A|R] , Init , Max ) :- 
	!,
	atom(A),
	line_position( user_output , Current ),
	atom_length( A , AL ) , 
	(
	    AL + Current + 2 > Max
	->
	    nl,
	    space( Init )
	;
	    true
	),
	display( user , A   ),
	display( user , ', ' ),
	list_display( R , Init, Max ).



:-  pred display_long_atom( A ) : atm
# "@var(A) atom is printed in 80 characters-wide.".


display_long_atom( X ) :-
	line_position( user_output , NInit ),
	atom_codes( X , XC ),
	display_long_atom_n( XC , NInit , 80 ).

display_long_atom_n( X , I , Max ) :-
	append( Y  , [32|XE] , X ),
	!,
	atom_codes( YC , Y ),
	atom_length( YC , AL ) , 
	line_position( user_output , Current ),
	(
	    AL + Current > Max
	->
	    nl,
	    space( I )
	;
	    true
	),	
	display( YC ), display( ' ' ),
	display_long_atom_n( XE , I , Max ).

display_long_atom_n( X , I , Max ) :-
	atom_codes( YC , X ),
	atom_length( YC , AL ) , 
	line_position( user_output , Current ),
	(
	    AL + Current > Max
	->
	    nl,
	    space( I )
	;
	    true
	),	
	display( YC ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% THE MENU ITSELF 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag( multi_arity_warnings, off ).

menu( M ) :- 
	menu( M , true ).

menu( M , B ) :- 
	atom( M ),
	!,
	menu( M , 0 , B ).

menu( M , B ) :- 
	M =.. [F,L],
	!,
	menu( F , L , B ).


menu( X , Level , Bool ) :-
	atom( X ),
	findall( cc${flag    => O,
	             message => T,
		     guard   => G ,
		     pre     => PreG,
		     post    => PG }, %(O,T,G,PreG,PG) , 
	         (menu_opt${   menu    => PossM, 
			       flag    => O,
			       message => T,
			       guard   => G,
			       pre     => PreG,
			       post    => PG },
		  is_good_menu( PossM , X , Level )),
		 Menu ),
	Menu \== [],
	exec_guard( Menu , [] , NoPosibles , Posibles , [] ),
	( Bool == true -> display( '\n\n\t\t\t(Press h for help)\n\n' ) ; true ),
	ask_menu( Posibles , NoPosibles , X , [] , Out ),
	save_flags( X , Out ).

:- set_prolog_flag( multi_arity_warnings, on ).



is_good_menu( M , Functor , Level ) :-
	M =.. [Functor , L1 ],
	!,
	L1 =< Level.

is_good_menu( M , Functor , _ ) :- 
	functor( M , Functor , 0 ).



exec_guard( [ CC | AR] , Entry , NoPos , Pos , A ) :- 
	CC= cc${ guard => G },
	!,
	(
	    G( Entry )
	->
	    Pos   = [ CC | PosR   ],
	    NoPos = NoPosR
	;
	    NoPos = [ CC | NoPosR ],
	    Pos   = PosR
	),
	exec_guard( AR , Entry , NoPosR , PosR , A ).

exec_guard( [] , _ , [] , A , A ).



ask_menu( [ CC | PR ] , NoPos , CurrentMenu , Result , Out ) :-
	CC = cc${ flag => O , message => Label , pre => PreG , post => PG },
	get_menu_options( O , OptsList ),
	get_flag( CurrentMenu , O , Default  ),
	
	repeat ,
	( PreG -> true ; true ),
	( ground(Default)
	-> 
	   (
	       atom( Default ) 
	   ->
	       DEF = Default 
	   ;
	       name( Default   , _DEF1 ),
	       atom_codes( DEF , _DEF1 )
	   ),
	   atom_concat( ' (' , DEF    , DD1 ),
	   atom_concat( DD1  , ') ? ' , DefPrn  )
	;
	   DefPrn = '?'
	),
	(
	    OptsList == []
	->
	    display_with_spaces( Label , 32 ),
	    display( DefPrn )
%	    mul_display( ['Select ',Label, DefPrn] )

	;
%	    mul_display( ['Select ',Label,' ',OptsList, DefPrn] )
	    display_with_spaces( Label , 32 ),
	    mul_display( [' ',OptsList, DefPrn] )
	),
	ask_option( OptsList , Default , Opt ),
	(
	    Opt == h
	->
	    display( '\nPrinting help for ' ),
	    display( O ) , nl,
	    get_menu_help( O , Help ),
	    nl,
	    display_long_atom( Help ),
	    nl,nl,
	    fail
	;
	    true
	),
	NewResult_ = [ O=Opt | Result ],
	PG( NewResult_ , NewResult ),
	% to caught errors. Should be check( ground( NewResult ) ).
	ground( NewResult ),
	exec_guard( NoPos , NewResult , NewNoPos , NewPos , PR ),
	ask_menu( NewPos , NewNoPos , CurrentMenu , NewResult , Out ),
	!.

ask_menu( [] , _ , _ , Out , Out ).
	


ask_option( [Opt] , _ , Opt ) :-
	    !,
	    display( '[Automatically Selected]\n' ).
	
ask_option( ask( int , F )  , Default , Opt ) :- 
	!,
	prompt_for_default( Opt , Default ),
	valid_flag_value( F , Opt ).

ask_option( ask( atom , F )  , Default , Opt ) :- 
	!,
	prompt_for_default( Opt , Default ),
	valid_flag_value( F , Opt ).


ask_option( OptsList , Default , OptOut ) :-
	!,
	%% Possible default option,
        prompt_for_default( Opt , Default ),
	( 
	    OptsList == []
	-> 
	    OptOut = Opt
	;
  	    closest_option( [h|OptsList] , Opt , OptOut )
	).




closest_option( List , Opt , OptOut ) :-
	atom_sub_member( List , Opt , PosibleOut ),
	(
	    check_for_exact_match( PosibleOut , Opt , OptOut )
	->
	    true
	;
	    (
		PosibleOut == []
	    ->
	        message( note , [ 'Incorrect Option' ] )
	    ;
	        message( note , [ 'Please be more specific. Posible Options: ',
	                      PosibleOut ] )
	    )
	).


%
% This are the posible cases:
%
% * INPUT:
%   [... aab, aa ... ]  Opt = aa
%   OUTPUT: [aab,aa], as the output is not a list of
%           1 element => we can think it is bad.
%
% SOLUTION: exact match. If there is any option what 
%           exactly match with the Opt, then is it the one selected.
%
atom_sub_member( [] , _ , [] ).

atom_sub_member( [A|As] , Opt , [A|Os] ) :-
	atom_concat( Opt , _ , A ),	    
	!,
	atom_sub_member( As , Opt , Os ).

atom_sub_member( [ _ | As ] , Opt , Out ) :-
	atom_sub_member( As , Opt , Out ).




check_for_exact_match( [ A ] , _  ,  A  ).

check_for_exact_match(  A , Opt , Opt ) :-
	member( Opt , A ).




save_flags( Menu , [ F=V | Fs ] ) :-
	!,
	set_flag( Menu , F , V ),
	save_flags( Menu , Fs ).

save_flags( _ , [] ).


set_flag( Menu , F , V ) :-
	nonvar(V),
	functor( Menu , NMenu , _ ),
	(data_facts:retract_fact( menu_flag( NMenu , F , _ ) )->true;true),
	data_facts:asserta_fact( menu_flag( NMenu , F , V ) ).


get_flag( Menu , F , V ) :-
	functor( Menu , NMenu , _ ),
	data_facts:current_fact( menu_flag( NMenu , F , V ) ),
	!.

get_flag( Menu , F , V ) :-
	functor( Menu , NMenu , _ ),
	menu_default( NMenu , F , V ),
	!.

get_flag( _Menu , F , V ) :-
	get_default_option( F , V ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% glue with pp_flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_menu_options( Flag ,  ask( int , Flag )  ) :-
	valid_flag_values( Flag , X ),
	functor( X , F , _ ),
	(F == int ; F == nnegint ),
	!.

get_menu_options( Flag ,  ask( atom , Flag )  ) :-
	valid_flag_values( Flag , X ),
	functor( X , F , _ ),
	(F == atom ),
	!.

get_menu_options( O , OptsList ) :-
	findall( F , valid_flag_value( O , F ), OptsList ).


get_default_option( O , Def ) :-
	current_pp_flag( O , Def ).

get_menu_help( O , Help ) :-
	pp_flag( O , Help ).



%%%%%
%%%%%  TO SAVE MENU FLAGS
%%%%%

%%------------------------------------------------------------------------
:- entry save_menu_config(Name) : atm.


:- pred save_menu_config(Name) 
	: atm
	# "Save the current flags configuration under the @var{Name} key.".

% Here we have an especification problem. We decided to have 2 (or more)
% menu levels. Basic levels hide some options from the user, while
% advance level show them up. The question is: if we modify a value in
% advance level, do we want that value appear as modified when using
% basic level again?
%
% Example:
% a => 1 , 2 , 3
% a(1) => AA , BB , CC
% b => 10 , 20 , 30
%
% a(1) is the advance option of a.
% by default:
%  * a    is set to 1.
%  * a(1) is set to CC. 
%  * b    is set to 10.
%
%
% if we execute basic level on a 1st instance (everything on default
% values), we will have a = 1 , a(1) = CC, b = 10.
%
% Now, we execute advance menu, and we change a(1) to BB. Again, we
% execute basic level, so a(1) option is not showed. What value does it
% have, CC or BB?

save_menu_config( Name ) :-
	findall( (AF,B,C) , 
	         (menu_opt${ menu => A, flag => B } ,
		  functor( A , AF , _ ),
		  get_flag( A , B , C )) , 
		  L ),
		  display( L ) , nl,
	% DTM: Not really sure if we want to do this. Read explication.
	save_flags_list( Name , L ).


save_flags_list( Name , List ) :-
	( persdbrt:retract_fact( menu_config( Name , _ ) ) , fail ; true ),
	persdbrt:assertz_fact( menu_config( Name , List ) ).



:- entry remove_menu_config(Name) : atm.

:- pred remove_menu_config(Name) 
	: atm
	# "Remove the configuration stored with the @var{Name} key.".


remove_menu_config( Name ) :-
	persdbrt:retract_fact( menu_config( Name , _ ) ),
	fail.

remove_menu_config( _Name ).



:- entry restore_menu_config( Name ) : atm.

:- pred restore_menu_config(Name) 
	: atm
	# "Restore the configuration saved with the @var{Name} key.".


restore_menu_config( Name ) :-
	menu_config( Name , L ),
	restore_flags_list( L ).

%What happends with non existing flags?
restore_flags_list( [] ) :- !.
restore_flags_list( [(A1,A2,A3)|As] ) :-
	 (set_flag( A1 , A2 , A3 )->true;true),
	 restore_flags_list( As ).




:- pred get_menu_configs( X ) : var( X ) => list( X , atom )

# "Returns a list of atoms in @var{X} with the name of stored
  configs.".

get_menu_configs( L ) :-
	findall( Name , menu_config( Name , _ ) , L ).
	



:- pred show_menu_configs 
	# "Show all stored configs.".


show_menu_configs :-
	get_menu_configs( L ),
	show_menu_list( L ).


show_menu_list( [] ) :- !.
show_menu_list( [A|B] ) :-
	write( A ), nl,
	show_menu_list( B ).




:- pred show_menu_config( C )
	: atm
	# "Show specific configuration values pointed by @var{C} key.".
	

show_menu_config( Name ) :-
	menu_config( Name , F ),
	show_config_list( F ).




show_config_list( [] ).
show_config_list( [ (A1 , A2 , A3) |As] ) :-
	display( 'Menu: ' ) , 
	display( A1 ),
	display( '   ' ),
	display( A2 ),
	display( ' = ' ) ,
	display( A3 ), nl,
	show_config_list( As ).


:- doc(version_maintenance,dir('version')).  

:- doc(version(1*0+880,2004/11/16,03:26*47+'CET'), "Fixed
   potential bug when saving menu flags.  (David Trallero Mena)").

:- doc(version(1*0+727,2004/10/11,18:07*50+'CEST'), "Menu options
   are plain saved, so we dont distinguish between menu levels (basic,
   advance, expert...). Read comment in @pred{save_flag_list/1}.
   (David Trallero Mena)").

:- doc(version(1*0+661,2004/09/21,14:22*22+'CEST'),
   "@pred{menu_opt/5} is now @pred{menu_opt/6}. The new argument is a
   new hook that is invoked whenever the selected menu option is going
   to be printed.  (David Trallero Mena)").

:- doc(version(1*0+660,2004/09/21,14:21*09+'CEST'), "Added
   @pred{get_menu_configs/1}.  (David Trallero Mena)").

:- doc(version(1*0+532,2004/07/07,13:39*10+'CEST'), "Changed
   length space for printing menu question to 32.  (David Trallero
   Mena)").

:- doc(version(1*0+477,2004/06/14,19:20*46+'CEST'), "modifications
   added for new pp_flag multifile definitions. Also bug corrected in
   space.  (David Trallero Mena)").

:- doc(version(1*0+470,2004/05/14,12:24*53+'CEST'), "added menu/2
   (David Trallero Mena)").

:- doc(version(1*0+467,2004/05/14,11:46*42+'CEST'),
"mult_display_list now prints 1 space more for better indenting (David
Trallero Mena)").

