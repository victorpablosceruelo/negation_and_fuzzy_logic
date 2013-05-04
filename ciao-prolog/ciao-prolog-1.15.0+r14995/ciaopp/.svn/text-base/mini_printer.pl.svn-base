% (CiaoPP with reduced footprint)
% TODO: integrate with CiaoPP

:- module( mini_printer, 
	             [ output/0,
	               output/1
		     ],
		     [ assertions
		     , api( ciaopp_api ) 
		     ] ).

:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(system), [copy_file/3]).
:- use_module(library(filenames)).
:- use_module(program(p_unit), [internal_predicate_names/1, read_program/2, type_of_goal/2]). 
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(program(unexpand), 
	[ transform_clause_list/3,
	  transform_body/3,
	  transform_name/3
	]).

:- use_module(library(pretty_print), [pretty_print/4]).
:- use_module(library(messages), [error_message/2]).
:- use_module(library(write), [writeq/1, writeq/2, write/2]).
:- use_module(library(odd), [setarg/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(typeslib(typeslib), 
	[ get_required_types/1,typedef_to_pred/3 ]).
:- use_module(library(format), [format/3]).

:- use_module(library(aggregates), [findall/3, (^)/2]).


:- push_prolog_flag( multi_arity_warnings, off ).

:- pred output 
	# "Outputs the current Module preprocessing state to a file
           named Module@tt{_opt.pl}, where Module is the current
           module.".

output :-
% 	findall( X , action( X ) , L ),
% 	% if this pred fails then there is no module loaded!
% 	concat_all( L , File ),
% 	atom_concat(File,'_co.pl',OptFile),
	get_output_name( OptFile , BaseFile ),
	output(OptFile),
	% create the link now
%	L = [ Path | _ ],
	file_name_extension( OptFile , _ , Extension ),
	atom_concat( [BaseFile, '_co', Extension] , COFile ),
	get_module( BaseFile , FileWOExt ),
	atom_concat( DirPath , FileWOExt , BaseFile ),
	% DTM: This is something "stupid": if COFile = OptFile,
        %      then system does "ln -s file_co.pl file_co.pl"
	%      so a cycle is created :((((
	( 
	    \+ (COFile = OptFile)
	->
%	    del_file_nofail( COFile ),
	    atom_concat(DirPath, RelOptFile, OptFile),
	    copy_file( RelOptFile , COFile , [overwrite, symlink] )
	;
	    true
	).


:- pred output(+Output)
	# "Outputs the current module preprocessing state to a file
           @var{Output}.".

output( File ) :-
	output( File , [print_written_file] ).


:- pred output(+Output,+Opts)
	# "Outputs the current module preprocessing state to a file
           @var{Output}. @var{Opts} can be: 
@begin{itemize}
@item @tt{print_written_file}: print a message telling the output filename.
@end{itemize}".

output( File , Opts ) :-
	internal_output( File , EFile ),
	!,
	(   member( print_written_file , Opts ) 
	->
	    display( '{written file ' ) ,
	    display( EFile ),
	    display( '}' ), 
	    nl
	;   true ).
output( File , _ ) :-
	error_message( "generating output of file ~w" , [File] ),
	fail.



internal_output( File , FileExt ) :-
%	push_prolog_flag( write_strings , on ),
	output_extension( File , Ext ),
	!,
	put_extension( File , Ext , FileExt ),
	open(FileExt,write,Stream),
	( output_by_ext(Ext, Stream )
	-> close(Stream) 
	;  close(Stream), fail).


% put extension if it does not have it
put_extension( File , Ext , FileExt ) :-
	atom_concat( '.' , Ext , PExt ),
	( atom_concat( _ , PExt , File )
	-> FileExt = File
	; atom_concat( File , PExt , FileExt )
	).


:- pred output_by_ext(Ext,File): (atm(Ext),sourcename(File)) 

# "Write asserted Ciao module to file @var{File} if @var{Ext} is
@tt{pl}.".


output_by_ext( pl , Stream ) :-
	curr_file( _ , Module ),
	write_headers(Stream,Module),
	% call api
	print_program(Stream),
	write_types(Stream).




% --- DTM: THIS HAS TO BE A HOOK
write_types(S):-
	get_required_types(Rules),
	nl(S),
	write_list_types(Rules,S).

write_list_types([],_).
write_list_types([Rule|L],S) :- 
	write_one_type(Rule,S),
	write_list_types(L,S). 

write_one_type(typedef(::=(Pred,Def)),S):-
        p_unit:internal_predicate_names( InternalNames ),
	functor(Pred, TypeName, Ari),
	PredAri is Ari + 1,
	curr_file(_,M),
	(
	    member( (TypeName,PredAri,Name) , InternalNames)
        -> 
	    true
	;
	    Name=TypeName
	),
	transform_name( Name , M , NameT ),
	format(S,":- regtype ~q/~w.~n~n",[NameT,PredAri]),
	transform_one_type_clause( Def , (TypeName , NameT) , DefT ),
	typedef_to_pred(DefT,NameT,Cls),
	transform_clause_list( Cls , M , ClsT ),
%	transform_types_clauses( ClsT , (TypeName , NameT) , ClsTT ),
	pretty_print(S,ClsT,[],_),
	nl(S),
	nl(S).


transform_one_type_clause( TH , (N,NT) , THT ) :-
	functor( TH , F , A ),
	(
	    F==N
	->
	    FT = NT
	;
	    FT = F
	),
	TH  =.. [_ |Args],
	THT =.. [FT|Args],
	transform_one_type_clause_args( A , THT , (N,NT) ).
transform_one_type_clause( TH , _ , TH ).

transform_one_type_clause_args( 0 , _ , _ ) :- !.
transform_one_type_clause_args( N , Pred , T ) :-
	N1 is N - 1 ,
	arg( N , Pred, ArgN ),
	transform_one_type_clause( ArgN , T , ArgNT ),
	setarg( N , Pred, ArgNT ),
	transform_one_type_clause_args( N1 , Pred , T ).

%------------------------------------------------------------------------

:- use_module(program(itf_db), [current_itf/3]).

write_headers( S , Mod ) :-
	% exports
        findall( F/A , 
	         (pred_spec(exported(Mod), Mod, F, A),
		  atom_concat( Mod, ':' , Mod2p ),
		  atom_concat( Mod2p , F , MF ),
		  % if imported and exported => reexported 
		  % ==> no need to appear in exported list
		  current_itf( defines , MF , A )
		 )
		 , E_List ),
	print_header( Mod , S , E_List ),
	nl( S ).

print_header( user(_Mod) , S , _E_List ) :- 
	!,
	display( S , ':- use_package( assertions ).\n' ).
print_header( _Mod , S , E_List ) :-
	display( S , ':- module( _' ),
	% DTM: Note that module name should not contain
        % illegal characters
	(
	    E_List = [_|_] 
	->
	    display( S , ', [' ),
	    print_atom_list( E_List , S ),
	    display( S , ']' )
	;
	    display( S , ', [] ' )
	),
	get_packages_to_output( Packages ),
	
	display( S , ', [' ),
	print_atom_list( Packages , S ),
	display( S , '] ).\n\n' ).



print_atom_list( [ ] , _ ).
print_atom_list( [ A ] , S ) :-
	!,
	writeq( S , A ).
print_atom_list( [ A | As ] , S ) :-
	writeq( S ,  A ) ,
	display( S , ' , ' ),
	print_atom_list( As , S ).

pred_spec(T,N,F,A):-
	type_of_goal( T, G ),
	transform_body( G, N, GT0 ),
	dont_want_qualification( GT0, GT),
	functor( GT, F, A0 ),
	special( F, A0, A).

dont_want_qualification( _:G, G):- !.
dont_want_qualification( G, G).

special(this_module,2,1):- !.
special(_,A,A).

:- pop_prolog_flag( multi_arity_warnings ).
