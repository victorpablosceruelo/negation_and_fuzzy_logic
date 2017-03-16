:- module(printer, [
		output/0,
		output/1,
		output/2,
		check_global_props/2
	    ],
	    [assertions, nativeprops, isomodes, api(ciaopp_api), hiord]).

:- use_module(library(system), [copy_file/3]).
:- use_module(library(filenames)).
:- use_module(library(lists)).

% :- use_module(program(clidlist),[inverse_rewrite_source_program/2,
%	                          inverse_rewrite_source_program_wk/2]). 
:- use_module(program(p_unit), [internal_predicate_names/1]).
% :- use_module(program(p_abs),[gen_registry_info/3,save_registry_info/2]).
% :- use_module(program(assrt_norm),[normalize_assertion/9]).
:- use_module(program(assrt_db), [assertion_body/7]).
:- use_module(program(itf_db),   [curr_file/2]).
:- use_module(program(clause_db),   [source_clause/3, clause_locator/2]).
:- use_module(program(unexpand),
	    [transform_clause_list/3,
% 		transform_body/3,
		transform_name/3
% --- DTM: This can be done just after loading module...
% --- DTM: This will go to c_itf soon I guess
	    ]).
%:- use_module(infer(infer_db),[domain/1]).
%:- use_module(infer(infer_dom),[non_collapsable/1]).
%:- use_module(infer(prepare_ai_output),
%	[ cleanup_output/1,
%	  output_program/3,
%	  prepare_ai_output/4
%	]).

:- use_module(ilciao(java_interface), [java_output/1]).
:- use_module(library(pretty_print),  [pretty_print/4]).
:- use_module(library(messages),      [error_message/2]).
:- use_module(library(odd),           [setarg/3]).
:- use_module(library(terms),         [atom_concat/2]).
:- use_module(typeslib(typeslib),
	    [get_required_types/1, typedef_to_pred/3]).
:- use_module(library(format), [format/3]).

:- doc(title, "Preprocessing Output").

:- doc(module, "This module saves the results produced by CiaoPP
   analysis and transformations").

% :- include(engine(builtin_exports)).
% :- include(engine(builtin_modules)).
% builtin_module(_):- fail.
% builtin_export(_ModName,_F,_A,_Meta):- fail.

%------------------------------------------------------------------------
% DUMP MODULE.
%------------------------------------------------------------------------
:- reexport(program(p_dump)).

%------------------------------------------------------------------------

% Solved already
% :- doc(bug,"Some assertions (for predicates with no clauses) might
%	be missing in the output.").
% :- doc(bug,"Multifile and meta_predicates declarations are not printed,
%	and initialization, and on_abort, and....").

% Solved
% :- doc(bug,"1. Should not print required types if they are already
% 	predicates in the source.").
% :- doc(bug,"2. Names of required types should not clash with names
%	already visible to the current module.").
% :- doc(bug,"3. Imports from engine() modules are not printed: this
%	might be wrong.").
% :- doc(bug,"4. An [assertions] package is printed: this might be wrong.
%        Idem with [regtypes].").
% :- doc(bug,"5. A use_module(library(assertions(native_props))) is printed:
%	this IS wrong!").
% :- doc(bug,"6. Operators should be activated to print.").
% :- doc(bug,"7. comp and prop assertions are lost in the output.").
:- doc(bug, "8. Printing a slightly big program takes too long!. This is 
	probably due to calling type simplification too many time...").
:- doc(bug, "9. It should be possible to decide which properties should be 
	printed when showing analysis results. For example, I may not be interested 
        in arguments being var but only on whether they are ground.").
:- doc(bug, "10. When showing predicate level analysis information, 
	normalization of the completes is not required if there is only one 
        complete for the predicate").
%% :- doc(bug,"11. When showing program point level analysis information and 
%% 	collapse_ai_versions is turned off, info should not be collapsed 
%%         but instead shown as different possibilities").
%% :- doc(bug,"12. Unexpand module names in meta-arguments. This shows
%% 	in list(A,'basic_props:atm') in e.g. analisis.pl. Also in true(G)
%%         for the pp_info of an analysis.").
%% :- doc( bug, "13. When Asseritiong Body has ([A];[B]), A and B are
%%                   not well printed. Look at:
%%                   ['term_typing:var'(X)];['term_typing:var'(X)]." ).

%% :- doc( bug , "11. TODO:  _opt.pl link" ).

%% :- doc( bug , "12. The following comment
%%                    :- doc(title,"Term input").  
%%                    is written by output/1 as
%%                    :- doc(title,[84,101,114,109,32,105,110,112,117,116])"). 

%------------------------------------------------------------------------

:- pred output # "Outputs the current Module preprocessing state to a
   file named Module@tt{_opt.pl}, where Module is the current
   module.".

output :-
% 	findall( X , action( X ) , L ),
% 	% if this pred fails then there is no module loaded!
% 	concat_all( L , File ),
% 	atom_concat(File,'_co.pl',OptFile),
	get_output_name(OptFile, BaseFile),
	output(OptFile),
% create the link now
%	L = [ Path | _ ],
	file_name_extension(OptFile, _, Extension),
	atom_concat([BaseFile, '_co', Extension], COFile),
	get_module(BaseFile, FileWOExt),
	atom_concat(DirPath, FileWOExt, BaseFile),
% DTM: This is something "stupid": if COFile = OptFile,
%      then system does "ln -s file_co.pl file_co.pl"
%      so a cycle is created :((((
	(
	    \+ (COFile = OptFile)
	->
%	    del_file_nofail( COFile ),
	    atom_concat(DirPath, RelOptFile, OptFile),
	    copy_file(RelOptFile, COFile, [overwrite, symlink])
	;
	    true
	).


:- pred output(+Output)
# "Outputs the current module preprocessing state to a file
           @var{Output}.".

output(File) :-
	output(File, [print_written_file]).


:- pred output(+Output, +Opts)
# "Outputs the current module preprocessing state to a file
           @var{Output}. @var{Opts} can be: 
@begin{itemize}
@item @tt{print_written_file}: print a message telling the output filename.
@end{itemize}".

output(File, Opts) :-
	internal_output(File, EFile),
	!,
	( member(print_written_file, Opts)
	->
	    display('{written file '),
	    display(EFile),
	    display('}'),
	    nl
	; true ).
output(File, _) :-
	error_message("generating output of file ~w", [File]),
	fail.



internal_output(File, FileExt) :-
%	push_prolog_flag( write_strings , on ),
	output_extension(File, Ext),
	!,
	put_extension(File, Ext, FileExt),
	open(FileExt, write, Stream),
	( output_by_ext(Ext, Stream)
	-> close(Stream)
	; close(Stream), fail ).


% put extension if it does not have it
put_extension(File, Ext, FileExt) :-
	atom_concat('.', Ext, PExt),
	( atom_concat(_, PExt, File)
	-> FileExt = File
	; atom_concat(File, PExt, FileExt)
	).


:- pred output_by_ext(Ext, File) : (atm(Ext), sourcename(File))

# "Write asserted Ciao module to file @var{File} if @var{Ext} is
@tt{pl}.".


output_by_ext(java, Stream) :-
% from complete/7 to assertions
	analysis_info_to_assertions,
	java_output(Stream).
%	output_by_ext( pl , Stream ).

output_by_ext(pl, Stream) :-
% 	curr_file(_, Module),
% 	write_headers(Stream, Module),
	write_headers(Stream),
% call api
	print_program(Stream),
	write_types(Stream).


write_headers(Stream) :-
	get_packages_to_output(Packages1),
	(
	    current_fact(source_clause(Key,
		    directive(module(Module, Exports, Packages0)), Dict), Ref) ->
	    difference(Packages1, Packages0, Packages2),
	    append(Packages0, Packages2, Packages),
	    Body = module(Module, Exports, Packages)
	;
	    current_fact(source_clause(Key,
		    directive(module(Module, Exports)), Dict), Ref) ->
	    ( Packages1 = [] -> Body = module(_, Exports)
	    ; Body = module(Module, Exports, Packages1)
	    )
	) ->
	Direc = direc${ref => Ref,
	    key => Key,
	    body => Body,
	    dic => Dict,
	    locator => Loc},
	clause_locator(Key, Loc),
	api_write(Stream, Direc)
    ;
	member(Package, Packages1),
	pretty_print(Stream, directive(use_package(Package)), [], []),
	fail
    ;
	true.

%% --- if we use the _opt.pl option
% link_output(LinkName,OutFileName):-
% 	( file_exists(LinkName)
% 	-> delete_file(LinkName)
% 	 ; true
% 	),
% 	atom_concat('ln -s ',OutFileName,Comm0),
% 	atom_concat(Comm0,' ',Comm1),
% 	atom_concat(Comm1,LinkName,Comm),
% 	system(Comm).







% create_ordered_program( [] , A , A ).
% create_ordered_program( [K|Ks] , A , [Cl|Cls] ) :-
% 	member_and_remove( Cl:K , A , AR ),
% 	!,
% 	create_ordered_program( Ks , AR , Cls ).
% create_ordered_program( [_|Ks] , A , Cls ) :-
% 	create_ordered_program( Ks , A , Cls ).


% --- DTM: Future...
% create_ordered_program( [K|Ks] , A , Cls ) :-
% 	error_message( "Internal Error: create_ordered_program: \
% Key ~w does not exist in the asserted clauses" , [K] ),
% 	create_ordered_program( Ks , A , Cls ).


% member_and_remove( [] , [] , [] ).
% member_and_remove( A , [A|Bs] , Bs ) :- !.
% member_and_remove( A , [B|Bs] , [B|Cs] ) :-
% 	member_and_remove( A , Bs , Cs ).




% --- DTM: THIS HAS TO BE A HOOK
write_types(S) :-
	get_required_types(Rules),
	nl(S),
	write_list_types(Rules, S).

write_list_types([],       _).
write_list_types([Rule|L], S) :-
	write_one_type(Rule, S),
	write_list_types(L, S).

write_one_type(typedef(::=(Pred, Def)), S) :-
	p_unit:internal_predicate_names(InternalNames),
	functor(Pred, TypeName, Ari),
	PredAri is Ari + 1,
	curr_file(_, M),
	(
	    member((TypeName, PredAri, Name), InternalNames)
	->
	    true
	;
	    Name=TypeName
	),
	transform_name(Name, M, NameT),
	format(S, ":- regtype ~q/~w.~n~n", [NameT, PredAri]),
	transform_one_type_clause(Def, (TypeName, NameT), DefT),
	typedef_to_pred(DefT, NameT, Cls),
	transform_clause_list(Cls, M, ClsT),
%	transform_types_clauses( ClsT , (TypeName , NameT) , ClsTT ),
	pretty_print(S, ClsT, [], _),
	nl(S),
	nl(S).


transform_one_type_clause(TH, (N, NT), THT) :-
	functor(TH, F, A),
	(
	    F==N
	->
	    FT = NT
	;
	    FT = F
	),
	TH =.. [_|Args],
	THT =.. [FT|Args],
	transform_one_type_clause_args(A, THT, (N, NT)).
transform_one_type_clause(TH, _, TH).

transform_one_type_clause_args(0, _,    _) :- !.
transform_one_type_clause_args(N, Pred, T) :-
	N1 is N - 1,
	arg(N, Pred, ArgN),
	transform_one_type_clause(ArgN, T, ArgNT),
	setarg(N, Pred, ArgNT),
	transform_one_type_clause_args(N1, Pred, T).

/*

% Commented out: those predicates are used to print the header of a
% module, but it is more easy to recover it from the original module
% declaration. --EMM

%------------------------------------------------------------------------
:- use_module(library(write),      [writeq/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(program(unexpand),   [transform_body/3]).
:- use_module(program(p_unit),     [type_of_goal/2]).
:- use_module(program(itf_db),     [current_itf/3]).

write_headers(S, Mod) :-
% engine default imports
%	findall( engine(M) , builtin_module(M), B_List ),
%	required_engine(B_List, Flag),
% exports
	findall(F/A,
	    ( pred_spec(exported(Mod), Mod, F, A),
		atom_concat(Mod,   ':', Mod2p),
		atom_concat(Mod2p, F,   MF),
% if imported and exported => reexported 
% ==> no need to appear in exported list
		current_itf(defines, MF, A)
	    ), E_List),
	print_header(Mod, S, E_List),
	nl(S).

print_header(user(_Mod), S, _E_List) :-
	!,
	display(S, ':- use_package( assertions ).\n').
print_header(_Mod, S, E_List) :-
	display(S, ':- module(_'),
% DTM: Note that module name should not contain
% illegal characters
%	atom_concat( '_' , Mod , Mod2 ),
%	displayq( S , Mod2 ),
	(
	    E_List = [_|_]
	->
	    display(S, ', ['),
	    print_atom_list(E_List, S),
	    display(S, ']')
	;
	    display(S, ', []')
	),
	get_packages_to_output(Packages),
	display(S, ', ['),
	print_atom_list(Packages, S),
	display(S, ']).\n\n').

print_atom_list([],  _).
print_atom_list([A], S) :-
	!,
	writeq(S, A).
print_atom_list([A|As], S) :-
	writeq(S, A),
	display(S, ', '),
	print_atom_list(As, S).

pred_spec(T, N, F, A) :-
	type_of_goal(T, G),
	transform_body(G, N, GT0),
	dont_want_qualification(GT0, GT),
	functor(GT, F, A0),
	special(F, A0, A).

dont_want_qualification(_:G, G) :- !.
dont_want_qualification(G,   G).

special(this_module, 2, 1) :- !.
special(_,           A, A).

*/

check_global_props(In, Out) :-
	assertion_body(Pred, Compat, Call0, Succ, Comp0, Comm, In),
	compact_props(Call0, compact_calls_prop, Call),
	compact_props(Comp0, remove_first_argument, Comp1),
	compact_props(Comp1, compact_global_prop, Comp),
	assertion_body(Pred, Compat, Call, Succ, Comp, Comm, Out).

:- meta_predicate compact_props(?, pred(2), ?).
compact_props([],   _,   []) :- !.
compact_props([A0|B0], CompactProp, [A|B]) :- !,
	compact_props(A0, CompactProp, A),
	compact_props(B0, CompactProp, B).
compact_props(A, CompactProp, B) :-
	CompactProp(A, B).

remove_first_argument(M:A, M:B) :-
	!,
	remove_first_argument(A, B).
remove_first_argument(A, B) :-
	A =.. [F, _|Args],
	!,
	B =.. [F|Args].
remove_first_argument(A, B) :-
	A =.. [B].

% TODO: compact_global_prop/2 is a hook, and its implementation
% TODO: for cost properties must not be implemented here, but in a
% TODO: separated module (perhaps resources ???). --EMM

% :- multifile custom_compact_global_prop/2.

:- use_module(library(resdefs(rescostfunc)), [compact_cf/3, compact_size/3]).

compact_global_prop(cost(Rel, Ap, Type, Res, _, IF, CFN), Cost) :-
	compact_cf(CFN, IF, CF),
	compact_cost(Rel, Ap, Type, Res, CF, Cost),
	!.
compact_global_prop(C, C).

compact_calls_prop(intervals(_, G, _, L), intervals(S, L)) :-
	compact_size(G, _, S), !.
compact_calls_prop(A, A).

compact_cost(rel, Ap, Type, Res, CF, RelCost) :-
	compact_rel_cost(Type, Ap, Res, CF, RelCost).
compact_cost(abs, Ap, Type, Res, CF, AbsCost) :-
	compact_abs_cost(Type, Ap, Res, CF, AbsCost).

compact_rel_cost(call, Ap, Res, CF, rel_cost(Ap, Res, CF)) :- !.
compact_rel_cost(Type, Ap, Res, CF, rel_cost(Ap, Type, Res, CF)).

compact_abs_cost(call, Ap, Res, CF, cost(Ap, Res, CF)) :- !.
compact_abs_cost(Type, Ap, Res, CF, cost(Ap, Type, Res, CF)).
