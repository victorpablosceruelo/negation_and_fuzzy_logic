% (CiaoPP with reduced footprint)
% TODO: integrate with CiaoPP

:- module( mini_driver,
	[ module/1,
	  transform/1
	],
	[
	    assertions
	]).

%------------------------------------------------------------------------

:- doc(module,"This module provides the main entry points for a user.
      Its predicates will be available at the Ciao shell prompt as
      commands for guiding the preprocessing of programs.

      This is also the module that you have to modify to incorporate a
      new feature into CiaoPP. Add a clause for @tt{analyze/2} (and for
      @tt{analysis/1}) for a new analysis. Add a clause for @tt{transform/2}
      (and for @tt{transformation/1}) for a new program transformation.

      As an alternative, you can add clauses for @tt{analysis/4} and
      @tt{analysis/1} or @tt{transformation/4} and
      @tt{transformation/1}.  Since these predicates are multifile,
      you can do this in your own sources, in which case you don't
      need to modify this module. The files
      @tt{examples/Extending/myanalyzer.pl} and
      @tt{examples/Extending/myspecialyzer.pl} in the source directory
      are examples of this.").

%------------------------------------------------------------------------
% Basic modules.
%------------------------------------------------------------------------

% :- use_module(ciaopp(preprocess_flags),
% 	[ current_pp_flag/2,
% 	  set_pp_flag/2,
% 	  push_pp_flag/2,
% 	  pop_pp_flag/1]).
:- use_module(program(p_unit), 	[preprocessing_unit/3
	                        , program/2            ] ). 
:- use_module(program(itf_db), [curr_file/2]).

:- use_module(ciaopp(api(api_module)), [define_new_module/2
 	                                 , add_action/1        ] ).
:- use_module(ciaopp(api(api_base)), [get_module/2]).


%------------------------------------------------------------------------
% Preprocessing modules.
%------------------------------------------------------------------------
:- use_module(typeslib(typeslib), [undoall_types/0]).

:- use_module(program(unexpand), 
	[ 
% --- DTM: This can be done just after loading module...
% --- DTM: This will go to c_itf soon I guess
	  generate_unexpanded_data/1, % TODO: kludge?
	  clean_unexpanded_data/0     % TODO: kludge?
	]).


:- use_module(library(prolog_sys), [statistics/2]).


%------------------------------------------------------------------------
:- pred module(FileName) : atm(FileName)
	# "Reads the code of @var{FileName} and its preprocessing unit,
          and sets it as the current module.".

:- pred module(FileNameList) : list(FileNameList,atm)
	# "Reads the code of the list of file names @var{FileNameList} (and
	   their preprocessing units), and sets them as the current
	   modules.".

module([M|Ms]):-
	!,
	module_with_flag([M|Ms],time,_).
module(Module):-
	module_with_flag([Module],time,_).

module_with_flag(ModList,Flag,Info):-
	statistics(runtime,_),
	undoall_types,
	% load 
	absolute_file_names(ModList,AbsoluteNameList),
	(   AbsoluteNameList = [AbsoluteName] -> true
	;   AbsoluteNameList = AbsoluteName
	),
	inform_user( ['{Note: Loading current module from ' , AbsoluteName] ),
	get_module( AbsoluteName , M ),
	define_new_module( M , AbsoluteName ),
	preprocessing_unit(AbsoluteNameList,_Ms,E),
	( E == yes -> Info=[error|Info0] ; Info=Info0 ),
	% assert_initial_types, 
	statistics(runtime,[_,T1]),
	(Flag = notime ->
	    true
	;
	    display_list(['{loaded in ', T1, ' msec.}\n'])
	),
	Info0=[time(T1,[])],
	inform_user(['}']),
	curr_file( _, Mod ),
	clean_unexpanded_data,
	generate_unexpanded_data( Mod ).
%------------------------------------------------------------------------


transform_(Tr,Cls,Ds,Info):-
	transformation(Tr,Cls,Ds,Info).


%------------------------------------------------------------------------

:- multifile transformation/4.
:- multifile transformation/2.
:- multifile transformation/1.

:- push_prolog_flag(multi_arity_warnings,off).


transform(Trans):- var(Trans), !, transformation(Trans).
transform(Trans):- transform(Trans,_Info).


:- pred transform(+Trans,-Info) 

# "Same as transform(@var{Trans}) but returns information that can be
	used to check the results of the transformation.".

transform(Trans,Info):-
	transformation(Trans), !,
	curr_file(File,_),
	inform_user(['{Transforming ',File]),
	% --- New Transformations
%	program(Cls,Ds),
	add_action( Trans ),
	transformation(Trans,Info),
	inform_user(['}']).
transform(Trans,_Info):-
	inform_user(['{Not a valid program transformation: ',Trans,'}']),
	fail.


absolute_file_names([],[]).
absolute_file_names([M|Ms],[A|As]):-
	absolute_file_name(M,'_opt','.pl','.',A,_,_),
	absolute_file_names(Ms,As).

:- pop_prolog_flag(multi_arity_warnings).
