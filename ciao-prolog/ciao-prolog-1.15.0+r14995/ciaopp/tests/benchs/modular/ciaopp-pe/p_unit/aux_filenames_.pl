:- module(aux_filenames_,[get_module_filename/3,just_module_name/2,is_library/1],[assertions]).

:- doc(title,"Auxiliary file name generation").

:- doc(author,"Jes@'{u}s Correas").

:- doc(module,"This module provides names for auxiliary files used
   during the execution of ciaopp. Depending on the value of several
   preprocess flags (e.g. @code{tmp_dir}, @code{asr_dir}), absolute
   paths are computed in some way. If a temporary directory is
   provided in those flags, a unique file name is generated
   automatically in order to avoid name clashes.").

:- use_module(library(filenames), [no_path_file_name/2, basename/2]).
:- use_module('..'(preprocess_flags_), [current_pp_flag/2]).
:- use_module(library(terms), [atom_concat/2]).

:- dynamic library_directory/1.
:- multifile library_directory/1.

:- doc(bug,"the get_module_filename/3 predicate name does not
	correspond to the real meaning. It needs a basename in second
	argument, whereas it seems that the module name is enough.").

get_module_filename(pl,Source,FileName):-
	!,
	absolute_file_name(Source,'','.pl','.',FileName,_,_).
get_module_filename(Type,Source,FileName):-
	absolute_file_name(Source,'','.pl','.',_,Base,_),
	get_extension_and_dir(Type,Ext,TmpDir),
	( TmpDir = source ->
	  atom_concat(Base,Ext,FileName)
	; unique_name(Base,Name),
	  atom_concat([TmpDir,'/',Name,Ext],FileName)
	).

get_extension_and_dir(asr,'.ast',TmpDir):-
	current_pp_flag(asr_dir,TmpDir).
get_extension_and_dir(reg,'.reg',TmpDir):-
	current_pp_flag(tmp_dir,TmpDir).
get_extension_and_dir(dump,'.dmp',TmpDir):-
	current_pp_flag(tmp_dir,TmpDir).


%% --------------------------------------------------------------------

unique_name(Base,Name):-
	atom_codes(Base,[0'/|BaseS]),
	unique_name_(BaseS,NameS),
	atom_codes(Name,NameS).

unique_name_([],[]).
unique_name_([0'.|Bs],[0'.,0'.|Ns]):-
	!,
	unique_name_(Bs,Ns).
unique_name_([0'/|Bs],[0'.|Ns]):-
	!,
	unique_name_(Bs,Ns).
unique_name_([X|Bs],[X|Ns]):-
	unique_name_(Bs,Ns).


%% --------------------------------------------------------------------

just_module_name(IM0,IM):-
	absolute_file_name(IM0,'','.pl','.',_,AbsBase,_),
	no_path_file_name(AbsBase,IM).

%% --------------------------------------------------------------------

:- pred is_library(+Base).

is_library(Base):-
	absolute_file_name(Base,'','.pl','.',AbsFile,_,_),
	library_directory(Path),
	absolute_file_name(Path,'','.pl','.',_,_,LibPath),
	(
	    atom_concat(_,'/',LibPath) ->
	    atom_concat(LibPath,_,AbsFile)
	;
	    atom_concat(LibPath,'/',LibPath1),
	    atom_concat(LibPath1,_,AbsFile)
	).

%% --------------------------------------------------------------------

file_name_path(Filename,Path):-
	atom(Filename),
	no_path_file_name(Filename,Name),
	atom_concat('/',Name,NameA),
	atom_concat(Path,NameA,Filename).

file_name_path(Filename,Path):-
	string(Filename),
	atom_codes(FilenameA,Filename),
	file_name_path(FilenameA,Path).

%% --------------------------------------------------------------------

add_final_slash([],[0'/]):- !.
add_final_slash([0'/],[]):- !.
add_final_slash([X|Xs],[X|Ys]):- !,
	add_final_slash(Xs,Ys).
add_final_slash(Dir,DirNoSlash):-
	atom(Dir),
	atom_codes(Dir,DirS),
	add_final_slash(DirS,DirNoSlashS),
	atom_codes(DirNoSlash,DirNoSlashS).

%% --------------------------------------------------------------------

:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+785,2004/10/26,14:59*54+'CEST'), "Fixed bug in
   is_library/1.  (Jesus Correas Fernandez)").

:- doc(version(1*0+581,2004/07/26,13:49*10+'CEST'), "calls to
   @pred{absolute_file_name/2} changed to @pred{absolute_file_name/7}.
   (Jesus Correas Fernandez)").

:- doc(version(1*0+547,2004/07/15,11:59*04+'CEST'), "Source file
   creation.  (Jesus Correas Fernandez)").

