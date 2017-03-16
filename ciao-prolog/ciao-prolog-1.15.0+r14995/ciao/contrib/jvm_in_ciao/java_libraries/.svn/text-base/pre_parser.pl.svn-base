:- module(pre_parser,[pre_parse_package/1]).

:- use_module(library(system), [directory_files/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(filenames)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(compiler), [use_module/1, unload/1]).

:- use_module(library(jvm_in_ciao(classfile_reader(classfile_reader))), 
	[read_classfile/2]).

pre_parse_package(Path) :-
	prepare_fout(Path,FOut,PackName),
	write_header(FOut,PackName),
	directory_files(Path,List),
	format("File list: ~q\n",[List]),
	read_all_classfiles(List,Path),
	collect_class_terms(List,FOut),
	close(FOut).

prepare_fout(Path,FOut,Basename) :-
	no_path_file_name(Path,Filename),
	basename(Filename,Basename),
	atom_concat(Basename,'.pl',ModuleName),
	open(ModuleName,write,FOut).

write_header(FOut,PackName) :-
	format(FOut,":- module(~q,[class/8,interface/7],[assertions]).\n\n",[PackName]),
%	format(FOut,":- doc(title,"Pre-parsed java/lang library").",[]),
	format(FOut,":- discontiguous class/8, interface/7.\n\n",[]).

read_all_classfiles(List,Path) :-
	member(ClassFile,List),
	is_classfile(ClassFile),
	atom_concat([Path,'/',ClassFile],ClassFile_p),
	read_classfile(ClassFile_p,[f(on),r(on),c(on)]),
	fail.
read_all_classfiles(_,_).

is_classfile(ClassFile) :-
	atom_concat(_,'.class',ClassFile).

collect_class_terms(List,FOut) :-
	member(ClassFile,List),
	is_classfile(ClassFile),
	basename(ClassFile,ClassName),
	atom_concat([ClassName,'_class'],PLFile),
	load_unload(PLFile),
	class_or_interface(PLFile,ClassTerm),
	format(FOut,"~q.\n\n",[ClassTerm]),
	format("Finished pre-parsing of ~q.\n",[ClassFile]),
	fail.
collect_class_terms(_,_).

class_or_interface(PLFile,ClassTerm) :-
	PLFile:class(ClassTerm),!.
class_or_interface(PLFile,ClassTerm) :-
	PLFile:interface(ClassTerm),!.
class_or_interface(_,_) :-
	format("Error: No class nor interface term\n",[]).

load_unload(ModuleName):-
 	use_module(ModuleName).
load_unload(ModuleName):-
 	unload(ModuleName),!,fail.
