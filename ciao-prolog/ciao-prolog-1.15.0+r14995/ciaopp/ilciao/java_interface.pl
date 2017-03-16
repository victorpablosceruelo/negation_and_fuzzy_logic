:- module(_,
%	_,
 	[
 	 get_ilciao_output_file/1,
 	 get_class_name_from_file/2,
 	 java_create/2,   
 	 java_generate_ciao/1,   
 	 java_invoke/2,   
 	 java_output/1,   
 	 java_print_object/1,   
 	 java_start_jvm/0,
 	 java_stop_jvm/0
 	],
        [ciaopaths, dcg, fsyntax, api(ciaopp_api), hiord]).

:- use_module(cafelito(clause_generator), [java_name_2_ciao_name/2]).
:- use_module(cafelito(just_java_parser)).
:- use_module(library(file_utils)).
:- use_module(library(filenames)).
:- use_module(library(javall(javart)), 
	[
	 java_create_object/2,
	 java_invoke_method/2,
	 java_start/1,
	 java_stop/0
	]).
:- use_module(library(lists)).
:- use_module(library(messages), 
	[
	 simple_message/1,
	 error_message/2
	]).
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(library(strings)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(vndict), 
	[
	    rename/2,
	    varnamesl2dict/2
	 ]).
:- use_module(library(write)).

% name of file containing the Java libraries needed (relative to ilciao)
lib_paths_file_name('ant.properties').

java_start_jvm:-	
	get_classpath(ClassPath),
	catch(java_start(ClassPath),java_exception(_),true).

java_stop_jvm:- 
	java_stop,
	!.
java_stop_jvm.

java_print_object(Object):-
	java_invoke_method(Object,toString(String)),
	write(String).

java_invoke(Object,Method):-
	catch(java_invoke_method(Object,Method), Java_exception, 
	java_invoke_error_catch(Java_exception, Method)),
	 !.
java_invoke_error_catch(java_exception(Msg),Method):-
	(Msg ='java_invoke_method: method not found' ->
	     error_message("Unknown Java method (probably you mispelled the name,  there is an arity mismatch, or the visibility of the Java method needs to be relaxed: ~q",[Method]),
	     !,
	     fail
	;
	    throw(java_exception(Msg))
	).

java_create(Class,Object):-
	catch(java_create_object(Class,Object),java_exception(_),
	(error_message("Error while creating object of class: ~q",[Class]),fail)).


:-push_prolog_flag(multi_arity_warnings,off).
% java_generate_ciao:-
% 	java_generate_ciao(NULL).
java_generate_ciao(MainClass):-
	java_create('soot.ciao.CiaoInterface',CiaoInterface),	
	current_fact(dir(FileDir)),
	simple_message("Generating intermediate representation ... "),
        java_invoke(CiaoInterface,generateCiao(FileDir,MainClass,_)),
	!.
:-pop_prolog_flag(multi_arity_warnings).


% TODO: replace by: 
% DONE (28-Feb) 
% To retrieve the output file, we need to know the source path loaded
% Once we know it, return the same path but ended in .pl
% We shouldn't return the outputs as below, always in the same dir.
% Output file name should be package.class_name.pl
get_ilciao_output_file(Out):-
% Old Version
%	get_ilciao_directory(Dir),
%	atom_concat(Dir,'/ciao/usr.pl',Out).
	current_fact(dir(Dir)),	
	current_fact(main_class(FileName)),
	atom_concat([Dir,'/',FileName,'.pl'],Out).
	

% TODO: replace by: 
% NOTE: I do not see how to do this (JNL)
% given that in ciao/lib/bundle_registry/bundless/ciaopp_src_auto.pl
% we have all the synonims, we should be able to retrieve from there instead
% of hardcoding it as below
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
get_ilciao_directory(Dir):-
	% TODO: expand an alias path instead? (JFMC)
	fsR(bundle_src(ciaopp),CiaoppPath),
 	atom_concat(CiaoppPath,'/ilciao',Dir).

% TODO: replace by: 
% DONE (28-Feb) 
% -open the given file
% - parse the package
% -return package + file name (except for .java)

:- data path/1.
:- data dir/1.
:- data main_class/1.

set_path(Path) :-
	(retract_fact(path(_)) -> true ; true),
	asserta_fact(path(Path)).

set_main_class(Main_Class) :-
	(retract_fact(main_class(_)) -> true ; true),
	asserta_fact(main_class(Main_Class)).

set_dir(Dir) :-
	(retract_fact(dir(_)) -> true ; true),
	asserta_fact(dir(Dir)).



% MainClass is either pack_name1.pack_name_2. .... .pack_namen.BaseName
% or Basename (if no packages)
get_class_name_from_file(Path,Main_Class):-
% Old version
% 	get_ilciao_directory(Dir),
% %	atom_concat(Dir,'/examples',Examples_Dir),
% 	Examples_Dir = Dir,
% 	atom_concat([Examples_Dir,'/',MC,'.java'],Path),
% 	atom_codes(MC,SMC),
% 	replace_slash_by_dot(SMC,SMC_1),
% 	atom_codes(Main_Class,SMC_1).
	file_to_string(Path,String),
	no_path_file_name(Path,FileName),
	file_name_extension(FileName,Base0,_Extension),
	set_prolog_flag(write_strings,on),
	mini_parser(String,Package),
	( var(Package) ->
	  Main_Class = Base0
	;
	  append(Package,".",Package0),
	  atom_codes(Base0,Base),
	  append(Package0,Base,Main_Class0),
  	  atom_codes(Main_Class,Main_Class0)
        ),
	set_path(Path),
	set_main_class(Main_Class),
	get_file_directory(Path,Dir),
	set_dir(Dir).

get_file_directory(Path,DirPath):-
	atom_codes(Path,Path_s),
	no_path_file_name(Path_s,FileName_s),
	atom_codes(FileName,FileName_s),
	atom_concat([DirPath,'/',FileName],Path).


mini_parser(String,Package) :-
	package_directives(Package,String,_).
mini_parser(_String,_).

package_directives(Package) -->	
        zero_or_more([or(comment,sp)]),	keyword("package"), pack_name(Package).

pack_name(Package,S,S0):-
	pack_name_(L,S,S0),
	concat_package(L,Package).

concat_package([X],X).
concat_package([X|Xs],Res2):-
	concat_package(Xs,Res),
	append(".",Res,Res1),
	append(X,Res1,Res2).

pack_name_([Name],S,S2) :-
	identifier(Name,S,S1),
	separator(";",S1,S2).
pack_name_([Name],S,S3) :-
	identifier(Name,S,S1),
	comment(S1,S2),
	separator(";",S2,S3).
pack_name_([Name|Rest],S,S3) :-
	identifier(Name,S,S1),
	separator(".",S1,S2),
	pack_name_(Rest,S2,S3).
	
/*
replace_slash_by_dot([],[]):-
	!.
replace_slash_by_dot([47|T],[46|R]):-
	!,
	replace_slash_by_dot(T,R).
replace_slash_by_dot([H|T],[H|R]):-
	replace_slash_by_dot(T,R).
*/

get_classpath(Classpath):-
	get_ilciao_directory(Dir),
	lib_paths_file_name(Lib_File),
	atom_concat([Dir,'/',Lib_File],File),
	open(File,read,Stream),
	get_paths(Stream,[],Paths),
	add_colon(Paths,Classpath),
	close(Stream).

get_paths(Stream, Paths, NewPaths):-
	get_line(Stream,Line),
	( Line == end_of_file ->
	  NewPaths = Paths
        ;
	  add_file(Line,Paths,Tmp),
	  get_paths(Stream,Tmp,NewPaths)          
        ).

% the first case is necessary if the comment line includes '='
add_file(Line,Paths,Paths):-
	append("#",_,Line),
	!.
add_file(Line,Paths,[Path|Paths]):-
	append(Lhs,RelativePath,Line),
	append(_,"=",Lhs),
	get_ilciao_directory(Dir),
	atom_codes(Dir,DirString),
	append(DirString,"/",DirString1),
	append(DirString1,RelativePath,Path),
   	!.
add_file(_,Paths,Paths).

add_colon([],[]).
add_colon([Path],Path):-
	!.
add_colon([Path|Rest],ClassPath):-
	append(Path,":",Tmp),
	add_colon(Rest,RestTmp),
	append(Tmp,RestTmp,ClassPath).

java_list_to_prolog_list(Java_List,Prolog_List):-
	java_invoke(Java_List,size(Size)),
	java_list_to_prolog_list_(0,Size,Java_List,Prolog_List).

java_list_to_prolog_list_(Size,Size,_Java_List,[]):-
	!.
java_list_to_prolog_list_(N,Size,Java_List,[H|T]):-
	java_invoke(Java_List,get(N,H)),
	N1 is N + 1,
	java_list_to_prolog_list_(N1,Size,Java_List,T).


java_output(Output_Stream) :-	
	curr_file(Src_File,_),	
	open(Src_File,read,Src_Stream),
	current_output(Current_Output_Stream),
	set_output(Output_Stream),
	java_create('soot.ciao.CiaoInterface',CiaoInterface),	
        java_invoke(CiaoInterface,
	    getEntryBlockMethodsSortedByLineNumber(BlockMethods)),
	java_invoke(BlockMethods,size(Size)),    
        write_methods(0,Size,BlockMethods,Src_Stream),
        set_output(Current_Output_Stream).

write_methods(S,S,_,Src_Stream):-
        stream_to_string(Src_Stream,Rest),   % 'stream_to_string' automatically closes the Stream
	nl,write_string(Rest).
write_methods(N,S,BMs,Src_Stream):-
	java_invoke(BMs,get(N,BM)),    
        java_invoke(BM,getStartingLineInSource(BM_Starting_Line)),
        contents_until_line(Src_Stream,BM_Starting_Line,Piece),
        write_string(Piece),
        write_method_assertions(BM),
        N1 is N + 1,
        write_methods(N1,S,BMs,Src_Stream).

% TODO: replace 'usr:' by the current module name
% DONE (28-Feb) 
write_method_assertions(BM):-
	java_invoke(BM,getBlockMethodName(BM_Name)),    
	java_invoke(BM,getFormalParameterCount(BM_Arity)),
	atom_codes(ABM_Name,BM_Name),
        atom_concat(['\'',AABM_Name, '\''],ABM_Name), 
	current_fact(main_class(Mod)),
        atom_concat([Mod,':',AABM_Name],QBM_Name2),
        functor(Head,QBM_Name2,BM_Arity),
        get_all_assertions(Head,Assertions),
        pretty_print_asserts(BM,Assertions).

pretty_print_asserts(_,[]):-
	!.	
pretty_print_asserts(BM, Assertions):-
	write_string("\n  /**\n"),
	member(Assertion,Assertions),
	pretty_print_assert(BM, Assertion),
        fail.
pretty_print_asserts(_,_):-
	write_string("\n   */").
	

pretty_print_assert(BM,Assert):-
   Assert = as${ 
		type   => Type,
                status => Status,
		head   => Head,
 	        dic    => VNs 
	       },
   (VNs = no ->
      create_varnames(BM,Head,New_VNs)
      ;
      New_VNs = VNs
   ), 
   rename_vndict(BM,New_VNs,Java_VNS),
   varnamesl2dict(Java_VNS,Dict),
   get_fields(Assert,Type,Fields),
   (Fields \== [], Fields\== [[]] ->
      get_java_assertion_type(Type,CML_Type),
      rename(Fields,Dict),
      print_annotation(BM,Status,CML_Type,Fields)
      ;
      true
   ),   
   fail.
pretty_print_assert(_,_).   


get_fields(Assert,entry,[Condition]):-
	Assert = as${ 
			call => Condition
		    }.
get_fields(Assert,calls,[Condition]):-
	Assert = as${ 
			call => Condition
		    }.
get_fields(Assert,success,[Condition]):-
	Assert = as${ 
			succ => Condition
		    }.
get_fields(Assert,comp,[Condition]):-
	Assert = as${ 
			comp => Condition
		    }.
get_fields(Assert,pred,Conditions):-
	Assert = as${ 
			call => Call,
			succ => Succ,
			comp => Comp
		    },
		    change_global_props(Comp,Comp0),
		    Conditions = [Call,Succ,Comp0].

change_global_props( [] , [] ) :- !.
change_global_props( [A|AR] , [B|BR] ) :- !,
	change_global_props(A , B ),
	change_global_props(AR , BR ).
change_global_props(A , B ) :-
        remove_first_argument(A, B).

remove_first_argument(M:A, M:B):- !,
	remove_first_argument(A, B).
remove_first_argument(A, B):-
        A =.. [F,_|Args], 
        !,
        B =.. [F|Args]. 
remove_first_argument(A, B):-
        A =.. [B].


get_java_assertion_type(entry,  on_external_calls).
get_java_assertion_type(calls,  requires).
get_java_assertion_type(success,ensures).
get_java_assertion_type(pred,   pred).
get_java_assertion_type(comp,   comp).


print_annotation(_,_,_,[]):-
	!.
print_annotation(BM,Status,pred,Fields):-
	!,
	Fields = [Call,Succ,Comp],
	write('   * '),
	write(Status),nl,  
	(Call \== [] ->
	 write('   *   if ('),
	 print_facts(BM,Call),write(')')
	 ;
         true
	),
	(Succ \== [] ->
	 write('  {'),nl,
	 write('   *        '),
	 print_facts(BM,Succ),nl,
	 write('   *   }')
	 ;
	 true
	),
	(Comp \== [], Comp\==[[]] ->
	 nl,write('   *  && '),
	 print_facts(BM,Comp)
	 ;
        true
	).
print_annotation(BM,Status,Type,Facts) :-
	write('   *'),nl,write( '   * '),
	write(Status),  
	write(' '),
	write(Type),nl,
	write('   *    '),print_facts(BM,Facts),
	nl.


print_facts(BM,[Pred|RPred]):-
	list(Pred),
	!,
	append(Pred,RPred,R),
	print_facts(BM,R).
print_facts(BM,[Pred|RPred]):-
	unexpand(Pred,Just_Pred),
	write_fact(BM,Just_Pred),
	member(Another_Pred,RPred),
	Pref2 = " && ",
	write_string(Pref2),
	unexpand(Another_Pred,Just_Another_Pred),
	write_fact(BM,Just_Another_Pred),
	fail.
print_facts(_,_).   

	
write_fact(_BM,cost(_,UB,Resource,Expr)):-
	!,
	New_Fact = cost(UB,Resource,Expr),
	write(New_Fact).
write_fact(_BM,Fact):-
	write(Fact).

create_varnames(BM,Head,Dict):-
	java_create('soot.ciao.CiaoInterface',CiaoInterface),	
        java_invoke(CiaoInterface,getFormalParameterNames(BM,Formal_Params)),
	java_list_to_prolog_list(Formal_Params,FPs),
	Head =.. [_|Head_Vars],     
	create_varnames_(FPs,Head_Vars,Dict).

create_varnames_([],[],[]).
create_varnames_([Formal_Param|RFP],[Head_Var|RHead_Var],[D|RD]):-
	atom_codes(AFormal_Param,Formal_Param),
	D = '='(AFormal_Param,Head_Var),
	create_varnames_(RFP,RHead_Var,RD).


rename_vndict(_,[],[]).
rename_vndict(BM,[H|T],[J|RJ]):-
	H = '='(Ciao_Var,Variable),
	java_name_2_ciao_name(Java_Var,Ciao_Var),
	J = '='(Java_Var,Variable),	
	rename_vndict(BM,T,RJ).
   

unexpand(Qualified_Pred,Pred):-
	Qualified_Pred =.. [Funct|Rest], 	
	get_mod_pred(Funct,_,Just_Pred),!,
	Pred =.. [Just_Pred|Rest].
unexpand(Pred,Pred).


contents_until_line(Stream,Limit,Contents):-
	line_count(Stream,Lines),
	Real_Limit is Limit-1,
	contents_until_line_(Stream,Lines,Real_Limit,[],Contents).

contents_until_line_(_,Lines,Limit,C,C):-
	Lines >= Limit,!.	
contents_until_line_(Stream,Lines,Limit,Contents_So_Far,Contents):-
	get_line(Stream,Line),
	append(Contents_So_Far,"\n",Tmp1),
	append(Tmp1,Line,New_Contents_So_Far),
	New_Lines is Lines + 1,
	contents_until_line_(Stream,New_Lines,Limit,New_Contents_So_Far,Contents).

