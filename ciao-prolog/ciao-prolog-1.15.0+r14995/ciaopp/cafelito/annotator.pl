:- module(_,	
	 %  _,
            [cafelito_module/2,cafelito_module/1,cafelito_output/1],
	    [api(ciaopp_api)]).


:- use_module(library(messages)).
:- use_module(library(write)).
:- use_module(library(strings)).
:- use_module(library(file_utils)).
:- use_module(library(vndict)).
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(ciaopp(preprocess_flags), [set_pp_flag/2]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(filenames), [file_name_extension/3]).

:- use_module(cafelito(clause_generator), 
                                    [option/1,cafelito2ciao/3,java_name_2_ciao_name/2]).



:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
% issue_debug_messages(annotator).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   CiaoPP loads the module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- push_prolog_flag(multi_arity_warnings,off).

:- pred cafelito_module(Java_File,Options):
	(sourcename(Java_File),list(Options,clause_generator:option))
# "Load file a Java file @var{Java_File} into CiaoPP as it was a Ciao
  file. @var{Options} is a list of options to change the normal
  behaviour (see @pred{clause_generator:option/1})".

cafelito_module(Java_File,Options):-
   set_pp_flag(prog_lang,java),
   cafelito2ciao(Java_File,Options,Ciao_Module),
   ciaopp_load(Ciao_Module).

cafelito_module(Java_File):-
   cafelito_module(Java_File,[]).

:- pop_prolog_flag(multi_arity_warnings).




:- pred ciaopp_load(Module):
	gnd(Module)
  #"Load module @var{Module} into CiaoPP database. It's the
    Cafelito counterpart for @pred{driver:module/1}".

ciaopp_load(Module) :-
   api_format(Module,API_Module),	
   API_Module = module(Name,Exported,Imported,Asserts,Clauses),
   debug_message("~w",[API_Module]),
   add_exported_predicate(Exported,Name),
   add_package_to_output(Imported),
   load_related_modules([engine(arithmetic),
                         library(nativeprops),
			 library(resdefs(resources_decl))],Name),
   add_assertions(Asserts),
   add_clauses(Clauses).



:- pred api_format(Ds,API_Ds)
	: (gnd(Ds),var(API_Ds))
        => gnd(Ds)
# "Change internal representation of a module (see
  clause_generator_types) to API compliant format. @var{Ds} represents
  a node in the internal tree while @var{API_Ds} is the API equivalent
  for that node (see api/api_internal_dec)".

api_format(Module,API_Module):-
   Module = module(Name,Exported,Imported,Asserts,Clauses),
   !,
   api_format(Asserts,API_Asserts),
   api_format(Clauses,API_Clauses),
   API_Module = module(Name,Exported,Imported,API_Asserts,API_Clauses).

api_format([],[]).

api_format([C|RC],[AC|RAC]):-
   C  = cls(Head,Body,Loc,Dict),
   !,
   api_format(Loc,API_Loc),
   AC = cls${
	      head     => Head,	
	      body     => Body,
	      locator  => API_Loc,
	      dic      => Dict
	    },
   api_format(RC,RAC).	    
api_format([A|RA],[AA|RAA]):-
  A   = as(Head,Status,Type,Compat,Call,Succ,Comp,Loc,Dic,Comment),
  !,
  api_format(Loc,API_Loc),
  AA  = as${
	     head    => Head,
	     status  => Status,
	     type    => Type,
	     compat  => Compat,
	     call    => Call,
	     succ    => Succ,
	     comp    => Comp,
	     locator => API_Loc,
	     dic     => Dic,
	     comment =>Comment
	   },
   api_format(RA,RAA).	    

api_format(Loc,API_Loc):-
   Loc     = loc(Line_Begin,Line_End,File,Module),
   !,
   API_Loc = loc${
		  line_begin => Line_Begin,
		  line_end   => Line_End,
		  file       => File,
		  module     => Module
		 }.
api_format(Loc,API_Loc):-
   Loc     = unknown,
   loc_unknown(API_Loc). 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Output of analysis results
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cafelito_output(Output_Str) :-
   curr_file(Src_File,_),	
   file_name_extension(Src_File,Base_Name,'.java'),
   atom_concat(Base_Name,'.cst',CST_File),
   file_terms(CST_File,Terms),
   Terms = [Module|_],
   Module = module(_,Name,_,Preds),

   open(Src_File,read,Src_Str),
   current_output(Out),
   set_output(Output_Str),
   write_preds(Name,Preds,Src_Str),
   set_output(Out).
   
   %close(Output_Str).

write_preds(_,[],Src_Str):-
   % 'stream_to_string' automatically closes the Stream
   stream_to_string(Src_Str,Rest),
   write_string(Rest).
write_preds(Module,[Pred|RPred],Src_Str):-
   Pred = pred(Loc,Mods,_Variable,(_R,_W),User_Asserts,_),
   Mods = (_,_,real),!,
   Loc  = (Method_Start,Method_End),
   asserts_starting_line(User_Asserts,Method_Start,Asserts_Start),
   contents_until_line(Src_Str,Asserts_Start,Piece1),
   % skip user assertions
   contents_until_line(Src_Str,Method_Start,_),
   write_string(Piece1),
   write_pred(Module,Pred),
   contents_until_line(Src_Str,Method_End,Piece2),
   write_string(Piece2),
   write_preds(Module,RPred,Src_Str).
write_preds(Module,[Pred|RPred],Src_Str):-
   Pred = pred(_,Mods,_,_,_,_),
   Mods = (_,_,virtual),!,
   write_preds(Module,RPred,Src_Str).


asserts_starting_line([],L,L).
asserts_starting_line([As|_],_,Line):-   
   As     = (As_Loc,_,_,_,_),
   As_Loc = (Line,_).

write_pred(Module,Pred):-
   Pred     = pred(_,_,Variable,(R,W),_,_),
   Variable = var(_,Id),
   atom_concat([Module,':',Id],Pred_Name),
   length(R,Arity1),
   length(W,Arity2),
   Arity is Arity1 + Arity2,
   functor(Head,Pred_Name,Arity),
   get_all_assertions(Head,Asserts),
   pretty_print_asserts(Pred,Asserts).


pretty_print_asserts(_,[]):-
   !.	
pretty_print_asserts(Pred,Asserts):-

   member(Assert,Asserts),
   pretty_print_assert(Pred,Assert),
   fail.
pretty_print_asserts(_,_).



pretty_print_assert(Pred,Assert):-
   Assert = as${ 
		type   => Type,
                status => Status,
		head   => Head,
 	        dic    => VNs 
	       },
   (VNs = no ->
      create_varnames(Pred,Head,New_VNs);
      New_VNs = VNs
   ), 
   rename_vndict(Pred,New_VNs,Java_VNS),
   varnamesl2dict(Java_VNS,Dict),
   get_fields(Assert,Type,Fields),
   (Fields \== [], Fields\== [[]] ->
      get_cml_type(Type,CML_Type),
      rename(Fields,Dict),
      print_annotation(Status,CML_Type,Fields)
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
   Conditions = [Call,Succ,Comp].	       


get_cml_type(entry,  entry).
get_cml_type(calls,  requires).
get_cml_type(success,ensures).
get_cml_type(pred,   pred).
get_cml_type(comp,   comp).


print_annotation(_,_,[]):-
   !.
print_annotation(Status,pred,Fields):-
   !,
   write_string("\n/*@\n"),
   Fields = [Call,Succ,Comp],
   Pref = '  @ ',
   write(Pref),
   write(Status),nl,  
   (Call \== [] ->
       write('  @   if'),nl,
       print_facts(Call),nl
       ;
       true
   ),
   (Succ \== [] ->
      write('  @    {'),nl,
      print_facts(Succ),nl,
      write('  @    }'),nl
      ;
      true
   ),
   (Comp \== [], Comp\==[[]] ->
      write('  @   + '),nl,
      print_facts(Comp),nl
      ;
      true
   ),
   write('  @ ; '),nl,
   write_string("  @*/"),nl.
print_annotation(Status,Type,Facts) :-
   write_string("\n/*@\n"),
   Pref = '  @ ',
   write(Pref),
   write(Status),  
   write(' '),
   write(Type),nl,
   print_facts(Facts),
   write(' ; '),nl,
   write_string("  @*/"),nl.

print_facts([Pred|RPred]):-
   list(Pred),
   !,
   append(Pred,RPred,R),
   print_facts(R).
print_facts([Pred|RPred]):-
   Pref1 = "  @      ",
   write_string(Pref1),
   unexpand(Pred,Just_Pred),
   write(Just_Pred),
   member(Another_Pred,RPred),
   Pref2 = " && ",
   write_string(Pref2),
   unexpand(Another_Pred,Just_Another_Pred),
   write(Just_Another_Pred),
   fail.
print_facts(_).   



create_varnames(Pred,Head,Dict):-
   Pred = pred(_,_,_,(R,W),_,_),	
   append(R,W,Pars),
   Head =.. [_|Args],
   create_varnames_(Pars,Args,Dict).

create_varnames_([],[],[]).
create_varnames_([P|RP],[A|RA],[D|RD]):-
   P = var(_,Name),
   D = '='(Name,A),
   create_varnames_(RP,RA,RD).


rename_vndict(_,[],[]).
rename_vndict(Pred,[H|T],[J|RJ]):-
   Pred = pred(_,_,Var,(R,_W),_,_),	
   Var = var(_,Pred_Name),
   H = '='(Ciao_Var,Variable),
   java_name_2_ciao_name(Java_Var,Ciao_Var),
   
   (member(var(_,Java_Var),R) ->
      J = '='(Java_Var,Variable);

      (Java_Var = Pred_Name ->
         J = '='('\\result',Variable);  
         J = '='(Java_Var,Variable)	
      )
   ),
   rename_vndict(Pred,T,RJ).
   


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


