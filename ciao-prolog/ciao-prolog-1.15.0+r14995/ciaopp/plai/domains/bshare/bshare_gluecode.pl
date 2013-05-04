:- module(_,
	[% operations
	 sh2nsh/3,
	 nshproject/4,
	 nshamgu/4,
	 nshunion/4,
	 nshcompare/3,
	 nshaugment/3,
	 nsh2sh/2,
	 process_response/0,
	 % paths
 	 posdb_path/3,
	 negdb_path/3
        ],[assertions]).

:- use_module(library(system), [shell/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(messages)).
:- use_module(library(strings), [get_line/2]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(domain(bshare(bshare_utils)), [concat_suffix/3]).
:- use_module(domain(bshare(config_paths))).


:- doc(author,"Jorge Navas").
:- doc(module,"This module implements all gluecode necessary to
   communicate @file{bshare.pl} with low-level operations in
   @file{low_level_ops/*}.").

:- doc(bug, "The names used for predicates and comments are
           obsolete.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sh2nsh(+Len,?DB,?NDB)                            
% Usage: % ./SH2NSH.pl len [shfilename [nshfilename]]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Converts a positive database (@var{DB}) in bits into a negative 
% database contained in @var{NDB}. @var{Len} is the number of variable 
% positions per string.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sh2nsh(Len,ShFile,NShFile):- 
	command(sh2nsh,Command),
	debug_message("Calling to ~q",[Command]),
	prepare_sh2nsh_args(Len,ShFile,NShFile,Command,Command_Args),
	shell(Command_Args,_Out).

prepare_sh2nsh_args(Len,ShFile,NShFile,Command,Command_Args):-
	atom_number(ALen,Len),
	atom_concat(Command,ALen,Command_Len),
	( var(ShFile) ->
	  Command_Args = Command_Len
        ;
	  current_pp_flag(bshare_option,Opt),
	  ( Opt == bSH -> 
	    atom_number(Mode,0),
            atom_concat([Command_Len,' ',ShFile,' ',NShFile,' ',Mode],
                         Command_Args)
          ; 
            ( Opt == tSH ->
	     atom_number(Mode,1),
             atom_concat([Command_Len,' ',ShFile,' ',NShFile,' ',Mode],
                         Command_Args)
            ;
     	     atom_concat([Command_Len,' ',ShFile,' ',NShFile],Command_Args)
            )
          )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sh2nsh(+NDB,+DB)                                                            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Converts a negative sharing groups @var{NDB} into a positive sharing groups 
% @var{DB}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nsh2sh(NShFile,ShFile):-
	command(crack,Command0),
	debug_message("Calling to ~q",[Command0]),
	atom_concat([Command0,NShFile,' > ',ShFile],Command2),
	shell(Command2,_Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nshproject(+Len,+X,?NDB,?NDB_proj)                             
% Usage: % ./NSHproject.pl len x [nshfilename [resultnshfilename]] | x
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @var{X} is a string of 0's and 1's of length @var{Len} such that a 1 
% specifies the bit positions to be extracted from @var{NDB}       
% (padded left with 0's to length @var{Len}                            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nshproject(Len,X,NShFile,ResultNSh):-
	command(project,Command),
	debug_message("Calling to ~q",[Command]),
	prepare_nshproject_args(Len,X,NShFile,ResultNSh,Command,Command_Args),
	shell(Command_Args,_Out).
	
prepare_nshproject_args(Len,X,NShFile,ResultNSh,Command,Command_Args):-
	( var(Len) ->
	  atom_concat([Command,X],Command_Args)
        ;
         ( var(NShFile) ->
	   atom_number(ALen,Len),
	   atom_concat([Command,ALen,' ',X],Command_Args)
         ;
           atom_number(ALen,Len),
	   atom_concat([Command,ALen,' ',X,' ',NShFile,' ',ResultNSh],Command_Args)
         )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nshamgu(?Len,+V,+T,?NDB)                                                    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The abstract unification between @var{V} and @var{T}. @var{V} is a string   
% of 0's and 1's of length @var{Len}, where a single 1 designates the         
% position of the variable @var{V} (padded left with zeros to length          
% @var{Len}. @var{T} is a string of 0's and 1's of length @var{Len}, where a  
% 1 represents the positions of the variables in the set @var{T} (padded left 
% with zeros to length @var{Len}.                                             
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nshamgu(Len,V,T,NShFile):-	
	command(amgu,Command),
	debug_message("Calling to ~q",[Command]),
	prepare_nshamgu_args(Len,V,T,NShFile,Command,Command_Args),
	shell(Command_Args,_Out).

prepare_nshamgu_args(Len,V,T,NShFile,Command,Command_Args):-
	( var(Len) ->
	  current_pp_flag(bshare_option,Opt),
	  ( Opt == bSH -> 
	    atom_number(Mode,0),
	    atom_concat([Command,V,' ',T,' ',Mode],Command_Args)
	  ; 
	      ( Opt == tSH ->
		atom_number(Mode,1),
		atom_concat([Command,V,' ',T,' ',Mode],Command_Args)
	      ;
		  atom_concat([Command,V,' ',T],Command_Args)
	      )
	  )
        ;
	  atom_number(ALen,Len),  
          ( var(NShFile) ->
	    atom_concat([Command,ALen,' ',V,' ',T],Command_Args)
          ;
	    atom_concat([Command,ALen,' ',V,' ',T,' ',NShFile],Command_Args)
          )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nshcompare(?Len,+NDB1,?NDB2)                                         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	  
% Succeed if @var{NDB1} and @var{NDB2} are equal               
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	  

nshcompare(Len,NShFile1,NShFile2):-
	command(compare,Command),
	debug_message("Calling to ~q",[Command]),
	prepare_nshcompare_args(Len,NShFile1,NShFile2,Command,Command_Args),	
	shell(Command_Args,_Out),!,
	process_response.

process_response:-
	files_path(Path),
	atom_concat(Path,'compare.txt',File),
	open(File,read,Stream),
	process_response_(Stream),
	close(Stream).


process_response_(Stream):-
	get_line(Stream,String),
	( String = end_of_file ->
	  warning_message("[NSHCompare]: end_of_file")
	;
	  ( number_codes(Res,String) ->
	    ( Res == 0 ->
	      !, fail
	    ;
		( Res == 1 ->  
		  true,!
		;
		    warning_message("[NSHCompare]:Invalid result ~q",[Res])
		)
	    )
	  ;
	      warning_message("[NSHCompare]:Invalid result ~q",[String])
	  )
	).
	    

prepare_nshcompare_args(Len,NShFile1,NShFile2,Command,Command_Args):-
	( var(Len) ->
	  ( var(NShFile2) ->
	    atom_concat([Command,NShFile1],Command_Args)
	  ;
	      current_pp_flag(bshare_option,Opt),
	      ( Opt == bSH -> 
		atom_number(Mode,0),
		atom_concat([Command,NShFile1,' ',NShFile2,' ',Mode],Command_Args)
	      ; 
		  ( Opt == tSH ->
		    atom_number(Mode,1),
		    atom_concat([Command,NShFile1,' ',NShFile2,' ',Mode],Command_Args)
		  ;
		      atom_concat([Command,NShFile1,' ',NShFile2],Command_Args)
		  )
	      )	  
	 )
	;
	    atom_number(ALen,Len),
	    atom_concat([Command,ALen,' ',NShFile1,' ',NShFile2],Command_Args)
        ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nshaugment(?Len,?N,?NDB)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adds all combinations of @var{N} positions to the end of each record. This 
% includes the case when len is zero.                            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nshaugment(Len,N,NShFile):-
	command(add,Command),
	debug_message("Calling to ~q",[Command]),
	prepare_nshadd_args(Len,N,NShFile,Command,Command_Args),
	shell(Command_Args,_Out).

prepare_nshadd_args(Len,N,NShFile,Command,Command_Args):-
	( var(Len) ->
	  ( var(N) ->
	    Command_Args = Command
          ;
	    atom_number(AN,N),
	    ( var(NShFile) ->
	      atom_concat([Command,AN],Command_Args)
            ;
	      atom_concat([Command,AN,' ',NShFile],Command_Args) 	
            )
          )
        ;
	   atom_number(ALen,Len),
	   atom_number(AN,N),
	  (var(NShFile) ->
	   atom_concat([Command,ALen,' ',AN],Command_Args)
	  ;
	   atom_concat([Command,ALen,' ',AN,' ',NShFile],Command_Args)   
          )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nshunion(?Len,+NDB1,?NDB2,?NDB_union)                                      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @var{NDB_union} is the negative union between @var{NDB1} and @var{NDB2}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nshunion(Len,NShFile1,NShFile2,ResultNShFile):-
	command(union,Command),
	debug_message("Calling to ~q",[Command]),
	prepare_nshunion_args(Len,NShFile1,NShFile2,ResultNShFile,Command,Command_Args),
	shell(Command_Args,_Out).

prepare_nshunion_args(Len,NShFile1,NShFile2,ResultNShFile,Command,Command_Args):-
	( var(Len) ->
	  ( var(NShFile2) ->
	    atom_concat([Command,NShFile1],Command_Args)
          ;
	    atom_concat([Command,NShFile1,' ',NShFile2,' ',ResultNShFile],Command_Args)
          )
        ;
	   atom_number(ALen,Len),
	   atom_concat([Command,ALen,' ',NShFile1,' ',NShFile2,' ',
	                ResultNShFile],Command_Args)
        ).
	      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AUXILIARY OPERATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
posdb_path(Id,rel,DB):-
	concat_suffix('SH',Id,DB).
posdb_path(Id,abs,DB):- 
	files_path(Base),
	concat_suffix('SH',Id,DB0),
	atom_concat(Base,DB0,DB).
negdb_path(Id,rel,DB):-
	concat_suffix('NSH',Id,DB).
negdb_path(Id,abs,DB):-
	files_path(Base),
	concat_suffix('NSH',Id,DB0),
	atom_concat(Base,DB0,DB).	

command(crack,Command):-
	current_pp_flag(bshare_option,tNSH),!,
	command_path(Path),
	atom_concat(Path,'crack_ndb.pl -b ',Command).
command(crack,Command):-
	current_pp_flag(bshare_option,tSH),!,
	command_path(Path),
	atom_concat(Path,'expand_cdb.pl ',Command).
command(crack,_Command):- !,
	error_message("crack operation not implemented").

command(project,Command):-
	current_pp_flag(bshare_option,tNSH),!,
	command_path(Path),
	atom_concat(Path,'NSHproject.pl ',Command).
command(project,Command):- !,
	command_path(Path),
	atom_concat(Path,'CSHproject.pl ',Command).

command(amgu,Command):-
	current_pp_flag(bshare_option,tNSH),!,
	command_path(Path),
	atom_concat(Path,'NSHamgu.pl ',Command).
command(amgu,Command):- !,
	command_path(Path),
	atom_concat(Path,'CSHamgu.pl ',Command).

command(compare,Command):-
	current_pp_flag(bshare_option,tNSH),!,
	command_path(Path),
	atom_concat(Path,'NSHcompare.pl ',Command).
command(compare,Command):- !,
	command_path(Path),
	atom_concat(Path,'CSHcompare.pl ',Command).

command(add,Command):-
	current_pp_flag(bshare_option,tNSH),!,
	command_path(Path),
	atom_concat(Path,'NSHadd.pl ',Command).
command(add,Command):- !,
	command_path(Path),
	atom_concat(Path,'CSHadd.pl ',Command).

command(union,Command):-
	current_pp_flag(bshare_option,tNSH),!,
	command_path(Path),
	atom_concat(Path,'NSHunion.pl ',Command).
command(union,Command):- !,
	command_path(Path),
	atom_concat(Path,'CSHunion.pl ',Command).

command(sh2nsh,Command):-
	current_pp_flag(bshare_option,tNSH),!,

	command_path(Path),
	atom_concat(Path,'SH2NSH.pl ',Command).
command(sh2nsh,Command):- !,
	command_path(Path),
	atom_concat(Path,'SH2CSH.pl ',Command).

command_path(Path):-
	current_pp_flag(bshare_option,bSH),!,
	bSH_path(Path).
command_path(Path):-
	current_pp_flag(bshare_option,tSH),!,
	tSH_path(Path).
command_path(Path):-
	current_pp_flag(bshare_option,tNSH),!,
	tNSH_path(Path).

